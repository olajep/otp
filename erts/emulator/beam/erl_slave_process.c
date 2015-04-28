/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2014. All Rights Reserved.
 *
 * The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved online at http://www.erlang.org/.
 *
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 *
 * %CopyrightEnd%
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "erl_db.h"
#include "dist.h"
#include "beam_catches.h"
#include "erl_instrument.h"
#include "erl_threads.h"
#include "erl_binary.h"
#include "beam_bp.h"
#include "erl_cpu_topology.h"
#include "erl_thr_progress.h"
#include "erl_thr_queue.h"
#include "erl_async.h"
#include "dtrace-wrapper.h"
#include "erl_ptab.h"

#include "erl_slave_process.h"
#include "erl_slave_load.h"
#include "slave_export.h"

typedef struct {
    Process *proc;
    erts_aint32_t state;
} ErtsEarlyProcInit;

static void early_init_process_struct(void *varg, Eterm data)
{
    ErtsEarlyProcInit *arg = (ErtsEarlyProcInit *) varg;
    Process *proc = arg->proc;

    proc->common.id = make_internal_pid(data);
    erts_smp_atomic32_init_relb(&proc->state, arg->state);

#ifdef ERTS_SMP
    erts_proc_lock_init(proc); /* All locks locked */
#endif
}

/*
** Allocate process and find out where to place next process.
*/
static Process*
alloc_process(erts_aint32_t state)
{
    ErtsEarlyProcInit init_arg;
    Process *p;

    p = erts_alloc_fnf(ERTS_ALC_T_PROC, sizeof(Process));
    if (!p)
	return NULL;

    sys_memzero(p, sizeof(Process));
    init_arg.proc = (Process *) p;
    init_arg.state = state;

    if (!erts_ptab_new_element(&erts_proc,
			       &p->common,
			       (void *) &init_arg,
			       early_init_process_struct)) {
	erts_free(ERTS_ALC_T_PROC, p);
	return NULL;
    }

    ASSERT(internal_pid_serial(p->common.id) <= ERTS_MAX_PID_SERIAL);

    p->approx_started = erts_get_approx_time();
    p->rcount = 0;
    p->heap = NULL;

    ASSERT(p == (Process *) (erts_ptab_pix2intptr_nob(
				 &erts_proc,
				 internal_pid_index(p->common.id))));
    return p;
}

Eterm
erl_create_slave_process(Process *parent, Eterm mod, Eterm func,
			 Eterm args, ErlSpawnOpts *so)
{
    Process *p;
    Sint arity;			/* Number of arguments. */
    Uint arg_size;		/* Size of arguments. */
    Uint sz;			/* Needed words on heap. */
    Uint heap_need;		/* Size needed on heap. */
    Eterm res = THE_NON_VALUE;
    erts_aint32_t state = 0;
    struct slave *slave;
    struct slave_command_run cmd = {
	.entry = slave_demo_prog,
	.parent_id = parent->common.id,
	.mod = mod,
	.func = func,
    };

#ifdef ERTS_SMP
    erts_smp_proc_lock(parent, ERTS_PROC_LOCKS_ALL_MINOR);
#endif

    /*
     * Check for errors.
     */
    if (is_not_atom(mod) || is_not_atom(func) || ((arity = erts_list_length(args)) < 0)) {
	so->error_code = BADARG;
	goto error;
    }

    if (!(slave = erts_slave_pop_free())) {
	erts_send_error_to_logger_str(parent->group_leader,
				      "Out of free slaves\n");
	so->error_code = SYSTEM_LIMIT;
	goto error;
    }

    state |= ERTS_PSFLG_SLAVE;

    p = alloc_process(state); /* All proc locks are locked by this thread on
				 success */
    if (!p) {
	erts_send_error_to_logger_str(parent->group_leader,
				      "Too many processes\n");
	so->error_code = SYSTEM_LIMIT;
	erts_slave_push_free(slave);
	goto error;
    }

    arg_size = size_object(args);
    heap_need = arg_size;

    p->flags = erts_default_process_flags;

    if (so->flags & SPO_USE_ARGS) {
	p->min_heap_size  = so->min_heap_size;
	p->min_vheap_size = so->min_vheap_size;
	p->max_gen_gcs    = so->max_gen_gcs;
    } else {
	p->min_heap_size  = H_MIN_SIZE;
	p->min_vheap_size = BIN_VH_MIN_SIZE;
	p->max_gen_gcs    = (Uint16) erts_smp_atomic32_read_nob(&erts_max_gen_gcs);
    }
    p->schedule_count = 0;
    ASSERT(p->min_heap_size == erts_next_heap_size(p->min_heap_size, 0));

    p->initial[INITIAL_MOD] = mod;
    p->initial[INITIAL_FUN] = func;
    p->initial[INITIAL_ARI] = (Uint) arity;

    /*
     * Must initialize binary lists here before copying binaries to process.
     */
    p->off_heap.first = NULL;
    p->off_heap.overhead = 0;

    heap_need +=
	IS_CONST(parent->group_leader) ? 0 : NC_HEAP_SIZE(parent->group_leader);

    if (heap_need < p->min_heap_size) {
	sz = heap_need = p->min_heap_size;
    } else {
	sz = erts_next_heap_size(heap_need, 0);
    }

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif
    p->heap = (Eterm *) erts_alloc(ERTS_ALC_T_SLAVE_HEAP, sizeof(Eterm)*sz);
    p->old_hend = p->old_htop = p->old_heap = NULL;
    p->high_water = p->heap;
    p->gen_gcs = 0;
    p->stop = p->hend = p->heap + sz;
    p->htop = p->heap;
    p->heap_sz = sz;
    p->catches = 0;

    p->bin_vheap_sz     = p->min_vheap_size;
    p->bin_old_vheap_sz = p->min_vheap_size;
    p->bin_old_vheap    = 0;
    p->bin_vheap_mature = 0;

    p->sys_task_qs = NULL;

    /* No need to initialize p->fcalls. */

    p->current = p->initial+INITIAL_MOD;

    p->arg_reg = p->def_arg_reg;
    p->max_arg_reg = sizeof(p->def_arg_reg)/sizeof(p->def_arg_reg[0]);
    p->arg_reg[0] = mod;
    p->arg_reg[1] = func;
    BM_STOP_TIMER(system);
    BM_MESSAGE(args,p,parent);
    BM_START_TIMER(system);
    BM_SWAP_TIMER(system,copy);
    p->arg_reg[2] = copy_struct(args, arg_size, &p->htop, &p->off_heap);
    BM_MESSAGE_COPIED(arg_size);
    BM_SWAP_TIMER(copy,system);
    p->arity = 3;

    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->reds = 0;

#ifdef ERTS_SMP
    p->common.u.alive.ptimer = NULL;
#else
    sys_memset(&p->common.u.alive.tm, 0, sizeof(ErlTimer));
#endif

    p->common.u.alive.reg = NULL;
    ERTS_P_LINKS(p) = NULL;
    ERTS_P_MONITORS(p) = NULL;
    p->nodes_monitors = NULL;
    p->suspend_monitors = NULL;

    ASSERT(is_pid(parent->group_leader));

    if (parent->group_leader == ERTS_INVALID_PID)
	p->group_leader = p->common.id;
    else {
	/* Needs to be done after the heap has been set up */
	p->group_leader =
	    IS_CONST(parent->group_leader)
	    ? parent->group_leader
	    : STORE_NC(&p->htop, &p->off_heap, parent->group_leader);
    }

    erts_get_default_tracing(&ERTS_TRACE_FLAGS(p), &ERTS_TRACER_PROC(p));

    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
#ifdef ERTS_SMP
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
#endif
    p->u.bif_timers = NULL;
    p->mbuf = NULL;
    p->mbuf_sz = 0;
    p->psd = NULL;
    p->dictionary = NULL;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_clock = 0;
    SEQ_TRACE_TOKEN(p) = NIL;
#ifdef USE_VM_PROBES
    DT_UTAG(p) = NIL;
    DT_UTAG_FLAGS(p) = 0;
#endif
    p->parent = (parent->common.id == ERTS_INVALID_PID
		 ? NIL
		 : parent->common.id);

    INIT_HOLE_CHECK(p);
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

    if (IS_TRACED(parent)) {
	if (ERTS_TRACE_FLAGS(parent) & F_TRACE_SOS) {
	    ERTS_TRACE_FLAGS(p) |= (ERTS_TRACE_FLAGS(parent) & TRACEE_FLAGS);
	    ERTS_TRACER_PROC(p) = ERTS_TRACER_PROC(parent);
	}
	if (ARE_TRACE_FLAGS_ON(parent, F_TRACE_PROCS)) {
	    trace_proc_spawn(parent, p->common.id, mod, func, args);
	}
	if (ERTS_TRACE_FLAGS(parent) & F_TRACE_SOS1) {
	    /* Overrides TRACE_CHILDREN */
	    ERTS_TRACE_FLAGS(p) |= (ERTS_TRACE_FLAGS(parent) & TRACEE_FLAGS);
	    ERTS_TRACER_PROC(p) = ERTS_TRACER_PROC(parent);
	    ERTS_TRACE_FLAGS(p) &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	    ERTS_TRACE_FLAGS(parent) &= ~(F_TRACE_SOS1 | F_TRACE_SOS);
	}
    }

    /*
     * Check if this process should be initially linked to its parent.
     */

    if (so->flags & SPO_LINK) {
#ifdef DEBUG
	int ret;
#endif
	if (IS_TRACED_FL(parent, F_TRACE_PROCS)) {
	    trace_proc(parent, parent, am_link, p->common.id);
	}

#ifdef DEBUG
	ret = erts_add_link(&ERTS_P_LINKS(parent),  LINK_PID, p->common.id);
	ASSERT(ret == 0);
	ret = erts_add_link(&ERTS_P_LINKS(p), LINK_PID, parent->common.id);
	ASSERT(ret == 0);
#else
	erts_add_link(&ERTS_P_LINKS(parent), LINK_PID, p->common.id);
	erts_add_link(&ERTS_P_LINKS(p), LINK_PID, parent->common.id);
#endif

	if (IS_TRACED(parent)) {
	    if (ERTS_TRACE_FLAGS(parent) & (F_TRACE_SOL|F_TRACE_SOL1)) {
		ERTS_TRACE_FLAGS(p) |= (ERTS_TRACE_FLAGS(parent)&TRACEE_FLAGS);
		ERTS_TRACER_PROC(p) = ERTS_TRACER_PROC(parent); /*maybe steal*/

		if (ERTS_TRACE_FLAGS(parent) & F_TRACE_SOL1) {/*maybe override*/
		    ERTS_TRACE_FLAGS(p) &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		    ERTS_TRACE_FLAGS(parent) &= ~(F_TRACE_SOL1 | F_TRACE_SOL);
		}
	    }
	}
    }

    /*
     * Test whether this process should be initially monitored by its parent.
     */
    if (so->flags & SPO_MONITOR) {
	Eterm mref;

	mref = erts_make_ref(parent);
	erts_add_monitor(&ERTS_P_MONITORS(parent), MON_ORIGIN, mref, p->common.id, NIL);
	erts_add_monitor(&ERTS_P_MONITORS(p), MON_TARGET, mref, parent->common.id, NIL);
	so->mref = mref;
    }

#ifdef ERTS_SMP
    p->scheduler_data = NULL;
    p->suspendee = NIL;
    p->pending_suspenders = NULL;
    p->pending_exit.reason = THE_NON_VALUE;
    p->pending_exit.bp = NULL;
#endif

#if !defined(NO_FPE_SIGNALS) || defined(HIPE)
    p->fp_exception = 0;
#endif

    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
    res = p->common.id;

    {
	Export *entry = slave_active_export_entry(mod, func, arity);
	if (entry) cmd.entry = entry->addressv[0];
	else erts_printf("No export entry, using default\n");
    }

    cmd.id = p->common.id;
    cmd.heap = p->heap;
    cmd.htop = p->htop;
    cmd.stop = p->stop;
    cmd.args = p->arg_reg[2];
    slave->c_p = p;
    erts_slave_send_command(slave, SLAVE_COMMAND_RUN, &cmd, sizeof(cmd));

    VERBOSE(DEBUG_PROCESSES, ("Created a new process: %T\n",p->common.id));

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(process_spawn)) {
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);

        dtrace_fun_decode(p, mod, func, arity, process_name, mfa);
        DTRACE2(process_spawn, process_name, mfa);
    }
#endif

 error:
    erts_smp_proc_unlock(parent, ERTS_PROC_LOCKS_ALL_MINOR);
    return res;
}
