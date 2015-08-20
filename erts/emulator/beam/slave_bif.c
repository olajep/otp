/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2015. All Rights Reserved.
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
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_thr_progress.h"
#include "slave_bif.h"

#define HARDDEBUG 0

static const erts_aint32_t ignored_psflgs = ERTS_PSFLG_PENDING_EXIT
    | SLAVE_STATE_PSFLGS;

#ifdef HIPE
#include "hipe_native_bif.h"
#include "hipe_bif0.h"

struct {
    Eterm (*f)(Process *, Eterm[]);
} primop_table[] = {
#define X(Name, Arity) \
    [SLAVE_PRIMOP_##Name] = { Name },
    SLAVE_PROXIED_PRIMOPS_DEFINER
#undef X
};
#endif

int
erts_slave_serve_bif(struct slave *slave, struct slave_syscall_bif *arg)
{
    Process *p = slave->c_p;
    erts_aint32_t old_state, new_state;
    BeamInstr *old_i;
    Eterm (*f)(Process*, Eterm*);
    Eterm *args;
    int trapping, trapping_to_beam;
    ASSERT(p);
    ASSERT(arg->bif_no < BIF_SIZE);
#ifdef HIPE
    ASSERT(-arg->bif_no < SLAVE_PRIMOP_BOUND);
#else
    ASSERT(arg->bif_no >= 0);
#endif
    old_state = erts_smp_atomic32_read_acqb(&p->state) & ~ignored_psflgs;

#if HARDDEBUG
    if (arg->bif_no >= 0) {
	BifEntry *bif = bif_table + arg->bif_no;
	switch (bif->arity) {
	case 0: erts_printf("Proxying (for slave %d) bif %T:%T()\n", slave->no, bif->module, bif->name); break;
	case 1: erts_printf("Proxying (for slave %d) bif %T:%T(%T)\n", slave->no, bif->module, bif->name, arg->args[0]); break;
	case 2: erts_printf("Proxying (for slave %d) bif %T:%T(%T,%T)\n", slave->no, bif->module, bif->name, arg->args[0], arg->args[1]); break;
	case 3: erts_printf("Proxying (for slave %d) bif %T:%T(%T,%T,%T)\n", slave->no, bif->module, bif->name, arg->args[0], arg->args[1], arg->args[2]); break;
	}
    } else
	erts_printf("Proxying (for slave %d) primop %d\n", -arg->bif_no);
#endif

    slave_state_swapin(p, &arg->state);
    old_i = p->i;

    if (arg->result == THE_NON_VALUE && p->freason == TRAP) {
#if HARDDEBUG
	erts_printf("Resuming trapped execution (i=%#x, f=%#x) at ",
		    p->i[0], p->i[1]);
	erts_printf("%T:%T/%d\n", p->i[-3], p->i[-2], p->i[-1]);
#endif
	ASSERT(p->i[0] == (BeamInstr)em_apply_bif);
	f = (Eterm (*)(Process*, Eterm*))p->i[1];
	args = ERTS_PROC_GET_SCHDATA(p)->x_reg_array;
    } else {
#ifdef HIPE
	if (arg->bif_no < 0)
	    f = primop_table[-arg->bif_no].f;
	else
#endif
	    f = bif_table[arg->bif_no].f;
	args = arg->args;
    }
    arg->result = f(p, args);
    trapping = arg->result == THE_NON_VALUE && p->freason == TRAP;
    trapping_to_beam = trapping && *p->i != (BeamInstr)em_apply_bif;

    if (trapping_to_beam) {
	Eterm m = (Eterm)p->i[-3], f = (Eterm)p->i[-2];
	UWord a = (UWord)p->i[-1];
	Export *e = erts_export_get_or_make_stub(m,f,a);
	p->i = e->slave_addressv[erts_active_code_ix()];
    } else if (trapping) {
	/*
	 * Since we never do a roundtrip into the emulator (not only for
	 * efficency, but also because the IP would not be understood by the
	 * slave), we have to manage reduction bookkeeping here
	 */
	p->reds += CONTEXT_REDS - p->fcalls;
	p->fcalls = CONTEXT_REDS;
    }

    arg->state_flags = 0;
    new_state = erts_smp_atomic32_read_nob(&p->state) & ~ignored_psflgs;
    if (new_state == (old_state | ERTS_PSFLG_EXITING)) {
	arg->state_flags |= ERTS_PSFLG_EXITING;
    } else if (new_state != old_state) {
	erl_exit(1, "Process state changed by bif %d!\nWas: %#x, Now: %#x\n",
		 arg->bif_no, old_state, new_state);
    }
    if (!trapping && p->i != old_i) {
	erl_exit(1, "IP changed by bif %d!\nWas: %#x, Now: %#x\n",
		 arg->bif_no, old_i, p->i);
    }

    slave_state_swapout(p, &arg->state);

#if HARDDEBUG
    if (!trapping) {
	if (arg->result != THE_NON_VALUE)
	    if (arg->bif_no >= 0)
		 erts_printf("Replying with result %T\n", arg->result);
	    else erts_printf("Replying with result %#x\n", arg->result);
	else erts_printf("Replying with error %d\n", p->freason);
    } else erts_printf("Trapping %s %T:%T/%d\n",
		       trapping_to_beam ? "to beam" : "into",
		       p->i[-3], p->i[-2], p->i[-1]);
#endif
    return !trapping || trapping_to_beam;
}

void
erts_slave_serve_gc(struct slave *slave, struct slave_syscall_gc *arg)
{
    Process *p = slave->c_p;

#if HARDDEBUG
    erts_printf("Garbage collecting %T (heap size %d, %d additional roots)\n",
		p->common.id, p->hend - p->heap, arg->nobj);
#endif

    slave_state_swapin(p, &arg->state);
    arg->ret = erts_garbage_collect(p, arg->need, arg->objv, arg->nobj);
    slave_state_swapout(p, &arg->state);

#if HARDDEBUG
    erts_printf("[gc %T] ret=%d, heap size %d\n", p->common.id, arg->ret,
		p->hend - p->heap);
#endif
}
