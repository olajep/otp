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

#define ERL_PROCESS_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_smp.h"
#include "global.h"
#include "erl_process_lock.h"
#include "epiphany.h"
#include "slave_command.h"


#define ERTS_DELAYED_WAKEUP_INFINITY (~(Uint64) 0)
#define ERTS_DELAYED_WAKEUP_REDUCTIONS ((Uint64) CONTEXT_REDS/2)

#define ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED (2000*CONTEXT_REDS)
#define ERTS_RUNQ_CALL_CHECK_BALANCE_REDS \
  (ERTS_RUNQ_CHECK_BALANCE_REDS_PER_SCHED/2)

#define ERTS_PROC_MIN_CONTEXT_SWITCH_REDS_COST (CONTEXT_REDS/10)

#ifndef ERTS_SCHED_MIN_SPIN
#define ERTS_SCHED_SPIN_UNTIL_YIELD 100
#else
#define ERTS_SCHED_SPIN_UNTIL_YIELD 1
#endif

#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_VERY_LONG 40
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_VERY_LONG 1000
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_LONG 20
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_LONG 1000
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_MEDIUM 10
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_MEDIUM 1000
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_SHORT 10
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_SHORT 0
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_VERY_SHORT 5
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_VERY_SHORT 0
#define ERTS_SCHED_SYS_SLEEP_SPINCOUNT_NONE 0
#define ERTS_SCHED_AUX_WORK_SLEEP_SPINCOUNT_FACT_NONE 0

#define ERTS_SCHED_TSE_SLEEP_SPINCOUNT_FACT 1000
#define ERTS_SCHED_SUSPEND_SLEEP_SPINCOUNT 0

#if 0 || defined(DEBUG)
#define ERTS_FAKE_SCHED_BIND_PRINT_SORTED_CPU_DATA
#endif

#if defined(DEBUG) && 0
#define HARDDEBUG
#else
#undef HARDDEBUG
#endif

#ifdef HARDDEBUG
#define HARDDEBUG_RUNQS
#endif

#ifdef HIPE
#include "hipe_mode_switch.h"	/* for hipe_init_process() */
#include "hipe_signal.h"	/* for hipe_thread_signal_init() */
#endif

#ifdef ERTS_ENABLE_LOCK_COUNT
#include "erl_lock_count.h"
#endif

#define MAX_BIT       (1 << PRIORITY_MAX)
#define HIGH_BIT      (1 << PRIORITY_HIGH)
#define NORMAL_BIT    (1 << PRIORITY_NORMAL)
#define LOW_BIT       (1 << PRIORITY_LOW)
#define PORT_BIT      (1 << ERTS_PORT_PRIO_LEVEL)

#define ERTS_EMPTY_RUNQ(RQ)					\
    ((ERTS_RUNQ_FLGS_GET_NOB((RQ)) & ERTS_RUNQ_FLGS_QMASK) == 0	\
     && (RQ)->misc.start == NULL)

#undef RUNQ_READ_RQ
#undef RUNQ_SET_RQ
#define RUNQ_READ_RQ(X) ((ErtsRunQueue *) erts_smp_atomic_read_nob((X)))
#define RUNQ_INIT_RQ(X, RQ) erts_smp_atomic_init_nob((X), (erts_aint_t) (RQ))
#define RUNQ_SET_RQ(X, RQ) erts_smp_atomic_set_nob((X), (erts_aint_t) (RQ))

#ifdef DEBUG
#  if defined(ARCH_64) && !HALFWORD_HEAP
#    define ERTS_DBG_SET_INVALID_RUNQP(RQP, N) \
    (RUNQ_SET_RQ((RQP), (0xdeadbeefdead0003LL | ((N) << 4)))
#  define ERTS_DBG_VERIFY_VALID_RUNQP(RQP) \
do { \
    ASSERT((RQP) != NULL); \
    ASSERT(((((Uint) (RQP)) & ((Uint) 0x3))) == ((Uint) 0)); \
    ASSERT((((Uint) (RQP)) & ~((Uint) 0xffff)) != ((Uint) 0xdeadbeefdead0000LL));\
} while (0)
#  else
#    define ERTS_DBG_SET_INVALID_RUNQP(RQP, N) \
    (RUNQ_SET_RQ((RQP), (0xdead0003 | ((N) << 4))))
#  define ERTS_DBG_VERIFY_VALID_RUNQP(RQP) \
do { \
    ASSERT((RQP) != NULL); \
    ASSERT(((((UWord) (RQP)) & ((UWord) 1))) == ((UWord) 0)); \
    ASSERT((((UWord) (RQP)) & ~((UWord) 0xffff)) != ((UWord) 0xdead0000)); \
} while (0)
#  endif
#else
#  define ERTS_DBG_SET_INVALID_RUNQP(RQP, N)
#  define ERTS_DBG_VERIFY_VALID_RUNQP(RQP)
#endif

#define ERTS_EMPTY_RUNQ_PORTS(RQ) \
    (RUNQ_READ_LEN(&(RQ)->ports.info.len) == 0 && (RQ)->misc.start == NULL)

const Process erts_invalid_process = {{ERTS_INVALID_PID}};

extern BeamInstr beam_apply[];
extern BeamInstr beam_exit[];
extern BeamInstr beam_continue_exit[];

#ifdef __OSE__
/* Eager check I/O not supported on OSE yet. */
int erts_eager_check_io = 0;
#else
int erts_eager_check_io = 0;
#endif
int erts_sched_compact_load;
int erts_sched_balance_util = 0;
Uint erts_no_schedulers;
#ifdef ERTS_DIRTY_SCHEDULERS
Uint erts_no_dirty_cpu_schedulers;
Uint erts_no_dirty_io_schedulers;
#endif

#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_VERY_LAZY		(4*1024*1024)
#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_LAZY			(512*1024)
#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_MEDIUM			(64*1024)
#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_EAGER			(16*1024)
#define ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_VERY_EAGER		(1024)

ErtsPTab erts_proc erts_align_attribute(ERTS_CACHE_LINE_SIZE);

int erts_sched_thread_suggested_stack_size = -1;

#ifdef ERTS_ENABLE_LOCK_CHECK
ErtsLcPSDLocks erts_psd_required_locks[ERTS_PSD_SIZE];
#endif

#ifdef ERTS_SMP
int erts_disable_proc_not_running_opt;

static ErtsAuxWorkData *aux_thread_aux_work_data;

#define ERTS_SCHDLR_SSPND_CHNG_WAITER		(((erts_aint32_t) 1) << 0)
#define ERTS_SCHDLR_SSPND_CHNG_MSB		(((erts_aint32_t) 1) << 1)
#define ERTS_SCHDLR_SSPND_CHNG_ONLN		(((erts_aint32_t) 1) << 2)

#ifndef DEBUG

#define ERTS_SCHDLR_SSPND_CHNG_SET(VAL, OLD_VAL) \
  erts_smp_atomic32_set_nob(&schdlr_sspnd.changing, (VAL))

#ifdef ERTS_DIRTY_SCHEDULERS
#define ERTS_SCHDLR_SSPND_DIRTY_CPU_CHNG_SET(VAL, OLD_VAL) \
  erts_smp_atomic32_set_nob(&schdlr_sspnd.dirty_cpu_changing, (VAL))
#define ERTS_SCHDLR_SSPND_DIRTY_IO_CHNG_SET(VAL, OLD_VAL) \
  erts_smp_atomic32_set_nob(&schdlr_sspnd.dirty_io_changing, (VAL))
#endif

#else

#define ERTS_SCHDLR_SSPND_CHNG_SET(VAL, OLD_VAL)			\
do {									\
    erts_aint32_t old_val__;						\
    old_val__ = erts_smp_atomic32_xchg_nob(&schdlr_sspnd.changing,     	\
					   (VAL));			\
    ASSERT(old_val__ == (OLD_VAL));					\
} while (0)

#ifdef ERTS_DIRTY_SCHEDULERS
#define ERTS_SCHDLR_SSPND_DIRTY_CPU_CHNG_SET(VAL, OLD_VAL)		\
do {									\
    erts_aint32_t old_val__;						\
    old_val__ = erts_smp_atomic32_xchg_nob(&schdlr_sspnd.dirty_cpu_changing, \
					   (VAL));			\
    ASSERT(old_val__ == (OLD_VAL));					\
} while (0)
#define ERTS_SCHDLR_SSPND_DIRTY_IO_CHNG_SET(VAL, OLD_VAL)		\
do {									\
    erts_aint32_t old_val__;						\
    old_val__ = erts_smp_atomic32_xchg_nob(&schdlr_sspnd.dirty_io_changing, \
					   (VAL));			\
    ASSERT(old_val__ == (OLD_VAL));					\
} while (0)
#endif

#endif


static struct {
    int online;
    int curr_online;
    int wait_curr_online;
#ifdef ERTS_DIRTY_SCHEDULERS
    int dirty_cpu_online;
    int dirty_cpu_curr_online;
    int dirty_cpu_wait_curr_online;
    int dirty_io_online;
    int dirty_io_curr_online;
    int dirty_io_wait_curr_online;
#endif
    erts_smp_atomic32_t changing;
    erts_smp_atomic32_t active;
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_smp_atomic32_t dirty_cpu_changing;
    erts_smp_atomic32_t dirty_cpu_active;
    erts_smp_atomic32_t dirty_io_changing;
    erts_smp_atomic32_t dirty_io_active;
#endif
    struct {
	int ongoing;
	long wait_active;
#ifdef ERTS_DIRTY_SCHEDULERS
	long dirty_cpu_wait_active;
	long dirty_io_wait_active;
#endif
	ErtsProcList *procs;
    } msb; /* Multi Scheduling Block */
} schdlr_sspnd;

static struct {
    int last_active_runqs;
    int forced_check_balance;
    erts_smp_atomic32_t checking_balance;
    int halftime;
    int full_reds_history_index;
    struct {
	int active_runqs;
	int reds;
	erts_aint32_t max_len;
    } prev_rise;
    Uint n;
} balance_info;

#define ERTS_BLNCE_SAVE_RISE(ACTIVE, MAX_LEN, REDS)	\
do {							\
    balance_info.prev_rise.active_runqs = (ACTIVE);	\
    balance_info.prev_rise.max_len = (MAX_LEN);		\
    balance_info.prev_rise.reds = (REDS);		\
} while (0)

#endif

erts_sched_stat_t erts_sched_stat;

#ifndef ERTS_SMP
ErtsSchedulerData *erts_scheduler_data;
#endif

ErtsAlignedRunQueue *erts_aligned_run_queues;
Uint erts_no_run_queues;

ErtsAlignedSchedulerData *erts_aligned_scheduler_data;
#ifdef ERTS_DIRTY_SCHEDULERS
ErtsAlignedSchedulerData *erts_aligned_dirty_cpu_scheduler_data;
ErtsAlignedSchedulerData *erts_aligned_dirty_io_scheduler_data;
#endif

typedef union {
    ErtsSchedulerSleepInfo ssi;
    char align[ERTS_ALC_CACHE_LINE_ALIGN_SIZE(sizeof(ErtsSchedulerSleepInfo))];
} ErtsAlignedSchedulerSleepInfo;

Uint erts_default_process_flags;
Eterm erts_system_monitor;
Eterm erts_system_monitor_long_gc;
Uint erts_system_monitor_long_schedule;
Eterm erts_system_monitor_large_heap;
struct erts_system_monitor_flags_t erts_system_monitor_flags;

/* system performance monitor */
Eterm erts_system_profile;
struct erts_system_profile_flags_t erts_system_profile_flags;

#if ERTS_MAX_PROCESSES > 0x7fffffff
#error "Need to store process_count in another type"
#endif

typedef enum {
    ERTS_PSTT_GC,	/* Garbage Collect */
    ERTS_PSTT_CPC	/* Check Process Code */
} ErtsProcSysTaskType;

#define ERTS_MAX_PROC_SYS_TASK_ARGS 2

struct ErtsProcSysTask_ {
    ErtsProcSysTask *next;
    ErtsProcSysTask *prev;
    ErtsProcSysTaskType type;
    Eterm requester;
    Eterm reply_tag;
    Eterm req_id;
    Uint req_id_sz;
    Eterm arg[ERTS_MAX_PROC_SYS_TASK_ARGS];
    ErlOffHeap off_heap;
    Eterm heap[1];
};

static ERTS_INLINE void
change_proc_schedule_state(Process *p,
			   erts_aint32_t clear_state_flags,
			   erts_aint32_t set_state_flags,
			   erts_aint32_t *statep)
{
    /*
     * NOTE: ERTS_PSFLG_RUNNING, ERTS_PSFLG_RUNNING_SYS and
     *       ERTS_PSFLG_ACTIVE_SYS are not allowed to be
     *       altered by this function!
     */
    erts_aint32_t a = *statep, n;

    ERTS_SMP_LC_ASSERT(locks == erts_proc_lc_my_proc_locks(p));

    ASSERT(!(a & ERTS_PSFLG_PROXY));
    ASSERT((clear_state_flags & (ERTS_PSFLG_RUNNING
				 | ERTS_PSFLG_RUNNING_SYS
				 | ERTS_PSFLG_ACTIVE_SYS)) == 0);
    ASSERT((set_state_flags & (ERTS_PSFLG_RUNNING
			       | ERTS_PSFLG_RUNNING_SYS
			       | ERTS_PSFLG_ACTIVE_SYS)) == 0);

    while (1) {
	erts_aint32_t e;
	n = e = a;

	if (clear_state_flags)
	    n &= ~clear_state_flags;

	if (set_state_flags)
	    n |= set_state_flags;

	a = erts_smp_atomic32_cmpxchg_mb(&p->state, n, e);
	if (a == e)
	    break;
    }

    *statep = a;
}

static ERTS_INLINE void
schedule_process(Process *p, erts_aint32_t in_state)
{
    erts_aint32_t state = in_state;
    change_proc_schedule_state(p,
			       0,
			       ERTS_PSFLG_ACTIVE,
			       &state);
}

void
erts_schedule_process(Process *p, erts_aint32_t state, ErtsProcLocks locks)
{
    schedule_process(p, state);
}

static void *sched_thread_func(void *vesdp) __noreturn;

void enter_scheduler(int number) {
    ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(number);
    ASSERT(number == epiphany_coreno());
    ASSERT(number == esdp->no - 1);

    sched_thread_func((void*)esdp);
}

static void *
sched_thread_func(void *vesdp)
{
    ErtsSchedulerData *esdp = vesdp;
    Uint no = esdp->no;
#ifdef ERTS_SMP
    // ESTUB: thread_progress

    erts_alloc_register_scheduler(vesdp);
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
    {
	char buf[31];
	erts_snprintf(&buf[0], 31, "scheduler %beu", no);
	erts_lc_set_thread_name(&buf[0]);
    }
#endif
#ifdef ERTS_SMP
#if HAVE_ERTS_MSEG
    erts_mseg_late_init();
#endif
#endif

#ifdef HIPE
    hipe_thread_signal_init();
#endif
    erts_thread_init_float();

    // erts_smp_mtx_lock(&schdlr_sspnd.mtx);

    /* ASSERT(erts_smp_atomic32_read_nob(&schdlr_sspnd.changing) */
    /*        & ERTS_SCHDLR_SSPND_CHNG_ONLN); */

/*     if (--schdlr_sspnd.curr_online == schdlr_sspnd.wait_curr_online) { */
/* 	erts_smp_atomic32_read_band_nob(&schdlr_sspnd.changing, */
/* 					~ERTS_SCHDLR_SSPND_CHNG_ONLN); */
/* 	if (no != 1) */
/* #ifdef ERTS_DIRTY_SCHEDULERS */
/* 	    erts_smp_cnd_broadcast(&schdlr_sspnd.cnd); */
/* #else */
/* 	    erts_smp_cnd_signal(&schdlr_sspnd.cnd); */
/* #endif */
/*     } */

/*     if (no == 1) { */
/* 	while (schdlr_sspnd.curr_online != schdlr_sspnd.wait_curr_online) */
/* 	    erts_smp_cnd_wait(&schdlr_sspnd.cnd, &schdlr_sspnd.mtx); */
/* 	ERTS_SCHDLR_SSPND_CHNG_SET(0, ERTS_SCHDLR_SSPND_CHNG_WAITER); */
/*     } */
/*     erts_smp_mtx_unlock(&schdlr_sspnd.mtx); */

#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    esdp->verify_unused_temp_alloc = NULL;
    /* esdp->verify_unused_temp_alloc */
    /*     = erts_alloc_get_verify_unused_temp_alloc( */
    /*         &esdp->verify_unused_temp_alloc_data); */
    /* ERTS_VERIFY_UNUSED_TEMP_ALLOC(NULL); */
#endif

    /* ETODO: Should be called during init instead. */
    erts_master_setup();

    process_main();
    /* No schedulers should *ever* terminate */
    erl_exit(ERTS_ABORT_EXIT,
	     "Scheduler thread number %beu terminated\n",
	     no);
}


/*
** Allocate process and find out where to place next process.
*/
static Process*
alloc_process(Eterm id, erts_aint32_t state)
{
    Process *p;

    p = erts_alloc_fnf(ERTS_ALC_T_PROC, sizeof(Process));
    if (!p)
	return NULL;

    memzero(p, sizeof(Process));

    ASSERT(((char *) p) == ((char *) &p->common));

    p->common.id = id;
    erts_smp_atomic32_init_relb(&p->state, state);

#ifdef ERTS_SMP
    erts_proc_lock_init(p); /* All locks locked */
    erts_smp_atomic32_init_nob(&p->common.refc, 1);
#endif

    ASSERT(internal_pid_serial(p->common.id) <= ERTS_MAX_PID_SERIAL);
    
    p->rcount = 0;
    p->heap = NULL;

    /* ASSERT(p == (Process *) (erts_ptab_pix2intptr_nob( */
    /* 				 &erts_proc, */
    /* 				 internal_pid_index(p->common.id)))); */

    return p;
}

static Process *
erl_create_process_ptr(const struct slave_syscall_ready *cmd, ErlSpawnOpts *so)
{
    Process *p;
    Sint arity;			/* Number of arguments. */
    Process *res = NULL;
    erts_aint32_t state = 0;
    erts_aint32_t prio = (erts_aint32_t) PRIORITY_NORMAL;

#ifdef ERTS_SMP
    //erts_smp_proc_lock(parent, ERTS_PROC_LOCKS_ALL_MINOR);
#endif

    /*
     * Check for errors.
     */

    if (is_not_atom(cmd->mod) || is_not_atom(cmd->func) || ((arity = erts_list_length(cmd->args)) < 0)) {
	so->error_code = BADARG;
	goto error;
    }

    state |= (((prio & ERTS_PSFLGS_PRIO_MASK) << ERTS_PSFLGS_ACT_PRIO_OFFSET)
	      | ((prio & ERTS_PSFLGS_PRIO_MASK) << ERTS_PSFLGS_USR_PRIO_OFFSET));

    p = alloc_process(cmd->id, state); /* All proc locks are locked by this thread
					* on success */
    if (!p) {
	/* erts_send_error_to_logger_str(parent->group_leader, */
	/* 			      "Too many processes\n"); */
	so->error_code = SYSTEM_LIMIT;
	goto error;
    }

#ifdef BM_COUNTERS
    processes_busy++;
#endif
    BM_COUNT(processes_spawned);

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
    // ASSERT(p->min_heap_size == erts_next_heap_size(p->min_heap_size, 0));
    
    p->initial[INITIAL_MOD] = cmd->mod;
    p->initial[INITIAL_FUN] = cmd->func;
    p->initial[INITIAL_ARI] = (Uint) arity;

    /* slave_state_swapin locks the main lock, so we unlock it to prevent a
     * deadlock (no other thread can reach p, because there's no process table,
     * so we're safe to do this). */
    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
    slave_state_swapin(p, &cmd->state);
    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif
    p->old_hend = p->old_htop = p->old_heap = NULL;
    p->high_water = p->heap;
    p->gen_gcs = 0;
    p->catches = 0;

    p->bin_vheap_sz     = p->min_vheap_size;
    p->bin_old_vheap_sz = p->min_vheap_size;
    p->bin_old_vheap    = 0;
    p->bin_vheap_mature = 0;

    p->sys_task_qs = NULL;

    p->current = p->initial+INITIAL_MOD;

    p->i = (BeamInstr *) beam_apply;
    p->cp = (BeamInstr *) beam_apply+1;

    p->arg_reg = p->def_arg_reg;
    p->max_arg_reg = sizeof(p->def_arg_reg)/sizeof(p->def_arg_reg[0]);
    p->arg_reg[0] = cmd->mod;
    p->arg_reg[1] = cmd->func;
    BM_STOP_TIMER(system);
    // BM_MESSAGE(args,p,parent);
    BM_START_TIMER(system);
    p->arg_reg[2] = cmd->args;
    p->arity = 3;

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

#ifdef ERTS_SMP
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
#endif
    p->u.bif_timers = NULL;
    p->psd = NULL;
    p->dictionary = NULL;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_clock = 0;
    SEQ_TRACE_TOKEN(p) = NIL;
#ifdef USE_VM_PROBES
    DT_UTAG(p) = NIL;
    DT_UTAG_FLAGS(p) = 0;
#endif
    p->parent = cmd->parent_id;
    p->group_leader = cmd->group_leader;

    INIT_HOLE_CHECK(p);
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

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

    res = p;

    /*
     * Give the process the expected state
     */
    schedule_process(p, state);

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

    // erts_smp_proc_unlock(parent, ERTS_PROC_LOCKS_ALL_MINOR);

   return res;
}

/*
 * Initiates a pseudo process that can be used
 * for arithmetic BIFs.
 */

void erts_init_empty_process(Process *p)
{
    p->htop = NULL;
    p->stop = NULL;
    p->hend = NULL;
    p->heap = NULL;
    p->gen_gcs = 0;
    p->max_gen_gcs = 0;
    p->min_heap_size = 0;
    p->min_vheap_size = 0;
    p->rcount = 0;
    p->common.id = ERTS_INVALID_PID;
    p->reds = 0;
    ERTS_TRACER_PROC(p) = NIL;
    ERTS_TRACE_FLAGS(p) = F_INITIAL_TRACE_FLAGS;
    p->group_leader = ERTS_INVALID_PID;
    p->flags = 0;
    p->fvalue = NIL;
    p->freason = EXC_NULL;
    p->ftrace = NIL;
    p->fcalls = 0;

    p->bin_vheap_sz = BIN_VH_MIN_SIZE;
    p->bin_old_vheap_sz = BIN_VH_MIN_SIZE;
    p->bin_old_vheap = 0;
    p->sys_task_qs = NULL;
    p->bin_vheap_mature = 0;
#ifdef ERTS_SMP
    p->common.u.alive.ptimer = NULL;
#else
    memset(&(p->common.u.alive.tm), 0, sizeof(ErlTimer));
#endif
    p->next = NULL;
    p->off_heap.first = NULL;
    p->off_heap.overhead = 0;
    p->common.u.alive.reg = NULL;
    p->heap_sz = 0;
    p->high_water = NULL;
    p->old_hend = NULL;
    p->old_htop = NULL;
    p->old_heap = NULL;
    p->mbuf = NULL;
    p->mbuf_sz = 0;
    p->psd = NULL;
    ERTS_P_MONITORS(p) = NULL;
    ERTS_P_LINKS(p) = NULL;         /* List of links */
    p->nodes_monitors = NULL;
    p->suspend_monitors = NULL;
    p->msg.first = NULL;
    p->msg.last = &p->msg.first;
    p->msg.save = &p->msg.first;
    p->msg.len = 0;
    p->u.bif_timers = NULL;
    p->dictionary = NULL;
    p->seq_trace_clock = 0;
    p->seq_trace_lastcnt = 0;
    p->seq_trace_token = NIL;
    p->initial[0] = 0;
    p->initial[1] = 0;
    p->initial[2] = 0;
    p->catches = 0;
    p->cp = NULL;
    p->i = NULL;
    p->current = NULL;

    /*
     * Saved x registers.
     */
    p->arity = 0;
    p->arg_reg = NULL;
    p->max_arg_reg = 0;
    p->def_arg_reg[0] = 0;
    p->def_arg_reg[1] = 0;
    p->def_arg_reg[2] = 0;
    p->def_arg_reg[3] = 0;
    p->def_arg_reg[4] = 0;
    p->def_arg_reg[5] = 0;

    p->parent = NIL;
    p->approx_started = 0;
    p->common.u.alive.started_interval = 0;

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif

    INIT_HOLE_CHECK(p);
#ifdef DEBUG
    p->last_old_htop = NULL;
#endif

    erts_smp_atomic32_init_nob(&p->state, (erts_aint32_t) PRIORITY_NORMAL);

#ifdef ERTS_SMP
    p->scheduler_data = NULL;
    p->msg_inq.first = NULL;
    p->msg_inq.last = &p->msg_inq.first;
    p->msg_inq.len = 0;
    p->suspendee = NIL;
    p->pending_suspenders = NULL;
    p->pending_exit.reason = THE_NON_VALUE;
    p->pending_exit.bp = NULL;
    erts_proc_lock_init(p);
    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
    RUNQ_INIT_RQ(&p->run_queue, ERTS_RUNQ_IX(0));
#endif

#if !defined(NO_FPE_SIGNALS) || defined(HIPE)
    p->fp_exception = 0;
#endif

}

#ifdef DEBUG

void
erts_debug_verify_clean_empty_process(Process* p)
{
    /* Things that erts_cleanup_empty_process() will *not* cleanup... */
    ASSERT(p->htop == NULL);
    ASSERT(p->stop == NULL);
    ASSERT(p->hend == NULL);
    ASSERT(p->heap == NULL);
    ASSERT(p->common.id == ERTS_INVALID_PID);
    ASSERT(ERTS_TRACER_PROC(p) == NIL);
    ASSERT(ERTS_TRACE_FLAGS(p) == F_INITIAL_TRACE_FLAGS);
    ASSERT(p->group_leader == ERTS_INVALID_PID);
    ASSERT(p->next == NULL);
    ASSERT(p->common.u.alive.reg == NULL);
    ASSERT(p->heap_sz == 0);
    ASSERT(p->high_water == NULL);
    ASSERT(p->old_hend == NULL);
    ASSERT(p->old_htop == NULL);
    ASSERT(p->old_heap == NULL);

    ASSERT(ERTS_P_MONITORS(p) == NULL);
    ASSERT(ERTS_P_LINKS(p) == NULL);
    ASSERT(p->nodes_monitors == NULL);
    ASSERT(p->suspend_monitors == NULL);
    ASSERT(p->msg.first == NULL);
    ASSERT(p->msg.len == 0);
    ASSERT(p->u.bif_timers == NULL);
    ASSERT(p->dictionary == NULL);
    ASSERT(p->catches == 0);
    ASSERT(p->cp == NULL);
    ASSERT(p->i == NULL);
    ASSERT(p->current == NULL);

    ASSERT(p->parent == NIL);

#ifdef ERTS_SMP
    ASSERT(p->msg_inq.first == NULL);
    ASSERT(p->msg_inq.len == 0);
    ASSERT(p->suspendee == NIL);
    ASSERT(p->pending_suspenders == NULL);
    ASSERT(p->pending_exit.reason == THE_NON_VALUE);
    ASSERT(p->pending_exit.bp == NULL);
#endif

    /* Thing that erts_cleanup_empty_process() cleans up */

    ASSERT(p->off_heap.first == NULL);
    ASSERT(p->off_heap.overhead == 0);

    ASSERT(p->mbuf == NULL);
}

#endif

void
erts_free_proc(Process *p)
{
#ifdef ERTS_SMP
    erts_proc_lock_fin(p);
#endif
    erts_free(ERTS_ALC_T_PROC, (void *) p);
}

void
erts_cleanup_empty_process(Process* p)
{
    /* We only check fields that are known to be used... */

    /* off_heap is cleaned up in master */
    p->off_heap.first = NULL;
    p->off_heap.overhead = 0;

    if (p->mbuf != NULL) {
	free_message_buffer(p->mbuf);
	p->mbuf = NULL;
    }
#ifdef ERTS_SMP
    erts_proc_lock_fin(p);
#endif
#ifdef DEBUG
    erts_debug_verify_clean_empty_process(p);
#endif
}

/*
 * p must be the currently executing process.
 */
static void
delete_process(Process* p)
{
    VERBOSE(DEBUG_PROCESSES, ("Removing process: %T\n",p->common.id));

    /* Cleanup psd */

    if (p->psd)
	erts_free(ERTS_ALC_T_PSD, p->psd);

    /* off_heap is cleaned up by the master */

    if (p->arg_reg != p->def_arg_reg) {
	erts_free(ERTS_ALC_T_ARG_REG, p->arg_reg);
    }

    /* mbuf is cleaned up later, since it might contain reason */

    erts_erase_dicts(p);

    /* pending messages are freed by the master */

    ASSERT(!p->nodes_monitors);
    ASSERT(!p->suspend_monitors);
}

static void
init_scheduler_data(ErtsSchedulerData* esdp, int num,
		    ErtsSchedulerSleepInfo* ssi,
		    ErtsRunQueue* runq,
		    char** daww_ptr, size_t daww_sz)
{
#ifdef ERTS_SMP
    erts_bits_init_state(&esdp->erl_bits_state);
    esdp->match_pseudo_process = NULL;
    esdp->free_process = NULL;
#endif
    esdp->x_reg_array =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_BEAM_REGISTER,
					   ERTS_X_REGS_ALLOCATED *
					   sizeof(Eterm));
    esdp->f_reg_array =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_BEAM_REGISTER,
					   MAX_REG * sizeof(FloatDef));
#if !HEAP_ON_C_STACK
    esdp->num_tmp_heap_used = 0;
#endif
#ifdef ERTS_DIRTY_SCHEDULERS
    if (ERTS_RUNQ_IX_IS_DIRTY(runq->ix)) {
	esdp->no = 0;
	ERTS_DIRTY_SCHEDULER_NO(esdp) = (Uint) num;
    }
    else {
	esdp->no = (Uint) num;
	ERTS_DIRTY_SCHEDULER_NO(esdp) = 0;
    }
#else
    esdp->no = (Uint) num;
#endif
    esdp->ssi = ssi;
    esdp->current_process = NULL;
    esdp->current_port = NULL;

    esdp->virtual_reds = 0;
    esdp->cpu_id = -1;

    // erts_init_atom_cache_map(&esdp->atom_cache_map); // ESTUB

    esdp->run_queue = runq;
    esdp->run_queue->scheduler = esdp;

    esdp->reductions = 0;

    esdp->timer_reference = 0;

    erts_port_task_handle_init(&esdp->nosuspend_port_task_handle);
}

void
erts_init_scheduling(int no_schedulers, int no_schedulers_online)
{
    int ix, n;
#if ERTS_HAVE_SCHED_UTIL_BALANCING_SUPPORT
    if (erts_sched_balance_util)
	erts_sched_compact_load = 0;
#endif

    ASSERT(no_schedulers_online <= no_schedulers);
    ASSERT(no_schedulers_online >= 1);
    ASSERT(no_schedulers >= 1);

    n = no_schedulers;
    erts_no_run_queues = n;
    erts_no_schedulers = n;

    /* Create and initialize scheduler specific data */
    erts_aligned_scheduler_data =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   n*sizeof(ErtsAlignedSchedulerData));					   

    for (ix = 0; ix < n; ix++) {
	ErtsSchedulerData *esdp = ERTS_SCHEDULER_IX(ix);
	init_scheduler_data(esdp, ix+1, NULL, ERTS_RUNQ_IX(ix), NULL, 0);
    }

#ifdef ERTS_SMP

    aux_thread_aux_work_data =
	erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_DATA,
					   sizeof(ErtsAuxWorkData));

    erts_smp_atomic32_init_nob(&schdlr_sspnd.changing, 0);
    schdlr_sspnd.online = no_schedulers_online;
    schdlr_sspnd.curr_online = no_schedulers;
    schdlr_sspnd.msb.ongoing = 0;
    erts_smp_atomic32_init_nob(&schdlr_sspnd.active, no_schedulers);
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_smp_atomic32_init_nob(&schdlr_sspnd.dirty_cpu_changing, 0);
    schdlr_sspnd.dirty_cpu_online = no_dirty_cpu_schedulers_online;
    schdlr_sspnd.dirty_cpu_curr_online = no_dirty_cpu_schedulers;
    erts_smp_atomic32_init_nob(&schdlr_sspnd.dirty_cpu_active, no_dirty_cpu_schedulers);
    erts_smp_atomic32_init_nob(&schdlr_sspnd.dirty_io_changing, 0);
    schdlr_sspnd.dirty_io_online = no_dirty_io_schedulers;
    schdlr_sspnd.dirty_io_curr_online = no_dirty_io_schedulers;
    erts_smp_atomic32_init_nob(&schdlr_sspnd.dirty_io_active, no_dirty_io_schedulers);
#endif
    schdlr_sspnd.msb.procs = NULL;
    balance_info.last_active_runqs = no_schedulers;
    balance_info.forced_check_balance = 0;
    balance_info.halftime = 1;
    balance_info.full_reds_history_index = 0;
    erts_smp_atomic32_init_nob(&balance_info.checking_balance, 0);
    balance_info.prev_rise.active_runqs = 0;
    balance_info.prev_rise.max_len = 0;
    balance_info.prev_rise.reds = 0;
    balance_info.n = 0;

    schdlr_sspnd.wait_curr_online = no_schedulers_online;
    schdlr_sspnd.curr_online *= 2; /* Boot strapping... */
    ERTS_SCHDLR_SSPND_CHNG_SET((ERTS_SCHDLR_SSPND_CHNG_ONLN
				| ERTS_SCHDLR_SSPND_CHNG_WAITER), 0);
#ifdef ERTS_DIRTY_SCHEDULERS
    schdlr_sspnd.dirty_cpu_wait_curr_online = no_dirty_cpu_schedulers_online;
    schdlr_sspnd.dirty_cpu_curr_online *= 2;
    ERTS_SCHDLR_SSPND_DIRTY_CPU_CHNG_SET((ERTS_SCHDLR_SSPND_CHNG_ONLN
					  | ERTS_SCHDLR_SSPND_CHNG_WAITER), 0);
    for (ix = no_dirty_cpu_schedulers_online; ix < no_dirty_cpu_schedulers; ix++) {
	ErtsSchedulerData* esdp = ERTS_DIRTY_CPU_SCHEDULER_IX(ix);
	erts_smp_atomic32_read_bor_nob(&esdp->ssi->flags, ERTS_SSI_FLG_SUSPENDED);
    }

    schdlr_sspnd.dirty_io_wait_curr_online = no_dirty_io_schedulers;
    schdlr_sspnd.dirty_io_curr_online *= 2;
    ERTS_SCHDLR_SSPND_DIRTY_IO_CHNG_SET((ERTS_SCHDLR_SSPND_CHNG_ONLN
					 | ERTS_SCHDLR_SSPND_CHNG_WAITER), 0);
#endif

#else /* !ERTS_SMP */
    {
	ErtsSchedulerData *esdp;
	esdp = ERTS_SCHEDULER_IX(0);
	erts_scheduler_data = esdp;
    }
    erts_no_schedulers = 1;
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_no_dirty_cpu_schedulers = 0;
    erts_no_dirty_io_schedulers = 0;
#endif
#endif

#ifndef ERTS_SMP
#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    erts_scheduler_data->verify_unused_temp_alloc = NULL;
    /*	   = erts_alloc_get_verify_unused_temp_alloc( */
    /*	       &erts_scheduler_data->verify_unused_temp_alloc_data); */
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(NULL);
#endif
#endif

#if !defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    erts_lc_set_thread_name("scheduler 1");
#endif
}

#ifdef USE_THREADS

ErtsSchedulerData *
erts_get_scheduler_data(void)
{
    return ERTS_SCHEDULER_IX(epiphany_coreno());
}

#endif

#ifdef ERTS_SMP

static ERTS_INLINE void
cancel_suspend_of_suspendee(Process *p, ErtsProcLocks p_locks)
{
    if (is_not_nil(p->suspendee)) {
	EPIPHANY_STUB_BT();
    }
}

#endif

static void
final_proc_cleanup(ErtsSchedulerData *esdp, Process *p)
{
#ifdef ERTS_SMP
    ASSERT(esdp->free_process == p);
    esdp->free_process = NULL;
#else
    state = erts_smp_atomic32_read_nob(&p->state);
    ASSERT(!(state & ERTS_PSFLG_IN_RUNQ));
#endif
    /*
     * Free all pending message buffers.
     */
    if (p->mbuf != NULL) {
	free_message_buffer(p->mbuf);
	p->mbuf = NULL;
    }

    erts_free_proc(p);
}

Process *schedule(Process *p, int calls)
{
    struct slave_syscall_ready *ready_arg = 0;
    ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());
    if (p) {
	erts_aint32_t state = erts_smp_atomic32_read_acqb(&p->state);
	ErtsSchedulerData *esdp;
#ifdef ERTS_SMP
	ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
	esdp = p->scheduler_data;
	ASSERT(esdp->current_process == p
	       || esdp->free_process == p);
#else
	esdp = erts_scheduler_data;
	ASSERT(esdp->current_process == p);
#endif
	p->reds += calls;
	while (erts_dispatch_slave_commands(p) != 0);
	if (state & ERTS_PSFLG_FREE) {
	    ready_arg = erts_alloc(ERTS_ALC_T_TMP,
				   sizeof(struct slave_syscall_ready));
	    ready_arg->exit_reason = p->fvalue;
	    slave_state_swapout(p, &ready_arg->state);
	    ready_arg->state.mbuf = NULL; /* We will free mbuf */
	    erts_master_syscall(SLAVE_SYSCALL_READY, ready_arg);

	    final_proc_cleanup(esdp, p);
	} else {
	    if (state & ERTS_PSFLG_PENDING_EXIT)
		erts_handle_pending_exit(p, ERTS_PROC_LOCK_MAIN);

	    p->fcalls = CONTEXT_REDS;
	    return p;
	}
    }
    {
	ErlSpawnOpts so;
	ErtsSchedulerData *esdp = erts_get_scheduler_data();
	if (!ready_arg) {
	    ready_arg = erts_alloc(ERTS_ALC_T_TMP,
				   sizeof(struct slave_syscall_ready));
	    erts_master_syscall(SLAVE_SYSCALL_READY, ready_arg);
	}
	so.flags = 0;
	p = erl_create_process_ptr(ready_arg, &so);
	ASSERT(epiphany_in_dram(p));
	p->fcalls = CONTEXT_REDS;

#ifndef ERTS_SMP
	erts_scheduler_data = calloc(1, sizeof(ErtsSchedulerData));
	ASSERT(epiphany_in_dram(erts_scheduler_data));
	init_scheduler_data(erts_scheduler_data, 0, NULL, NULL, NULL, 0);
#else
	p->scheduler_data = esdp;
	esdp->current_process = p;

	/* Lock the main lock so lc is happy */
	erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
#endif
	erts_smp_atomic32_set_nob(&p->state, erts_smp_atomic32_read_nob(&p->state) | ERTS_PSFLG_RUNNING);
	erts_free(ERTS_ALC_T_TMP, ready_arg);
	return p;
    }
}

int
erts_set_gc_state(Process *c_p, int enable)
{
    ErtsProcSysTaskQs *dgc_tsk_qs;
    ASSERT(c_p == erts_get_current_process());
    ASSERT((ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS)
	   & erts_smp_atomic32_read_nob(&c_p->state));
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(c_p));

    if (!enable) {
	c_p->flags |= F_DISABLE_GC;
	return 0;
    }

    c_p->flags &= ~F_DISABLE_GC;

    dgc_tsk_qs = ERTS_PROC_GET_DELAYED_GC_TASK_QS(c_p);
    if (!dgc_tsk_qs)
	return 0;

    EPIPHANY_STUB_BT();
    return 1;
}

static ERTS_INLINE void
set_proc_exiting(Process *p,
		 erts_aint32_t in_state,
		 Eterm reason,
		 ErlHeapFragment *bp)
{
    erts_aint32_t state = in_state;
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(p) == ERTS_PROC_LOCKS_ALL);

    change_proc_schedule_state(p,
			       ERTS_PSFLG_SUSPENDED|ERTS_PSFLG_PENDING_EXIT,
			       ERTS_PSFLG_EXITING|ERTS_PSFLG_ACTIVE,
			       &state);

    p->fvalue = reason;
    if (bp)
	erts_link_mbuf_to_proc(p, bp);
    /*
     * We used to set freason to EXC_EXIT here, but there is no need to
     * save the stack trace since this process irreversibly is going to
     * exit.
     */
    p->freason = EXTAG_EXIT;
    KILL_CATCHES(p);
    p->i = (BeamInstr *) beam_exit;
}

static ERTS_INLINE erts_aint32_t
set_proc_self_exiting(Process *c_p)
{
    erts_aint32_t state;

    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == ERTS_PROC_LOCKS_ALL);

    state = erts_smp_atomic32_read_nob(&c_p->state);
    ASSERT(state & (ERTS_PSFLG_RUNNING|ERTS_PSFLG_RUNNING_SYS));

    change_proc_schedule_state(c_p,
			       ERTS_PSFLG_SUSPENDED|ERTS_PSFLG_PENDING_EXIT,
			       ERTS_PSFLG_EXITING|ERTS_PSFLG_ACTIVE,
			       &state);

    return state;
}

void
erts_handle_pending_exit(Process *c_p, ErtsProcLocks locks)
{
    ErtsProcLocks xlocks;
    ASSERT(is_value(c_p->pending_exit.reason));
    ERTS_SMP_LC_ASSERT(erts_proc_lc_my_proc_locks(c_p) == locks);
    ERTS_SMP_LC_ASSERT(locks & ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_LC_ASSERT(!((ERTS_PSFLG_EXITING|ERTS_PSFLG_FREE)
			 & erts_smp_atomic32_read_nob(&c_p->state)));

    /* Ensure that all locks on c_p are locked before proceeding... */
    if (locks == ERTS_PROC_LOCKS_ALL)
	xlocks = 0;
    else {
	xlocks = ~locks & ERTS_PROC_LOCKS_ALL;
	if (erts_smp_proc_trylock(c_p, xlocks) == EBUSY) {
	    erts_smp_proc_unlock(c_p, locks & ~ERTS_PROC_LOCK_MAIN);
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCKS_ALL_MINOR);
	}
    }

    set_proc_exiting(c_p,
		     erts_smp_atomic32_read_acqb(&c_p->state),
		     c_p->pending_exit.reason,
		     c_p->pending_exit.bp);
    c_p->pending_exit.reason = THE_NON_VALUE;
    c_p->pending_exit.bp = NULL;

    if (xlocks)
	erts_smp_proc_unlock(c_p, xlocks);
}

/* this function fishishes a process and propagates exit messages - called
   by process_main when a process dies */
void 
erts_do_exit_process(Process* p, Eterm reason)
{
    p->arity = 0;		/* No live registers */
    p->fvalue = reason;

#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(process_exit)) {
        DTRACE_CHARBUF(process_buf, DTRACE_TERM_BUF_SIZE);
        DTRACE_CHARBUF(reason_buf, DTRACE_TERM_BUF_SIZE);

        dtrace_proc_str(p, process_buf);
        erts_snprintf(reason_buf, DTRACE_TERM_BUF_SIZE - 1, "%T", reason);
        DTRACE2(process_exit, process_buf, reason_buf);
    }
#endif

#ifdef ERTS_SMP
    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
    /* By locking all locks (main lock is already locked) when going
       to exiting state (ERTS_PSFLG_EXITING), it is enough to take any lock when
       looking up a process (erts_pid2proc()) to prevent the looked up
       process from exiting until the lock has been released. */
    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
#endif

#ifndef ERTS_SMP
    set_proc_self_exiting(p);
#else
    if (ERTS_PSFLG_PENDING_EXIT & set_proc_self_exiting(p)) {
	/* Process exited before pending exit was received... */
	p->pending_exit.reason = THE_NON_VALUE;
	if (p->pending_exit.bp) {
	    free_message_buffer(p->pending_exit.bp);
	    p->pending_exit.bp = NULL;
	}
    }

    cancel_suspend_of_suspendee(p, ERTS_PROC_LOCKS_ALL); 

    ERTS_SMP_MSGQ_MV_INQ2PRIVQ(p);
#endif

    /* ESTUB: tracing */
    /* if (IS_TRACED(p)) { */
    /* 	if (IS_TRACED_FL(p, F_TRACE_CALLS)) */
    /* 	    erts_schedule_time_break(p, ERTS_BP_CALL_TIME_SCHEDULE_EXITING); */

    /* 	if (IS_TRACED_FL(p,F_TRACE_PROCS)) */
    /* 	    trace_proc(p, p, am_exit, reason); */
    /* } */

    /* erts_trace_check_exiting(p->common.id); */

    /* ASSERT((ERTS_TRACE_FLAGS(p) & F_INITIAL_TRACE_FLAGS) */
    /* 	   == F_INITIAL_TRACE_FLAGS); */

    if (p->u.bif_timers)
	EPIPHANY_STUB_BT();

    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL_MINOR);

    /*
     * The p->u.bif_timers of this process can *not* be used anymore;
     * will be overwritten by misc termination data.
     */
    p->u.terminate = NULL;

    erts_continue_exit_process(p);
}

void
erts_continue_exit_process(Process *p)
{
    /* ErtsLink* lnk; */
    /* ErtsMonitor *mon; */
    /* ErtsProcLocks curr_locks = ERTS_PROC_LOCK_MAIN; */
    /* Eterm reason = p->fvalue; */
    DistEntry *dep;
    /* struct saved_calls *scb; */
    /* process_breakpoint_time_t *pbt; */
    erts_aint32_t state;
    /* void *nif_export; */

#ifdef DEBUG
    /* int yield_allowed = 1; */
#endif

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN == erts_proc_lc_my_proc_locks(p));

    ASSERT(ERTS_PROC_IS_EXITING(p));

#ifdef ERTS_SMP
    if (p->flags & F_HAVE_BLCKD_MSCHED) {
	EPIPHANY_STUB_BT();
    }
#endif

    if (p->flags & F_USING_DB) {
	EPIPHANY_STUB_BT();
	/* if (erts_db_process_exiting(p, ERTS_PROC_LOCK_MAIN)) */
	/*     goto yield; */
	/* p->flags &= ~F_USING_DB; */
    }

    erts_set_gc_state(p, 1);
    state = erts_smp_atomic32_read_acqb(&p->state);
    if (state & ERTS_PSFLG_ACTIVE_SYS) {
	EPIPHANY_STUB_BT();
	/* if (cleanup_sys_tasks(p, state, CONTEXT_REDS) >= CONTEXT_REDS/2) */
	/*     goto yield; */
    }

    if (p->flags & F_USING_DDLL) {
	EPIPHANY_STUB_BT();
	/* erts_ddll_proc_dead(p, ERTS_PROC_LOCK_MAIN); */
	/* p->flags &= ~F_USING_DDLL; */
    }

    if (p->nodes_monitors) {
	EPIPHANY_STUB_BT();
	/* erts_delete_nodes_monitors(p, ERTS_PROC_LOCK_MAIN); */
	/* p->nodes_monitors = NULL; */
    }
	

    if (p->suspend_monitors) {
	EPIPHANY_STUB_BT();
	/* erts_sweep_suspend_monitors(p->suspend_monitors, */
	/* 			    resume_suspend_monitor, */
	/* 			    p); */
	/* p->suspend_monitors = NULL; */
    }

    /*
     * The registered name *should* be the last "erlang resource" to
     * cleanup.
     */
    if (p->common.u.alive.reg) {
	EPIPHANY_STUB_BT();
	/* (void) erts_unregister_name(p, ERTS_PROC_LOCK_MAIN, NULL, THE_NON_VALUE); */
	/* ASSERT(!p->common.u.alive.reg); */
    }

    erts_smp_proc_lock(p, ERTS_PROC_LOCKS_ALL_MINOR);
    /* curr_locks = ERTS_PROC_LOCKS_ALL; */

    /*
     * From this point on we are no longer allowed to yield
     * this process.
     */
#ifdef DEBUG
    /* yield_allowed = 0; */
#endif

    /*
     * Note! The monitor and link fields will be overwritten 
     * by erts_ptab_delete_element() below.
     */
    /* mon = ERTS_P_MONITORS(p); */
    /* lnk = ERTS_P_LINKS(p); */

    {
	/* Do *not* use erts_get_runq_proc() */
	/* ErtsRunQueue *rq; */
	/* rq = erts_get_runq_current(ERTS_GET_SCHEDULER_DATA_FROM_PROC(p)); */

	/* erts_smp_runq_lock(rq); */

#ifdef ERTS_SMP
	ASSERT(p->scheduler_data);
	ASSERT(p->scheduler_data->current_process == p);
	ASSERT(p->scheduler_data->free_process == NULL);

	p->scheduler_data->current_process = NULL;
	p->scheduler_data->free_process = p;
#endif

	/* Time of death! */
	/* erts_ptab_delete_element(&erts_proc, &p->common); */

	/* erts_smp_runq_unlock(rq); */
    }

    /*
     * All "erlang resources" have to be deallocated before this point,
     * e.g. registered name, so monitoring and linked processes can
     * be sure that all interesting resources have been deallocated
     * when the monitors and/or links hit.
     */

    {
	/* Inactivate and notify free */
	erts_aint32_t n, e, a = erts_smp_atomic32_read_nob(&p->state);
#ifdef ERTS_SMP
	int refc_inced = 0;
#endif
	while (1) {
	    n = e = a;
	    ASSERT(a & ERTS_PSFLG_EXITING);
	    n |= ERTS_PSFLG_FREE;
	    n &= ~ERTS_PSFLG_ACTIVE;
#ifdef ERTS_SMP
	    if ((n & ERTS_PSFLG_IN_RUNQ) && !refc_inced) {
		erts_smp_proc_inc_refc(p);
		refc_inced = 1;
	    }
#endif
	    a = erts_smp_atomic32_cmpxchg_mb(&p->state, n, e);
	    if (a == e)
		break;
	}

#ifdef ERTS_SMP
	if (refc_inced && !(n & ERTS_PSFLG_IN_RUNQ))
	    erts_smp_proc_dec_refc(p);
#endif
    }
    
    dep = ((p->flags & F_DISTRIBUTION)
	   ? ERTS_PROC_SET_DIST_ENTRY(p, ERTS_PROC_LOCKS_ALL, NULL)
	   : NULL);
    /* scb = ERTS_PROC_SET_SAVED_CALLS_BUF(p, ERTS_PROC_LOCKS_ALL, NULL); */
    /* pbt = ERTS_PROC_SET_CALL_TIME(p, ERTS_PROC_LOCKS_ALL, NULL); */
    /* nif_export = ERTS_PROC_SET_NIF_TRAP_EXPORT(p, ERTS_PROC_LOCKS_ALL, NULL); */

    erts_smp_proc_unlock(p, ERTS_PROC_LOCKS_ALL);
#ifdef BM_COUNTERS
    processes_busy--;
#endif

    if (dep) {
	EPIPHANY_STUB_BT();
	/* erts_do_net_exits(dep, reason); */
	/* if(dep) */
	/*     erts_deref_dist_entry(dep); */
    }

    delete_process(p);

#ifdef ERTS_SMP
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    ERTS_SMP_CHK_HAVE_ONLY_MAIN_PROC_LOCK(p);
#endif

    return;
}

void
slave_serve_timeout(Process *p, struct slave_command_timeout *cmd)
{
    BeamInstr** pi = (BeamInstr **) p->def_arg_reg;
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));
    if (!(p->flags & F_INSLPQUEUE)
	|| p->scheduler_data->timer_reference != cmd->reference
	|| p->common.id != cmd->process) {
	 /* The timeout is late, drop it */
	return;
    }

    p->i = *pi;
    p->flags |= F_TIMO;
    p->flags &= ~F_INSLPQUEUE;
    erts_smp_atomic32_read_bor_nob(&p->state, ERTS_PSFLG_ACTIVE);
}

void
cancel_timer(Process* p)
{
    struct master_command_timer cmd = {
	.op = MASTER_TIMER_OP_CANCEL,
	.process = p->common.id,
	.reference = p->scheduler_data->timer_reference,
    };
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));
    p->flags &= ~(F_INSLPQUEUE|F_TIMO);

    /*
     * We don't /need/ to send a cancel message. Clearing the F_INSLPQUEUE flag
     * is sufficient to inhibit the timeout when it arrives. Indeed, it must be
     * that way to prevent races with timeout messages that are sent before the
     * cancel message is received.
     */
    erts_master_send_command(MASTER_COMMAND_TIMER, &cmd, sizeof(cmd));
}

/*
 * Insert a process into the time queue, with a timeout 'timeout' in ms.
 */
void
set_timer(Process* p, Uint timeout)
{
    struct master_command_timer cmd = {
	.op = MASTER_TIMER_OP_SET,
	.process = p->common.id,
	.timeout = timeout,
    };
    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));

    /* check for special case timeout=0 DONT ADD TO time queue */
    if (timeout == 0) {
	p->flags |= F_TIMO;
	return;
    }
    p->flags |= F_INSLPQUEUE;
    p->flags &= ~F_TIMO;

    p->scheduler_data->timer_reference++;
    cmd.reference = p->scheduler_data->timer_reference;

    erts_master_send_command(MASTER_COMMAND_TIMER, &cmd, sizeof(cmd));
}

#if defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
int
erts_dbg_check_halloc_lock(Process *p)
{
    if (ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p))
	return 1;
    if (p->common.id == ERTS_INVALID_PID)
	return 1;
    if (p->scheduler_data && p == p->scheduler_data->match_pseudo_process)
	return 1;
    if (erts_thr_progress_is_blocking())
	return 1;
    return 0;
}
#endif

void
slave_serve_exit(Process *c_p, struct slave_command_exit *cmd)
{
    if (cmd->receiver != c_p->common.id
	|| (erts_smp_atomic32_read_nob(&c_p->state) & ERTS_PSFLG_EXITING)) {
	/* The message is for an already dead process; drop it */
	if (cmd->bp) free_master_message_buffer(cmd->bp);
    }

    /* We might want to copy the reason to heap, since we're able to */
    c_p->pending_exit.reason = cmd->reason;
    c_p->pending_exit.bp = cmd->bp;
    erts_smp_atomic32_read_bor_nob(&c_p->state, ERTS_PSFLG_PENDING_EXIT);
}
