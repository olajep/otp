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
#include "erl_node_container_utils.h"
#include "epiphany.h"


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

#ifdef USE_THREADS
static erts_tsd_key_t sched_data_key;
#endif

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
    // erts_tsd_set(sched_data_key, vesdp);
#ifdef ERTS_SMP
#if HAVE_ERTS_MSEG
    erts_mseg_late_init();
#endif
#if ERTS_USE_ASYNC_READY_Q
    esdp->aux_work_data.async_ready.queue = erts_get_async_ready_queue(no);
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

    process_main();
    /* No schedulers should *ever* terminate */
    erl_exit(ERTS_ABORT_EXIT,
	     "Scheduler thread number %beu terminated\n",
	     no);
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
    RUNQ_SET_RQ(&p->run_queue, ERTS_RUNQ_IX(0));
#endif

#if !defined(NO_FPE_SIGNALS) || defined(HIPE)
    p->fp_exception = 0;
#endif

}

void
erts_cleanup_empty_process(Process* p)
{
    /* We only check fields that are known to be used... */

    // erts_cleanup_offheap(&p->off_heap); // ESTUB
    p->off_heap.first = NULL;
    p->off_heap.overhead = 0;

    if (p->mbuf != NULL) {
	free_message_buffer(p->mbuf);
	p->mbuf = NULL;
    }
#ifdef DEBUG
    erts_debug_verify_clean_empty_process(p);
#endif
}

/*
** Allocate process and find out where to place next process.
*/
static Process*
alloc_process(ErtsRunQueue *rq, erts_aint32_t state)
{
    Process *p;

    p = erts_alloc_fnf(ERTS_ALC_T_PROC, sizeof(Process));
    if (!p)
	return NULL;

    ASSERT(((char *) p) == ((char *) &p->common));

    p->common.id = 0x3;
    ASSERT(internal_pid_serial(p->common.id) <= ERTS_MAX_PID_SERIAL);
    
    p->rcount = 0;
    p->heap = NULL;

    /* ASSERT(p == (Process *) (erts_ptab_pix2intptr_nob( */
    /* 				 &erts_proc, */
    /* 				 internal_pid_index(p->common.id)))); */

    return p;
}

static Process *
erl_create_process_ptr(Process* parent, /* Parent of process (default group leader). */
		       Eterm mod,	/* Tagged atom for module. */
		       Eterm func,	/* Tagged atom for function. */
		       Eterm args,	/* Arguments for function (must be well-formed list). */
		       ErlSpawnOpts* so) /* Options for spawn. */
{
    Process *p;
    Sint arity;			/* Number of arguments. */
    Uint arg_size;		/* Size of arguments. */
    Uint sz;			/* Needed words on heap. */
    Uint heap_need;		/* Size needed on heap. */
    Process *res = NULL;
    erts_aint32_t state = 0;
    erts_aint32_t prio = (erts_aint32_t) PRIORITY_NORMAL;

    /*
     * Check for errors.
     */

    if (is_not_atom(mod) || is_not_atom(func) || ((arity = erts_list_length(args)) < 0)) {
	so->error_code = BADARG;
	goto error;
    }

    state |= (((prio & ERTS_PSFLGS_PRIO_MASK) << ERTS_PSFLGS_ACT_PRIO_OFFSET)
	      | ((prio & ERTS_PSFLGS_PRIO_MASK) << ERTS_PSFLGS_USR_PRIO_OFFSET));

    p = alloc_process(NULL, state); /* All proc locks are locked by this thread
				     on success */
    if (!p) {
	erts_send_error_to_logger_str(parent->group_leader,
				      "Too many processes\n");
	so->error_code = SYSTEM_LIMIT;
	goto error;
    }

#ifdef BM_COUNTERS
    processes_busy++;
#endif
    BM_COUNT(processes_spawned);

    BM_SWAP_TIMER(system,size);
    arg_size = size_object(args);
    BM_SWAP_TIMER(size,system);
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
    // ASSERT(p->min_heap_size == erts_next_heap_size(p->min_heap_size, 0));
    
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
	sz = MAX(1024, heap_need);
	// sz = erts_next_heap_size(heap_need, 0);
    }

#ifdef HIPE
    hipe_init_process(&p->hipe);
#ifdef ERTS_SMP
    hipe_init_process_smp(&p->hipe_smp);
#endif
#endif
    p->heap = (Eterm *) ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP, sizeof(Eterm)*sz);
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

    p->i = (BeamInstr *) beam_apply;
    p->cp = (BeamInstr *) beam_apply+1;

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

    /*
     * Check if this process should be initially linked to its parent.
     */

    if (so->flags & SPO_LINK) {
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

    res = p;

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

   return res;
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
    esdp->x_reg_array = calloc(ERTS_X_REGS_ALLOCATED, sizeof(Eterm));
    ASSERT(epiphany_in_dram(esdp->x_reg_array));
    esdp->f_reg_array = calloc(MAX_REG, sizeof(FloatDef));
    ASSERT(epiphany_in_dram(esdp->f_reg_array));
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
#ifdef USE_THREADS
	erts_tsd_set(sched_data_key, (void *) esdp);
#endif
    }
    erts_no_schedulers = 1;
#ifdef ERTS_DIRTY_SCHEDULERS
    erts_no_dirty_cpu_schedulers = 0;
    erts_no_dirty_io_schedulers = 0;
#endif
#endif

#ifndef ERTS_SMP
#ifdef ERTS_DO_VERIFY_UNUSED_TEMP_ALLOC
    erts_scheduler_data->verify_unused_temp_alloc
	= erts_alloc_get_verify_unused_temp_alloc(
	    &erts_scheduler_data->verify_unused_temp_alloc_data);
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(NULL);
#endif
#endif

#if !defined(ERTS_SMP) && defined(ERTS_ENABLE_LOCK_CHECK)
    erts_lc_set_thread_name("scheduler 1");
#endif
}

#ifdef ERTS_SMP

ErtsSchedulerData *
erts_get_scheduler_data(void)
{
    return ERTS_SCHEDULER_IX(epiphany_coreno());
}

#endif

Process *schedule(Process *p, int calls)
{
    extern BeamInstr *demo_prog;
    if (p) {
        p->reds += calls;
        return p;
    } else {
	ErlSpawnOpts so;
	Process parent;
	erts_init_empty_process(&parent);
	so.flags = 0;
	p = erl_create_process_ptr(&parent, am_false, am_start, NIL, &so);
	ASSERT(epiphany_in_dram(p));
	erts_cleanup_empty_process(&parent);
	p->i = demo_prog;
	// Should last a while
	p->fcalls = 100000;

#ifndef ERTS_SMP
	erts_scheduler_data = calloc(1, sizeof(ErtsSchedulerData));
	ASSERT(epiphany_in_dram(erts_scheduler_data));
	init_scheduler_data(erts_scheduler_data, 0, NULL, NULL, NULL, 0);
#else
	p->scheduler_data = erts_get_scheduler_data();
#endif
	return p;
    }
}
