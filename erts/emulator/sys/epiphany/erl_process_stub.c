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

static UWord thr_prgr_later_cleanup_op_threshold = ERTS_THR_PRGR_LATER_CLEANUP_OP_THRESHOLD_MEDIUM;

ErtsPTab erts_proc erts_align_attribute(ERTS_CACHE_LINE_SIZE);

int erts_sched_thread_suggested_stack_size = -1;

#ifdef ERTS_ENABLE_LOCK_CHECK
ErtsLcPSDLocks erts_psd_required_locks[ERTS_PSD_SIZE];
#endif

static struct {
    int aux_work;
    int tse;
    int sys_schedule;
} sched_busy_wait;

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
    erts_smp_mtx_t mtx;
    erts_smp_cnd_t cnd;
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
    erts_smp_mtx_t update_mtx;
    erts_smp_atomic32_t no_runqs;
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

static erts_smp_atomic32_t function_calls;

#ifdef ERTS_SMP
static erts_smp_atomic32_t doing_sys_schedule;
static erts_smp_atomic32_t no_empty_run_queues;
long erts_runq_supervision_interval = 0;
static ethr_event runq_supervision_event;
static erts_tid_t runq_supervisor_tid;
static erts_atomic_t runq_supervisor_sleeping;
#else /* !ERTS_SMP */
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

static ErtsAlignedSchedulerSleepInfo *aligned_sched_sleep_info;
#ifdef ERTS_DIRTY_SCHEDULERS
#ifdef ERTS_SMP
static ErtsAlignedSchedulerSleepInfo *aligned_dirty_cpu_sched_sleep_info;
static ErtsAlignedSchedulerSleepInfo *aligned_dirty_io_sched_sleep_info;
#endif
#endif

static Uint last_reductions;
static Uint last_exact_reductions;
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

#define ERTS_PROC_SYS_TASK_SIZE(HSz) \
    (sizeof(ErtsProcSysTask) - sizeof(Eterm) + sizeof(Eterm)*(HSz))

struct ErtsProcSysTaskQs_ {
    int qmask;
    int ncount;
    ErtsProcSysTask *q[ERTS_NO_PROC_PRIO_LEVELS];
};

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(proc_sys_task_queues,
				 ErtsProcSysTaskQs,
				 50,
				 ERTS_ALC_T_PROC_SYS_TSK_QS)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(misc_op_list,
				 ErtsMiscOpList,
				 10,
				 ERTS_ALC_T_MISC_OP_LIST)

ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(proclist,
				 ErtsProcList,
				 200,
				 ERTS_ALC_T_PROC_LIST)

#define ERTS_SCHED_SLEEP_INFO_IX(IX)					\
    (ASSERT(-1 <= ((int) (IX))					        \
		 && ((int) (IX)) < ((int) erts_no_schedulers)),		\
     &aligned_sched_sleep_info[(IX)].ssi)
#ifdef ERTS_DIRTY_SCHEDULERS
#define ERTS_DIRTY_CPU_SCHED_SLEEP_INFO_IX(IX)				\
    (ASSERT(0 <= ((int) (IX))					        \
	    && ((int) (IX)) < ((int) erts_no_dirty_cpu_schedulers)),	\
     &aligned_dirty_cpu_sched_sleep_info[(IX)].ssi)
#define ERTS_DIRTY_IO_SCHED_SLEEP_INFO_IX(IX)				\
    (ASSERT(0 <= ((int) (IX))					        \
	    && ((int) (IX)) < ((int) erts_no_dirty_io_schedulers)),	\
     &aligned_dirty_io_sched_sleep_info[(IX)].ssi)
#endif

#define ERTS_FOREACH_RUNQ(RQVAR, DO)					\
do {									\
    ErtsRunQueue *RQVAR;						\
    int ix__;								\
    for (ix__ = 0; ix__ < erts_no_run_queues; ix__++) {			\
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_smp_runq_lock(RQVAR);					\
	{ DO; }								\
	erts_smp_runq_unlock(RQVAR);					\
    }									\
} while (0)

#define ERTS_FOREACH_OP_RUNQ(RQVAR, DO)					\
do {									\
    ErtsRunQueue *RQVAR;						\
    int ix__;								\
    ERTS_SMP_LC_ASSERT(erts_smp_lc_mtx_is_locked(&schdlr_sspnd.mtx));	\
    for (ix__ = 0; ix__ < schdlr_sspnd.online; ix__++) {		\
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_smp_runq_lock(RQVAR);					\
	{ DO; }								\
	erts_smp_runq_unlock(RQVAR);					\
    }									\
} while (0)

#define ERTS_ATOMIC_FOREACH_RUNQ_X(RQVAR, DO, DOX)			\
do {									\
    ErtsRunQueue *RQVAR;						\
    int ix__;								\
    for (ix__ = 0; ix__ < erts_no_run_queues; ix__++) {			\
	RQVAR = ERTS_RUNQ_IX(ix__);					\
	erts_smp_runq_lock(RQVAR);					\
	{ DO; }								\
    }									\
    { DOX; }								\
    for (ix__ = 0; ix__ < erts_no_run_queues; ix__++)			\
	erts_smp_runq_unlock(ERTS_RUNQ_IX(ix__));			\
} while (0)

#define ERTS_ATOMIC_FOREACH_RUNQ(RQVAR, DO) \
  ERTS_ATOMIC_FOREACH_RUNQ_X(RQVAR, DO, )

Process *schedule(Process *p, int calls)
{
    if (p) {
        p->reds += calls;
        return p;
    }
    EPIPHANY_STUB(schedule);
}
