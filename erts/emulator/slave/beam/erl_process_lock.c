/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2007-2015. All Rights Reserved.
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


/*
 * Description:	Impementation of Erlang process locks.
 *
 * Author: 	Rickard Green
 */

/*
 * A short explanation of the process lock implementation:
 *     Each process has a lock bitfield and a number of lock wait
 *   queues.
 *     The bit field contains of a number of lock flags (L1, L2, ...)
 *   and a number of wait flags (W1, W2, ...). Each lock flag has a
 *   corresponding wait flag. The bit field isn't guarranteed to be
 *   larger than 32-bits which sets a maximum of 16 different locks
 *   per process. Currently, only 4 locks per process are used. The
 *   bit field is operated on by use of atomic operations (custom
 *   made bitwise atomic operations). When a lock is locked the
 *   corresponding lock bit is set. When a thread is waiting on a
 *   lock the wait flag for the lock is set.
 *     The process table is protected by pix (process index) locks
 *   which is spinlocks that protects a number of process indices in
 *   the process table. The pix locks also protects the lock queues
 *   and modifications of wait flags.
 *     When acquiring a process lock we first try to set the lock
 *   flag. If we are able to set the lock flag and the wait flag
 *   isn't set we are done. If the lock flag was already set we
 *   have to acquire the pix lock, set the wait flag, and put
 *   ourselves in the wait queue.
 *   Process locks will always be acquired in fifo order.
 *     When releasing a process lock we first unset all lock flags
 *   whose corresponding wait flag is clear (which will succeed).
 *   If wait flags were set for the locks being released, we acquire
 *   the pix lock, and transfer the lock to the first thread
 *   in the wait queue.
 *     Note that wait flags may be read without the pix lock, but
 *   it is important that wait flags only are modified when the pix
 *   lock is held.
 *     This implementation assumes that erts_smp_atomic_or_retold()
 *   provides necessary memorybarriers for a lock operation, and that
 *   erts_smp_atomic_and_retold() provides necessary memorybarriers
 *   for an unlock operation.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "erl_process.h"
#include "erl_process_lock.h"
#include "erl_thr_progress.h"

#ifdef ERTS_SMP

#if ERTS_PROC_LOCK_OWN_IMPL

#define ERTS_PROC_LOCK_SPIN_COUNT_MAX  2000
#define ERTS_PROC_LOCK_SPIN_COUNT_SCHED_INC 32
#define ERTS_PROC_LOCK_SPIN_COUNT_BASE 1000
#define ERTS_PROC_LOCK_AUX_SPIN_COUNT 50

#define ERTS_PROC_LOCK_SPIN_UNTIL_YIELD 25

#ifdef ERTS_PROC_LOCK_DEBUG
#define ERTS_PROC_LOCK_HARD_DEBUG
#endif

#if SIZEOF_INT < 4
#error "The size of the 'uflgs' field of the erts_tse_t type is too small"
#endif

static int proc_lock_spin_count;
static int aux_thr_proc_lock_spin_count;

static void cleanup_tse(void);

#endif /* ERTS_PROC_LOCK_OWN_IMPL */

#ifdef ERTS_ENABLE_LOCK_CHECK
static struct {
    Sint16 proc_lock_main;
    Sint16 proc_lock_link;
    Sint16 proc_lock_msgq;
    Sint16 proc_lock_status;
} lc_id;
#endif

erts_pix_lock_t erts_pix_locks[ERTS_NO_OF_PIX_LOCKS];


void
erts_init_proc_lock(int cpus)
{
    int i;
    for (i = 0; i < ERTS_NO_OF_PIX_LOCKS; i++) {
#ifdef ERTS_ENABLE_LOCK_COUNT
	erts_mtx_init_x(&erts_pix_locks[i].u.mtx,
			"pix_lock", make_small(i), 1);
#else
	erts_mtx_init(&erts_pix_locks[i].u.mtx, "pix_lock");
#endif
    }
#if ERTS_PROC_LOCK_OWN_IMPL
    erts_thr_install_exit_handler(cleanup_tse);
    if (cpus > 1) {
	proc_lock_spin_count = ERTS_PROC_LOCK_SPIN_COUNT_BASE;
	proc_lock_spin_count += (ERTS_PROC_LOCK_SPIN_COUNT_SCHED_INC
				 * ((int) erts_no_schedulers));
	aux_thr_proc_lock_spin_count = ERTS_PROC_LOCK_AUX_SPIN_COUNT;
    }
    else if (cpus == 1) {
	proc_lock_spin_count = 0;
	aux_thr_proc_lock_spin_count = 0;
    }
    else { /* No of cpus unknown. Assume multi proc, but be conservative. */
	proc_lock_spin_count = ERTS_PROC_LOCK_SPIN_COUNT_BASE/2;
	aux_thr_proc_lock_spin_count = ERTS_PROC_LOCK_AUX_SPIN_COUNT/2;
    }
    if (proc_lock_spin_count > ERTS_PROC_LOCK_SPIN_COUNT_MAX)
	proc_lock_spin_count = ERTS_PROC_LOCK_SPIN_COUNT_MAX;
#endif
#ifdef ERTS_ENABLE_LOCK_CHECK
    lc_id.proc_lock_main	= erts_lc_get_lock_order_id("proc_main");
    lc_id.proc_lock_link	= erts_lc_get_lock_order_id("proc_link");
    lc_id.proc_lock_msgq	= erts_lc_get_lock_order_id("proc_msgq");
    lc_id.proc_lock_status	= erts_lc_get_lock_order_id("proc_status");
#endif
}

#if ERTS_PROC_LOCK_OWN_IMPL

#ifdef ERTS_ENABLE_LOCK_CHECK
#define CHECK_UNUSED_TSE(W) ERTS_LC_ASSERT((W)->uflgs == 0)
#else
#define CHECK_UNUSED_TSE(W)
#endif

static ERTS_INLINE erts_tse_t *
tse_fetch(erts_pix_lock_t *pix_lock)
{
    erts_tse_t *tse = erts_tse_fetch();
    tse->uflgs = 0;
    return tse;
}

static ERTS_INLINE void
tse_return(erts_tse_t *tse)
{
    CHECK_UNUSED_TSE(tse);
    erts_tse_return(tse);
}

static void
cleanup_tse(void)
{
    erts_tse_t *tse = erts_tse_fetch();
    if (tse)
	erts_tse_return(tse);
}

/*
 * erts_proc_lock_failed() is called when erts_smp_proc_lock()
 * wasn't able to lock all locks. We may need to transfer locks
 * to waiters and wait for our turn on locks.
 *
 * Iff !ERTS_PROC_LOCK_ATOMIC_IMPL, the pix lock is locked on entry.
 *
 * This always returns with the pix lock unlocked.
 */
void
erts_proc_lock_failed(Process *p,
		      erts_pix_lock_t *pixlck,
		      ErtsProcLocks locks,
		      ErtsProcLocks old_lflgs)
{
    EPIPHANY_STUB_FUN();
}

/*
 * erts_proc_unlock_failed() is called when erts_smp_proc_unlock()
 * wasn't able to unlock all locks. We may need to transfer locks
 * to waiters.
 */
void
erts_proc_unlock_failed(Process *p,
			erts_pix_lock_t *pixlck,
			ErtsProcLocks wait_locks)
{
    EPIPHANY_STUB_FUN();
}

#endif /* ERTS_PROC_LOCK_OWN_IMPL */

void
erts_proc_lock_prepare_proc_lock_waiter(void)
{
#if ERTS_PROC_LOCK_OWN_IMPL
    tse_return(tse_fetch(NULL));
#endif
}

void
erts_proc_safelock(Process *a_proc,
		   ErtsProcLocks a_have_locks,
		   ErtsProcLocks a_need_locks,
		   Process *b_proc,
		   ErtsProcLocks b_have_locks,
		   ErtsProcLocks b_need_locks)
{
    EPIPHANY_STUB_FUN();
}

void
erts_proc_lock_init(Process *p)
{
    int i;
#if ERTS_PROC_LOCK_OWN_IMPL
    /* We always start with all locks locked */
#if ERTS_PROC_LOCK_ATOMIC_IMPL
    erts_smp_atomic32_init_nob(&p->lock.flags,
			       (erts_aint32_t) ERTS_PROC_LOCKS_ALL);
#else
    p->lock.flags = ERTS_PROC_LOCKS_ALL;
#endif
    for (i = 0; i <= ERTS_PROC_LOCK_MAX_BIT; i++)
	p->lock.queue[i] = NULL;
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_proc_lc_trylock(p, ERTS_PROC_LOCKS_ALL, 1,__FILE__,__LINE__);
#endif
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL

#ifdef ERTS_ENABLE_LOCK_COUNT
    int do_lock_count = 1;
#else
    int do_lock_count = 0;
#endif

    erts_mtx_init_x(&p->lock.main, "proc_main", p->common.id, do_lock_count);
    ethr_mutex_lock(&p->lock.main.mtx);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(1, &p->lock.main.lc);
#endif
    erts_mtx_init_x(&p->lock.link, "proc_link", p->common.id, do_lock_count);
    ethr_mutex_lock(&p->lock.link.mtx);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(1, &p->lock.link.lc);
#endif
    erts_mtx_init_x(&p->lock.msgq, "proc_msgq", p->common.id, do_lock_count);
    ethr_mutex_lock(&p->lock.msgq.mtx);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(1, &p->lock.msgq.lc);
#endif
    erts_mtx_init_x(&p->lock.status, "proc_status", p->common.id,
		    do_lock_count);
    ethr_mutex_lock(&p->lock.status.mtx);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_trylock(1, &p->lock.status.lc);
#endif
#endif
    erts_atomic32_init_nob(&p->lock.refc, 1);
#ifdef ERTS_PROC_LOCK_DEBUG
    for (i = 0; i <= ERTS_PROC_LOCK_MAX_BIT; i++)
	erts_smp_atomic32_init_nob(&p->lock.locked[i], (erts_aint32_t) 1);
#endif
#ifdef ERTS_ENABLE_LOCK_COUNT
    erts_lcnt_proc_lock_init(p);
    erts_lcnt_proc_lock(&(p->lock), ERTS_PROC_LOCKS_ALL);
    erts_lcnt_proc_lock_post_x(&(p->lock), ERTS_PROC_LOCKS_ALL, __FILE__, __LINE__);
#endif
}

void
erts_proc_lock_fin(Process *p)
{
#if ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    erts_mtx_destroy(&p->lock.main);
    erts_mtx_destroy(&p->lock.link);
    erts_mtx_destroy(&p->lock.msgq);
    erts_mtx_destroy(&p->lock.status);
#endif
#if defined(ERTS_ENABLE_LOCK_COUNT) && defined(ERTS_SMP)
    erts_lcnt_proc_lock_destroy(p);
#endif
}

/* --- Process lock counting ----------------------------------------------- */

#if ERTS_PROC_LOCK_OWN_IMPL && defined(ERTS_ENABLE_LOCK_COUNT)
void erts_lcnt_proc_lock_init(Process *p) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_PROCLOCK) {
    if (p->common.id != ERTS_INVALID_PID) {
	erts_lcnt_init_lock_x(&(p->lock.lcnt_main),   "proc_main",   ERTS_LCNT_LT_PROCLOCK, p->common.id);
	erts_lcnt_init_lock_x(&(p->lock.lcnt_msgq),   "proc_msgq",   ERTS_LCNT_LT_PROCLOCK, p->common.id);
	erts_lcnt_init_lock_x(&(p->lock.lcnt_link),   "proc_link",   ERTS_LCNT_LT_PROCLOCK, p->common.id);
	erts_lcnt_init_lock_x(&(p->lock.lcnt_status), "proc_status", ERTS_LCNT_LT_PROCLOCK, p->common.id);
    } else {
	erts_lcnt_init_lock(&(p->lock.lcnt_main),   "proc_main",   ERTS_LCNT_LT_PROCLOCK);
	erts_lcnt_init_lock(&(p->lock.lcnt_msgq),   "proc_msgq",   ERTS_LCNT_LT_PROCLOCK);
	erts_lcnt_init_lock(&(p->lock.lcnt_link),   "proc_link",   ERTS_LCNT_LT_PROCLOCK);
	erts_lcnt_init_lock(&(p->lock.lcnt_status), "proc_status", ERTS_LCNT_LT_PROCLOCK);
    }
    } else {
	sys_memzero(&(p->lock.lcnt_main), sizeof(p->lock.lcnt_main));
	sys_memzero(&(p->lock.lcnt_msgq), sizeof(p->lock.lcnt_msgq));
	sys_memzero(&(p->lock.lcnt_link), sizeof(p->lock.lcnt_link));
	sys_memzero(&(p->lock.lcnt_status), sizeof(p->lock.lcnt_status));
    }
}
	

void erts_lcnt_proc_lock_destroy(Process *p) {
    erts_lcnt_destroy_lock(&(p->lock.lcnt_main));
    erts_lcnt_destroy_lock(&(p->lock.lcnt_msgq));
    erts_lcnt_destroy_lock(&(p->lock.lcnt_link));
    erts_lcnt_destroy_lock(&(p->lock.lcnt_status));
}

void erts_lcnt_proc_lock(erts_proc_lock_t *lock, ErtsProcLocks locks) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_PROCLOCK) { 
    if (locks & ERTS_PROC_LOCK_MAIN) {
	erts_lcnt_lock(&(lock->lcnt_main));
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
        erts_lcnt_lock(&(lock->lcnt_msgq));
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	erts_lcnt_lock(&(lock->lcnt_link));
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	erts_lcnt_lock(&(lock->lcnt_status));
    }
    }
}

void erts_lcnt_proc_lock_post_x(erts_proc_lock_t *lock, ErtsProcLocks locks, char *file, unsigned int line) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_PROCLOCK) { 
    if (locks & ERTS_PROC_LOCK_MAIN) {
	erts_lcnt_lock_post_x(&(lock->lcnt_main), file, line);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
        erts_lcnt_lock_post_x(&(lock->lcnt_msgq), file, line);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	erts_lcnt_lock_post_x(&(lock->lcnt_link), file, line);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	erts_lcnt_lock_post_x(&(lock->lcnt_status), file, line);
    }
    }
}

void erts_lcnt_proc_lock_unaquire(erts_proc_lock_t *lock, ErtsProcLocks locks) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_PROCLOCK) { 
    if (locks & ERTS_PROC_LOCK_MAIN) {
	erts_lcnt_lock_unaquire(&(lock->lcnt_main));
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
        erts_lcnt_lock_unaquire(&(lock->lcnt_msgq));
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	erts_lcnt_lock_unaquire(&(lock->lcnt_link));
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	erts_lcnt_lock_unaquire(&(lock->lcnt_status));
    }
    }
}

void erts_lcnt_proc_unlock(erts_proc_lock_t *lock, ErtsProcLocks locks) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_PROCLOCK) { 
    if (locks & ERTS_PROC_LOCK_MAIN) {
	erts_lcnt_unlock(&(lock->lcnt_main));
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
        erts_lcnt_unlock(&(lock->lcnt_msgq));
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	erts_lcnt_unlock(&(lock->lcnt_link));
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	erts_lcnt_unlock(&(lock->lcnt_status));
    }
    }
}
void erts_lcnt_proc_trylock(erts_proc_lock_t *lock, ErtsProcLocks locks, int res) {
    if (erts_lcnt_rt_options & ERTS_LCNT_OPT_PROCLOCK) { 
    if (locks & ERTS_PROC_LOCK_MAIN) {
	erts_lcnt_trylock(&(lock->lcnt_main), res);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
        erts_lcnt_trylock(&(lock->lcnt_msgq), res);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	erts_lcnt_trylock(&(lock->lcnt_link), res);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	erts_lcnt_trylock(&(lock->lcnt_status), res);
    }
    }
}


void erts_lcnt_enable_proc_lock_count(int enable)
{
    int i, max = erts_ptab_max(&erts_proc);

    for (i = 0; i < max; ++i) {
	Process* p = erts_pix2proc(i);
	if (p) {
	    if (enable) {
		if (!ERTS_LCNT_LOCK_TYPE(&(p->lock.lcnt_main))) {
		    erts_lcnt_proc_lock_init(p);
		}
	    } else {
		if (ERTS_LCNT_LOCK_TYPE(&(p->lock.lcnt_main))) {
		    erts_lcnt_proc_lock_destroy(p);
		}
	    }
	}
    }
}

#endif /* ifdef ERTS_ENABLE_LOCK_COUNT */


/* --- Process lock checking ----------------------------------------------- */

#ifdef ERTS_ENABLE_LOCK_CHECK

#if ERTS_PROC_LOCK_OWN_IMPL

void
erts_proc_lc_lock(Process *p, ErtsProcLocks locks, char *file, unsigned int line)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->common.id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_lock_x(&lck,file,line);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_lock_x(&lck,file,line);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_lock_x(&lck,file,line);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_lock_x(&lck,file,line);
    }
}

void
erts_proc_lc_trylock(Process *p, ErtsProcLocks locks, int locked,
		     char* file, unsigned int line)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->common.id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_trylock_x(locked, &lck, file, line);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_trylock_x(locked, &lck, file, line);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_trylock_x(locked, &lck, file, line);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_trylock_x(locked, &lck, file, line);
    }
}

void
erts_proc_lc_unlock(Process *p, ErtsProcLocks locks)
{
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->common.id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_unlock(&lck);
    }
}

#endif /* ERTS_PROC_LOCK_OWN_IMPL */

void
erts_proc_lc_might_unlock(Process *p, ErtsProcLocks locks)
{
#if ERTS_PROC_LOCK_OWN_IMPL
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->common.id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_might_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_might_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_might_unlock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_might_unlock(&lck);
    }
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    if (locks & ERTS_PROC_LOCK_MAIN)
	erts_lc_might_unlock(&p->lock.main.lc);
    if (locks & ERTS_PROC_LOCK_LINK)
	erts_lc_might_unlock(&p->lock.link.lc);
    if (locks & ERTS_PROC_LOCK_MSGQ)
	erts_lc_might_unlock(&p->lock.msgq.lc);
    if (locks & ERTS_PROC_LOCK_STATUS)
	erts_lc_might_unlock(&p->lock.status.lc);
#endif
}

void
erts_proc_lc_require_lock(Process *p, ErtsProcLocks locks, char *file,
			  unsigned int line)
{
#if ERTS_PROC_LOCK_OWN_IMPL
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->common.id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_require_lock(&lck, file, line);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_require_lock(&lck, file, line);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_require_lock(&lck, file, line);
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_require_lock(&lck, file, line);
    }
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    if (locks & ERTS_PROC_LOCK_MAIN)
	erts_lc_require_lock(&p->lock.main.lc, file, line);
    if (locks & ERTS_PROC_LOCK_LINK)
	erts_lc_require_lock(&p->lock.link.lc, file, line);
    if (locks & ERTS_PROC_LOCK_MSGQ)
	erts_lc_require_lock(&p->lock.msgq.lc, file, line);
    if (locks & ERTS_PROC_LOCK_STATUS)
	erts_lc_require_lock(&p->lock.status.lc, file, line);
#endif
}

void
erts_proc_lc_unrequire_lock(Process *p, ErtsProcLocks locks)
{
#if ERTS_PROC_LOCK_OWN_IMPL
    erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					   p->common.id,
					   ERTS_LC_FLG_LT_PROCLOCK);
    if (locks & ERTS_PROC_LOCK_STATUS) {
	lck.id = lc_id.proc_lock_status;
	erts_lc_unrequire_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	lck.id = lc_id.proc_lock_msgq;
	erts_lc_unrequire_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	lck.id = lc_id.proc_lock_link;
	erts_lc_unrequire_lock(&lck);
    }
    if (locks & ERTS_PROC_LOCK_MAIN) {
	lck.id = lc_id.proc_lock_main;
	erts_lc_unrequire_lock(&lck);
    }
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    if (locks & ERTS_PROC_LOCK_MAIN)
	erts_lc_unrequire_lock(&p->lock.main.lc);
    if (locks & ERTS_PROC_LOCK_LINK)
	erts_lc_unrequire_lock(&p->lock.link.lc);
    if (locks & ERTS_PROC_LOCK_MSGQ)
	erts_lc_unrequire_lock(&p->lock.msgq.lc);
    if (locks & ERTS_PROC_LOCK_STATUS)
	erts_lc_unrequire_lock(&p->lock.status.lc);
#endif
}

#if ERTS_PROC_LOCK_OWN_IMPL

int
erts_proc_lc_trylock_force_busy(Process *p, ErtsProcLocks locks)
{
    if (locks & ERTS_PROC_LOCKS_ALL) {
	erts_lc_lock_t lck = ERTS_LC_LOCK_INIT(-1,
					       p->common.id,
					       ERTS_LC_FLG_LT_PROCLOCK);

	if (locks & ERTS_PROC_LOCK_MAIN)
	    lck.id = lc_id.proc_lock_main;
	else if (locks & ERTS_PROC_LOCK_LINK)
	    lck.id = lc_id.proc_lock_link;
	else if (locks & ERTS_PROC_LOCK_MSGQ)
	    lck.id = lc_id.proc_lock_msgq;
	else if (locks & ERTS_PROC_LOCK_STATUS)
	    lck.id = lc_id.proc_lock_status;
	else
	    erts_lc_fail("Unknown proc lock found");

	return erts_lc_trylock_force_busy(&lck);
    }
    return 0;
}

#endif /* ERTS_PROC_LOCK_OWN_IMPL */

void erts_proc_lc_chk_only_proc_main(Process *p)
{
#if ERTS_PROC_LOCK_OWN_IMPL
    erts_lc_lock_t proc_main = ERTS_LC_LOCK_INIT(lc_id.proc_lock_main,
						 p->common.id,
						 ERTS_LC_FLG_LT_PROCLOCK);
    erts_lc_check_exact(&proc_main, 1);
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    erts_lc_check_exact(&p->lock.main.lc, 1);
#endif
}

#if ERTS_PROC_LOCK_OWN_IMPL
#define ERTS_PROC_LC_EMPTY_LOCK_INIT \
  ERTS_LC_LOCK_INIT(-1, THE_NON_VALUE, ERTS_LC_FLG_LT_PROCLOCK)
#endif /* ERTS_PROC_LOCK_OWN_IMPL */

void
erts_proc_lc_chk_have_proc_locks(Process *p, ErtsProcLocks locks)
{
    int have_locks_len = 0;
#if ERTS_PROC_LOCK_OWN_IMPL
    erts_lc_lock_t have_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT};
    if (locks & ERTS_PROC_LOCK_MAIN) {
	have_locks[have_locks_len].id = lc_id.proc_lock_main;
	have_locks[have_locks_len++].extra = p->common.id;
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	have_locks[have_locks_len].id = lc_id.proc_lock_link;
	have_locks[have_locks_len++].extra = p->common.id;
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	have_locks[have_locks_len].id = lc_id.proc_lock_msgq;
	have_locks[have_locks_len++].extra = p->common.id;
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	have_locks[have_locks_len].id = lc_id.proc_lock_status;
	have_locks[have_locks_len++].extra = p->common.id;
    }
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    erts_lc_lock_t have_locks[4];
    if (locks & ERTS_PROC_LOCK_MAIN)
	have_locks[have_locks_len++] = p->lock.main.lc;
    if (locks & ERTS_PROC_LOCK_LINK)
	have_locks[have_locks_len++] = p->lock.link.lc;
    if (locks & ERTS_PROC_LOCK_MSGQ)
	have_locks[have_locks_len++] = p->lock.msgq.lc;
    if (locks & ERTS_PROC_LOCK_STATUS)
	have_locks[have_locks_len++] = p->lock.status.lc;
#endif
    erts_lc_check(have_locks, have_locks_len, NULL, 0);
}

void
erts_proc_lc_chk_proc_locks(Process *p, ErtsProcLocks locks)
{
    int have_locks_len = 0;
    int have_not_locks_len = 0;
#if ERTS_PROC_LOCK_OWN_IMPL
    erts_lc_lock_t have_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT,
				    ERTS_PROC_LC_EMPTY_LOCK_INIT};
    erts_lc_lock_t have_not_locks[4] = {ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT,
					ERTS_PROC_LC_EMPTY_LOCK_INIT};

    if (locks & ERTS_PROC_LOCK_MAIN) {
	have_locks[have_locks_len].id = lc_id.proc_lock_main;
	have_locks[have_locks_len++].extra = p->common.id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_main;
	have_not_locks[have_not_locks_len++].extra = p->common.id;
    }
    if (locks & ERTS_PROC_LOCK_LINK) {
	have_locks[have_locks_len].id = lc_id.proc_lock_link;
	have_locks[have_locks_len++].extra = p->common.id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_link;
	have_not_locks[have_not_locks_len++].extra = p->common.id;
    }
    if (locks & ERTS_PROC_LOCK_MSGQ) {
	have_locks[have_locks_len].id = lc_id.proc_lock_msgq;
	have_locks[have_locks_len++].extra = p->common.id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_msgq;
	have_not_locks[have_not_locks_len++].extra = p->common.id;
    }
    if (locks & ERTS_PROC_LOCK_STATUS) {
	have_locks[have_locks_len].id = lc_id.proc_lock_status;
	have_locks[have_locks_len++].extra = p->common.id;
    }
    else {
	have_not_locks[have_not_locks_len].id = lc_id.proc_lock_status;
	have_not_locks[have_not_locks_len++].extra = p->common.id;
    }
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    erts_lc_lock_t have_locks[4];
    erts_lc_lock_t have_not_locks[4];

    if (locks & ERTS_PROC_LOCK_MAIN)
	have_locks[have_locks_len++] = p->lock.main.lc;
    else
	have_not_locks[have_not_locks_len++] = p->lock.main.lc;
    if (locks & ERTS_PROC_LOCK_LINK)
	have_locks[have_locks_len++] = p->lock.link.lc;
    else
	have_not_locks[have_not_locks_len++] = p->lock.link.lc;
    if (locks & ERTS_PROC_LOCK_MSGQ)
	have_locks[have_locks_len++] = p->lock.msgq.lc;
    else
	have_not_locks[have_not_locks_len++] = p->lock.msgq.lc;
    if (locks & ERTS_PROC_LOCK_STATUS)
	have_locks[have_locks_len++] = p->lock.status.lc;
    else
	have_not_locks[have_not_locks_len++] = p->lock.status.lc;
#endif

    erts_lc_check(have_locks, have_locks_len,
		  have_not_locks, have_not_locks_len);
}

ErtsProcLocks
erts_proc_lc_my_proc_locks(Process *p)
{
    int resv[4];
    ErtsProcLocks res = 0;
#if ERTS_PROC_LOCK_OWN_IMPL
    erts_lc_lock_t locks[4] = {ERTS_LC_LOCK_INIT(lc_id.proc_lock_main,
						 p->common.id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_link,
						 p->common.id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_msgq,
						 p->common.id,
						 ERTS_LC_FLG_LT_PROCLOCK),
			       ERTS_LC_LOCK_INIT(lc_id.proc_lock_status,
						 p->common.id,
						 ERTS_LC_FLG_LT_PROCLOCK)};
#elif ERTS_PROC_LOCK_RAW_MUTEX_IMPL
    erts_lc_lock_t locks[4] = {p->lock.main.lc,
			       p->lock.link.lc,
			       p->lock.msgq.lc,
			       p->lock.status.lc};
#endif

    erts_lc_have_locks(resv, locks, 4);
    if (resv[0])
	res |= ERTS_PROC_LOCK_MAIN;
    if (resv[1])
	res |= ERTS_PROC_LOCK_LINK;
    if (resv[2])
	res |= ERTS_PROC_LOCK_MSGQ;
    if (resv[3])
	res |= ERTS_PROC_LOCK_STATUS;

    return res;
}

void
erts_proc_lc_chk_no_proc_locks(char *file, int line)
{
    int resv[4];
    int ids[4] = {lc_id.proc_lock_main,
		  lc_id.proc_lock_link,
		  lc_id.proc_lock_msgq,
		  lc_id.proc_lock_status};
    erts_lc_have_lock_ids(resv, ids, 4);
    if (!ERTS_IS_CRASH_DUMPING && (resv[0] || resv[1] || resv[2] || resv[3])) {
	erts_lc_fail("%s:%d: Thread has process locks locked when expected "
		     "not to have any process locks locked",
		     file, line);
    }
}

#endif /* #ifdef ERTS_ENABLE_LOCK_CHECK */

#endif /* ERTS_SMP */
