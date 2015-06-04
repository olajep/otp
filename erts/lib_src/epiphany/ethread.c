/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2015. All Rights Reserved.
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
 * Description: Epiphany implementation of the ethread library
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"

#define ETHR_CHILD_WAIT_SPIN_COUNT 4000

#define ETHREAD_IMPL__

#include "ethread.h"
#include "ethr_internal.h"

#include <e-lib.h>
#include "epiphany.h"

#ifndef ETHR_HAVE_ETHREAD_DEFINES
#error Missing configure defines
#endif

#define HARDDEBUG 0

static ethr_tsd_key ethr_ts_event_key__;

void
ethr_compiler_barrier(void)
{
    ETHR_MEMBAR(0);
}

int
ethr_init(ethr_init_data *id)
{
    int res;

    if (!ethr_not_inited__)
	return EINVAL;

    ethr_not_inited__ = 0;

    res = ethr_init_common__(id);
    if (res != 0)
	goto error;

    res = ethr_tsd_key_create(&ethr_ts_event_key__, "ethr_ts_event_key");
    if (res != 0)
	goto error;

    return 0;
 error:
    ethr_not_inited__ = 1;
    return res;
}

int
ethr_late_init(ethr_late_init_data *id)
{
    int res = ethr_late_init_common__(id);
    if (res != 0)
	return res;
    ethr_not_completely_inited__ = 0;
    return 0;
}

int
ethr_thr_create(ethr_tid *tid, void * (*func)(void *), void *arg,
		ethr_thr_opts *opts)
{
    EPIPHANY_STUB_FUN();
}

int
ethr_thr_join(ethr_tid tid, void **res)
{
    EPIPHANY_STUB_FUN();
}

int
ethr_thr_detach(ethr_tid tid)
{
    EPIPHANY_STUB_FUN();
}

void
ethr_thr_exit(void *res)
{
    EPIPHANY_STUB_FUN();
}

ethr_tid
ethr_self(void)
{
    return e_get_coreid();
}

int
ethr_equal_tids(ethr_tid tid1, ethr_tid tid2)
{
    EPIPHANY_STUB_FUN();
}


/*
 * Thread specific events
 */

ethr_ts_event *
ethr_get_ts_event(void)
{
    EPIPHANY_STUB_FUN();
}

void
ethr_leave_ts_event(ethr_ts_event *tsep)
{
    EPIPHANY_STUB_FUN();
}

/*
 * Thread specific data
 */

int
ethr_tsd_key_create(ethr_tsd_key *keyp, char *keyname)
{
    void **buf = calloc(epiphany_workgroup_size()+1, sizeof(void*));
    if (buf) buf[epiphany_workgroup_size()] = keyname;
    *keyp = buf;
#if HARDDEBUG
    erts_printf("Creating tsd key %s for %d cores at 0x%x\n", keyname,
		epiphany_workgroup_size(), buf);
#endif
    return buf ? 0 : ENOMEM;
}

int
ethr_tsd_key_delete(ethr_tsd_key key)
{
#if HARDDEBUG
    erts_printf("Deleting tsd %s\n", key[epiphany_workgroup_size()]);
#endif
    free(key);
    return 0;
}

int
ethr_tsd_set(ethr_tsd_key key, void *value)
{
#if HARDDEBUG
    erts_printf("set tsd %s[%d] = 0x%x\n", key[epiphany_workgroup_size()],
		epiphany_coreno(), value);
#endif
    key[epiphany_coreno()] = value;
    return 0;
}

void *
ethr_tsd_get(ethr_tsd_key key)
{
#if HARDDEBUG
    erts_printf("get tsd %s[%d] = 0x%x\n", key[epiphany_workgroup_size()],
		epiphany_coreno(), key[epiphany_coreno()]);
#endif
    return key[epiphany_coreno()];
}

/* internal exports */

int ethr_set_tse__(ethr_ts_event *tsep)
{
    return ethr_tsd_set(ethr_ts_event_key__, (void *) tsep);
}

ethr_ts_event *ethr_get_tse__(void)
{
    return ethr_tsd_get(ethr_ts_event_key__);
}

/*
 * Signal functions
 */

#if ETHR_HAVE_ETHR_SIG_FUNCS

int ethr_sigmask(int how, const sigset_t *set, sigset_t *oset)
{
    EPIPHANY_STUB_FUN();
}

int ethr_sigwait(const sigset_t *set, int *sig)
{
    EPIPHANY_STUB_FUN();
}

#endif /* #if ETHR_HAVE_ETHR_SIG_FUNCS */

ETHR_IMPL_NORETURN__
ethr_abort__(void)
{
    abort();
}

#if !ETHR_DISABLE_EPIPHANY_BARRIER
volatile char epiphany_dram_write_barrier_data[16];
#endif

/* Atomics */
#if !ETHR_DISABLE_EPIPHANY_ATOMICS
#define PETERSON_MAGIC 0xBCBD1264

/*
 * The "atomics" are implemented by acquiring a Peterson's algorithm mutex. The
 * reason the SRAM mutexes are not used instead is that atomics are not freed,
 * so we'd leak and run out of mutexes.
 */
void
ethr_native_atomic32_init(ethr_native_atomic32_t *var, ethr_sint32_t val)
{
    int i;
#if HARDDEBUG
    if (!epiphany_in_dram(var))
	erts_printf("Warning, atomic in SRAM at %x\n", (unsigned)var);
#endif
    ASSERT(epiphany_sane_address(var));
    var->magic = PETERSON_MAGIC;
    for (i = 0; i < ETHR_MAX_EPIPHANY_CORECOUNT; i++) var->level[i] = -1;
    for (i = 0; i < ETHR_MAX_EPIPHANY_CORECOUNT - 1; i++) var->waiting[i] = -1;
    var->val = val;
}

static void
lock_peterson(ethr_native_atomic32_t *var) {
    int l, me = epiphany_coreno();
    ASSERT(0 <= me && me < ETHR_MAX_EPIPHANY_CORECOUNT);
    if (var->magic != PETERSON_MAGIC) {
	erts_printf("Warning, late initing atomic at 0x%x\n", (unsigned)var);
	ethr_native_atomic32_init(var, var->val);
	epiphany_backtrace();
	returning_abort();
    }

    for (l = 0; l < ETHR_MAX_EPIPHANY_CORECOUNT - 1; l++) {
	var->level[me] = l;
	var->waiting[l] = me;
	ETHR_MEMBAR(ETHR_StoreLoad);
	while (var->waiting[l] == me) {
	    int k, count = 0;
	    for (k = 0; k < ETHR_MAX_EPIPHANY_CORECOUNT; k++) {
		if (k == me) continue;
		if (var->level[k] >= l) {
		    count++;
		    break;
		}
	    }
	    if (count == 0) break;
	}
    }
}

static void
unlock_peterson(ethr_native_atomic32_t *var) {
    int me = epiphany_coreno();
    var->level[me] = -1;
}

ethr_sint32_t
ethr_native_atomic32_cmpxchg(ethr_native_atomic32_t *var,
			     ethr_sint32_t val,
			     ethr_sint32_t old_val)
{
    ethr_sint32_t read_val;
    ASSERT(epiphany_sane_address(var));
#if HARDDEBUG
    erts_printf("Locking peterson at %x\n", (unsigned)var);
#endif
    lock_peterson(var);
    read_val = var->val;
    if (read_val == old_val)
	var->val = val;

#if HARDDEBUG
    erts_printf("Unlocking peterson at %x\n", (unsigned)var);
#endif
    unlock_peterson(var);
    return read_val;
}

#endif /* !ETHR_DISABLE_EPIPHANY_ATOMICS */

/* Spinlocks */
#if !ETHR_DISABLE_EPIPHANY_SPINLOCK
#include "epiphany.h"

#define MUTEX_COUNT 16
#define MUTEX_UNLOCKED 0
#define MUTEX_UNUSED -1
static EPIPHANY_SRAM_DATA e_mutex_t mutexes[MUTEX_COUNT] = {
    [0] = MUTEX_UNLOCKED,
    [1 ... MUTEX_COUNT-1] = MUTEX_UNUSED,
};

#if defined(DEBUG) || defined(ETHR_DEBUG)
#  define SANITY_TEST_SPINLOCK(LOCK) sanity_test_spinlock(LOCK)

static int sane_spinlock(ethr_native_spinlock_t *lock) {
    unsigned origin_row, origin_col, rows, cols;
    if (!epiphany_sane_address(lock)) return 0;
    epiphany_workgroup_origin(&origin_row, &origin_col);
    epiphany_workgroup_dimens(&rows, &cols);
    if (origin_row > lock->row || lock->row >= origin_row + rows) return 0;
    if (origin_col > lock->col || lock->col >= origin_col + cols) return 0;
    if (0 > lock->ix || lock->ix >= MUTEX_COUNT) return 0;
    return 1;
}

static void sanity_test_spinlock(ethr_native_spinlock_t *lock) {
    if (!sane_spinlock(lock)) {
	if (!epiphany_sane_address(lock)) {
	    erts_printf("Spinlock at bad address 0x%x!\n", lock);
	} else {
	    erts_printf("Spinlock at 0x%x (row=%d, col=%d, ix=%d) fails sanity "
			"test!\n",
			lock, (int)lock->row, (int)lock->col, (int)lock->ix);
	}
	epiphany_backtrace();
	returning_abort();
    }
}

#else
#  define SANITY_TEST_SPINLOCK(LOCK)
#endif

static int
allocate_on_core(char row, char col)
{
    e_mutex_t *core_mutexes = e_get_global_address(row, col, mutexes);
    int i;
    e_mutex_lock(row, col, mutexes + 0);
    for (i = 1; i < MUTEX_COUNT; i++) {
	if (core_mutexes[i] == MUTEX_UNUSED) {
	    e_mutex_init(row, col, mutexes + i, NULL);
	    e_mutex_unlock(row, col, mutexes + 0);
	    return i;
	}
    }
    e_mutex_unlock(row, col, mutexes + 0);
    return -1;
}

void
ethr_native_spinlock_init(ethr_native_spinlock_t *lock)
{
    ethr_native_spinlock_t lockv;
    e_coreid_t me = e_get_coreid();
    unsigned myrow, row, mycol, col;
    int i;
#if HARDDEBUG
    if (!epiphany_in_dram(lock)) {
	/*
	 * It is unnecessary to use this allocation scheme if the address is in
	 * SRAM anyway.
	 */
	erts_printf("Warning, initing spinlock at 0x%x\n", lock);
    }
#endif
    ASSERT(epiphany_sane_address(lock));

    e_coords_from_coreid(me, &myrow, &mycol);
    col = mycol;
    row = myrow;
    if ((i = allocate_on_core(myrow, mycol)) == -1) {
	unsigned origin_row, rows, origin_col, cols;
	epiphany_workgroup_origin(&origin_row, &origin_col);
	epiphany_workgroup_dimens(&rows, &cols);
	for (row = origin_row; row < origin_row + rows && i == -1; row++) {
	    for (col = origin_col; col < origin_col + cols && i == -1; col++) {
		if (col == mycol && row == myrow) continue;
		i = allocate_on_core(row, col);
	    }
	}
    }

    if (i == -1) {
	ASSERT(!"Out of mutexes!");
	erl_exit(1, "Out of mutexes!");
    }

    lockv.row = row;
    lockv.col = col;
    lockv.ix = (unsigned short)i;

    *lock = lockv;
}

int
ethr_native_spinlock_destroy(ethr_native_spinlock_t *lock)
{
    ethr_native_spinlock_t lockv = *lock;
    volatile e_mutex_t *mtx = e_get_global_address(lockv.row, lockv.col, mutexes + lockv.ix);
    SANITY_TEST_SPINLOCK(lock);
    ASSERT(*mtx == MUTEX_UNLOCKED);
    *mtx = MUTEX_UNUSED;
    return 0;
}

void
ethr_native_spin_unlock(ethr_native_spinlock_t *lock)
{
    ethr_native_spinlock_t lockv = *lock;
    volatile e_mutex_t *mtx = e_get_global_address(lockv.row, lockv.col, mutexes + lockv.ix);
    e_coreid_t me = e_get_coreid();
    SANITY_TEST_SPINLOCK(lock);
#if HARDDEBUG
    if (*mtx != me) {
	erts_printf("Unlocking mutex at 0x%x locked by other core %x (me=%x)\n",
		    lock, *mtx, me);
	epiphany_backtrace();
    }
#endif
    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore|ETHR_StoreLoad|ETHR_StoreStore);
    e_mutex_unlock(lockv.row, lockv.col, mutexes + lockv.ix);
    /*
     * Explicit barrier against the core containing the mutex since ETHR_MEMBAR
     * only ensures DRAM writes have completed.
     */
    while (*mtx == me);
}

int
ethr_native_spin_trylock(ethr_native_spinlock_t *lock)
{
    ethr_native_spinlock_t lockv = *lock;
#if defined(DEBUG) || defined(ETHR_DEBUG)
    volatile e_mutex_t *mtx = e_get_global_address(lockv.row, lockv.col, mutexes + lockv.ix);
    SANITY_TEST_SPINLOCK(lock);
    ASSERT(*((volatile e_mutex_t*)mtx) != MUTEX_UNUSED);
#endif
    if (e_mutex_trylock(lockv.row, lockv.col, mutexes + lockv.ix)) {
	return EBUSY;
    } else {
	/*
	 * We barrier just because consumers might assume a barrier is implied
	 * in a lock.
	 */
	ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore|ETHR_StoreLoad
		    |ETHR_StoreStore);
	return 0;
    }
}

int
ethr_native_spin_is_locked(ethr_native_spinlock_t *lock)
{
    ethr_native_spinlock_t lockv = *lock;
    volatile e_mutex_t *mtx = e_get_global_address(lockv.row, lockv.col, mutexes + lockv.ix);
    SANITY_TEST_SPINLOCK(lock);
    ASSERT(*mtx != MUTEX_UNUSED);
    return *mtx == MUTEX_UNLOCKED;
}

void
ethr_native_spin_lock(ethr_native_spinlock_t *lock)
{
    ethr_native_spinlock_t lockv = *lock;
#if defined(DEBUG) || defined(ETHR_DEBUG)
    volatile e_mutex_t *mtx = e_get_global_address(lockv.row, lockv.col, mutexes + lockv.ix);
    SANITY_TEST_SPINLOCK(lock);
    ASSERT(*mtx != MUTEX_UNUSED);
#endif
    e_mutex_lock(lockv.row, lockv.col, mutexes + lockv.ix);
    /* We barrier just because consumers might assume a barrier is implied in a lock. */
    ETHR_MEMBAR(ETHR_LoadLoad|ETHR_LoadStore|ETHR_StoreLoad|ETHR_StoreStore);
}

#endif /* !ETHR_DISABLE_EPIPHANY_SPINLOCK */
