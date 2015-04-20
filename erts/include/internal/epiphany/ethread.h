/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2005-2015. All Rights Reserved.
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
 * Low-level ethread support on Adapteva Epiphany.
 */
#ifndef ETHREAD_EPIPHANY_ETHREAD_H
#define ETHREAD_EPIPHANY_ETHREAD_H

/* Atomics */
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATIVE_ATOMIC32_IMPL "epiphany"

#define ETHR_MAX_EPIPHANY_CORECOUNT 16

typedef struct {
    unsigned magic;
    volatile signed char level[ETHR_MAX_EPIPHANY_CORECOUNT];
    volatile signed char waiting[ETHR_MAX_EPIPHANY_CORECOUNT-1];
    ethr_sint32_t val;
} ethr_native_atomic32_t;

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INIT
void ethr_native_atomic32_init(ethr_native_atomic32_t *var, ethr_sint32_t val);

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADDR
static inline ethr_sint32_t *
ethr_native_atomic32_addr(ethr_native_atomic32_t *arg) {
    return &arg->val;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG
ethr_sint32_t ethr_native_atomic32_cmpxchg(ethr_native_atomic32_t *var,
					   ethr_sint32_t val,
					   ethr_sint32_t old_val);

/*
 * Epiphany provides no way to barrier, but with some assumptions, we can
 * provide that.
 *
 * We assume that all accesses that all accesses that take the same route
 * through the same mesh network (of which there are three; one for on-code
 * loads, one for on-core stores, and one for off core accesses) are strictly
 * ordered. In other words, we assume that memory accesses cannot "overtake"
 * each other on the same mesh network, and that any endpoint applies memory
 * accesses in the order they arrive.
 *
 * We also assume that loads are ordered before any succeeding accesses. This is
 * sane presuming the architecture is strictly in-order and does not speculate
 * loads or stores.
 *
 * Lastly, we assume all available off-core memory is on the east bus (which is
 * the case on the currently available Parallella boards).
 *
 * Under these assumptions, we can construct a memory barrier for all off-core
 * accesses by simply writing something to DRAM, and then spinning until we can
 * read that thing back.
 */
static inline void epiphany_dram_write_barrier(void);
#define ETHR_LoadLoad	(1 << 0)
#define ETHR_LoadStore	(1 << 1)
#define ETHR_StoreLoad	(1 << 2)
#define ETHR_StoreStore	(1 << 3)

#define ETHR_MEMBAR(B) do {				\
	__asm__ __volatile__ ("" : : : "memory");	\
	__sync_synchronize();				\
	if ((B) & ETHR_StoreLoad) {			\
	    epiphany_dram_write_barrier();		\
	}						\
    } while(0)

extern volatile char epiphany_dram_write_barrier_data[16];
static inline void epiphany_dram_write_barrier(void) {
    register unsigned coreid, index;
    register int new;
    /* We could use e-lib, but we don't want to include it everywhere. */
    __asm__("MOVFS %0,COREID;" : "=r"(coreid));
    ETHR_ASSERT((0b111100111100 & coreid) == ((32 << 6) | 8));
    index = (coreid & 0b11) | ((coreid >> 4) & 0b1100);
    new = !epiphany_dram_write_barrier_data[index];
    epiphany_dram_write_barrier_data[index] = new;
    /*
     * We tell GCC memory is clobbered so that it will not try to be smart with
     * the read.
     */
    __asm__ __volatile__ ("" : : : "memory");
    while (epiphany_dram_write_barrier_data[index] != new);
}

/* Spinlocks */
#define ETHR_HAVE_NATIVE_SPINLOCKS 1
#define ETHR_NATIVE_SPINLOCKS_REQUIRE_DESTRUCTION 1
#define ETHR_NATIVE_SPINLOCK_IMPL "epiphany"
/*
 * The epiphany mutexes /are/ spinlocks, but they need to be allocated in
 * SRAM. The mutexes are allocated from an array.
 */
typedef struct {
    char row, col;
    unsigned short ix;
} ethr_native_spinlock_t;

void ethr_native_spinlock_init(ethr_native_spinlock_t*);
int ethr_native_spinlock_destroy(ethr_native_spinlock_t*);
void ethr_native_spin_unlock(ethr_native_spinlock_t*);
int ethr_native_spin_trylock(ethr_native_spinlock_t*);
int ethr_native_spin_is_locked(ethr_native_spinlock_t*);
void ethr_native_spin_lock(ethr_native_spinlock_t*);

#endif /* ETHREAD_EPIPHANY_ETHREAD_H */
