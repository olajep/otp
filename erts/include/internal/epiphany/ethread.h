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
#include <e-lib.h>

/* Atomics */
#define ETHR_HAVE_NATIVE_ATOMIC32 1
#define ETHR_NATIVE_ATOMIC32_IMPL "epiphany"
typedef ethr_sint32_t *ethr_native_atomic32_t;

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_INIT
void ethr_native_atomic32_init(ethr_native_atomic32_t *var, ethr_sint32_t val);

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_ADDR
static inline ethr_sint32_t *
ethr_native_atomic32_addr(ethr_native_atomic32_t *arg) {
    return *arg;
}

#define ETHR_HAVE_ETHR_NATIVE_ATOMIC32_CMPXCHG
ethr_sint32_t ethr_native_atomic32_cmpxchg(ethr_native_atomic32_t *var,
					   ethr_sint32_t val,
					   ethr_sint32_t old_val);

/* Epiphany provides no way to barrier, but as long as all accesses are within
 * the same 1MiB region of memory, all accesses should be sequentially
 * consistent (but this is never promised by the specification). Also, loads are
 * always ordered since there is no out-of-order execution or speculation.
 */
#define ETHR_MEMBAR(B) do {			  \
	__asm__ __volatile__ ("" : : : "memory"); \
	__sync_synchronize();			  \
    } while(0)

/* Spinlocks */
#define ETHR_HAVE_NATIVE_SPINLOCKS 1
#define ETHR_NATIVE_SPINLOCKS_REQUIRE_DESTRUCTION 1
#define ETHR_NATIVE_SPINLOCK_IMPL "epiphany"
/* The epiphany mutexes /are/ spinlocks, but they need to be allocated in
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
