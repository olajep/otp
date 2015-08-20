/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2012. All Rights Reserved.
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


#ifndef HIPE_STACK_H
#define HIPE_STACK_H

#include "hipe_arch.h"

#if defined(ERTS_SLAVE_EMU_ENABLED) || defined(ERTS_SLAVE)
#  include "slave.h"
#endif

/*
 * Stack descriptors.
 */

#include <stddef.h>	/* offsetof() */

struct SLAVE_SHARED_DATA sdesc {
    struct SLAVE_SHARED_DATA {
	unsigned long hvalue;	/* return address */
	struct sdesc *next;	/* hash collision chain */
    } bucket;
    unsigned int summary; /* frame size, exn handler presence flag, arity */
#ifdef DEBUG
    Eterm dbg_M, dbg_F;
    unsigned dbg_A;
#endif
    unsigned int livebits[1]; /* size depends on arch & data in summary field */
};

struct SLAVE_SHARED_DATA sdesc_with_exnra {
    unsigned long exnra;
    struct sdesc sdesc;
};

static __inline__ unsigned int sdesc_fsize(const struct sdesc *sdesc)
{
    return sdesc->summary >> 9;
}

static __inline__ unsigned int sdesc_arity(const struct sdesc *sdesc)
{
    return sdesc->summary & 0xFF;
}

static __inline__ unsigned long sdesc_exnra(const struct sdesc *sdesc)
{
    if ((sdesc->summary & (1<<8))) {
	const char *tmp;
	tmp = (const char*)sdesc - offsetof(struct sdesc_with_exnra, sdesc);
	return ((const struct sdesc_with_exnra*)tmp)->exnra;
    }
    return 0;
}

struct SLAVE_SHARED_DATA hipe_sdesc_table {
    unsigned int log2size;
    unsigned int mask;		/* INV: mask == (1 << log2size)-1 */
    unsigned int used;
    ErtsAlcType_t alctr;
    unsigned long ra_lsr_count;
    struct sdesc **bucket;
};
extern struct hipe_sdesc_table hipe_sdesc_table;

#ifndef ERTS_SLAVE
extern struct sdesc *hipe_put_sdesc(struct sdesc*);
extern void hipe_init_sdesc_table(struct sdesc*);
extern struct sdesc *hipe_decode_sdesc(Eterm);
#endif

#ifdef ERTS_SLAVE_EMU_ENABLED
extern struct hipe_sdesc_table *const hipe_slave_sdesc_table;
extern void *const slave_nbif_stack_trap_ra;
extern struct sdesc *hipe_slave_put_sdesc(struct sdesc*);
extern void hipe_slave_init_sdesc_table(struct sdesc*);
extern struct sdesc *hipe_slave_decode_sdesc(Eterm);
#endif

#if !defined(__GNUC__) || (__GNUC__ < 2) || (__GNUC__ == 2 && __GNUC_MINOR__ < 96)
#define __builtin_expect(x, expected_value) (x)
#endif
#define likely(x)	__builtin_expect((x),1)
#define unlikely(x)	__builtin_expect((x),0)

#ifdef ERTS_SLAVE_EMU_ENABLED
static __inline__ const struct sdesc *hipe_slave_find_sdesc(unsigned long ra)
{
    struct hipe_sdesc_table *const table = hipe_slave_sdesc_table;
    unsigned int i = (ra >> table->ra_lsr_count) & table->mask;
    const struct sdesc *sdesc = table->bucket[i];
    ASSERT(table->mask);
    ASSERT(sdesc);
    if (likely(sdesc->bucket.hvalue == ra))
	return sdesc;
    do {
	sdesc = sdesc->bucket.next;
	ASSERT(sdesc);
    } while (sdesc->bucket.hvalue != ra);
    return sdesc;
}
#endif

static __inline__ const struct sdesc *hipe_find_sdesc(unsigned long ra)
{
    unsigned int i = (ra >> HIPE_RA_LSR_COUNT) & hipe_sdesc_table.mask;
    const struct sdesc *sdesc = hipe_sdesc_table.bucket[i];
    ASSERT(hipe_sdesc_table.mask);
    ASSERT(sdesc);
    if (likely(sdesc->bucket.hvalue == ra))
	return sdesc;
    do {
	sdesc = sdesc->bucket.next;
	ASSERT(sdesc);
    } while (sdesc->bucket.hvalue != ra);
    return sdesc;
}

AEXTERN(void,nbif_stack_trap_ra,(void));

extern void hipe_print_nstack(Process*);
extern void hipe_find_handler(Process*);
extern void (*hipe_handle_stack_trap(Process*))(void);
extern void hipe_update_stack_trap(Process*, const struct sdesc*);
extern int hipe_fill_stacktrace(Process*, int, Eterm**);

#if 0 && defined(HIPE_NSTACK_GROWS_UP)
#define hipe_nstack_start(p)	((p)->hipe.nstack)
#define hipe_nstack_used(p)	((p)->hipe.nsp - (p)->hipe.nstack)
#endif
#if defined(HIPE_NSTACK_GROWS_DOWN)
#define hipe_nstack_start(p)	((p)->hipe.nsp)
#define hipe_nstack_used(p)	((p)->hipe.nstend - (p)->hipe.nsp)
#endif

/*
 * GC support procedures
 */
extern Eterm *fullsweep_nstack(Process *p, Eterm *n_htop);
extern void gensweep_nstack(Process *p, Eterm **ptr_old_htop, Eterm **ptr_n_htop);

#ifdef ERTS_SLAVE_EMU_ENABLED
extern void hipe_slave_update_stack_trap(Process*, const struct sdesc*);
#endif

#endif /* HIPE_STACK_H */
