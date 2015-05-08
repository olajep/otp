/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2013. All Rights Reserved.
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
#include "config.h"
#endif
#include "sys.h"
#include "beam_catches.h"
#include "global.h"
#include "slave.h"

#ifdef ERTS_SLAVE_EMU_ENABLED
#  include "slave_syms.h"
#endif

/* R14B04 has about 380 catches when starting erlang */
#define DEFAULT_TABSIZE (1024)
typedef struct {
    BeamInstr *cp;
    unsigned cdr;
} SLAVE_SHARED_DATA beam_catch_t;

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

struct bc_pool {
    int free_list;
    unsigned high_mark;
    unsigned tabsize;
    beam_catch_t *beam_catches;
    /* 
     * Note that the 'beam_catches' area is shared by pools. Used slots
     * are readonly as long as the module is not purgable. The free-list is
     * protected by the code_ix lock.
     */

    IF_DEBUG(int is_staging;)
} SLAVE_SHARED_DATA;

#ifndef ERTS_SLAVE
static
#endif
struct bc_pool bccix[ERTS_NUM_CODE_IX];

#ifdef ERTS_SLAVE_EMU_ENABLED
static int slave_initialized = 0;
static struct bc_pool *slave_bccix = (void*)SLAVE_SYM_bccix;
#endif

BeamInstr *beam_catches_car(unsigned i)
{
    struct bc_pool* p = &bccix[erts_active_code_ix()];

    if (i >= p->tabsize) {
	erl_exit(1, "beam_catches_car: index %#x is out of range\r\n", i);
    }
    return p->beam_catches[i].cp;
}

#ifndef ERTS_SLAVE
static void
init_pool(struct bc_pool pool[ERTS_NUM_CODE_IX], ErtsAlcType_t alctr)
{
    int i;

    pool[0].tabsize   = DEFAULT_TABSIZE;
    pool[0].free_list = -1;
    pool[0].high_mark = 0;
    pool[0].beam_catches = erts_alloc(alctr,
				      sizeof(beam_catch_t)*DEFAULT_TABSIZE);
    IF_DEBUG(pool[0].is_staging = 0);
    for (i=1; i<ERTS_NUM_CODE_IX; i++) {
	pool[i] = pool[i-1];
    }
}

void beam_catches_init(void)
{
#ifndef ERTS_SLAVE
    init_pool(bccix, ERTS_ALC_T_CODE);
    /* For initial load: */
    IF_DEBUG(bccix[erts_staging_code_ix()].is_staging = 1);
#endif
}

#ifdef ERTS_SLAVE_EMU_ENABLED
void slave_catches_init(void)
{
    init_pool(slave_bccix, ERTS_ALC_T_SLAVE_CODE);
    slave_initialized = 1;
}
#endif

static void gc_old_vec(struct bc_pool pool[ERTS_NUM_CODE_IX],
		       ErtsAlcType_t alctr, beam_catch_t* vec)
{
    int i;
    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	if (pool[i].beam_catches == vec) {
	    return;
	}
    }
    erts_free(alctr, vec);
}


static void pool_catches_start_staging(struct bc_pool pool[ERTS_NUM_CODE_IX],
				       ErtsAlcType_t alctr)
{
    ErtsCodeIndex dst = erts_staging_code_ix();
    ErtsCodeIndex src = erts_active_code_ix();
    beam_catch_t* prev_vec = pool[dst].beam_catches;

    ASSERT(!pool[src].is_staging && !pool[dst].is_staging);

    pool[dst] = pool[src];
    gc_old_vec(pool, alctr, prev_vec);
    IF_DEBUG(pool[dst].is_staging = 1);
}

void beam_catches_start_staging(void)
{
    pool_catches_start_staging(bccix, ERTS_ALC_T_CODE);
#ifdef ERTS_SLAVE_EMU_ENABLED
    if (slave_initialized)
	pool_catches_start_staging(slave_bccix, ERTS_ALC_T_SLAVE_CODE);
#endif
}

void beam_catches_end_staging(int commit)
{
    IF_DEBUG(bccix[erts_staging_code_ix()].is_staging = 0);
#if defined(ERTS_SLAVE_EMU_ENABLED) && defined(DEBUG)
    if (slave_initialized)
	slave_bccix[erts_staging_code_ix()].is_staging = 0;
#endif
}

static unsigned
pool_catches_cons(struct bc_pool pool[ERTS_NUM_CODE_IX], ErtsAlcType_t alctr,
		  BeamInstr *cp, unsigned cdr)
{
    int i;
    struct bc_pool* p = &pool[erts_staging_code_ix()];

    ASSERT(p->is_staging);
    /*
     * Allocate from free_list while it is non-empty.
     * If free_list is empty, allocate at high_mark.
     */
    if (p->free_list >= 0) {
	i = p->free_list;
	p->free_list = p->beam_catches[i].cdr;
    }
    else {
	if (p->high_mark >= p->tabsize) {
	    /* No free slots and table is full: realloc table */
	    beam_catch_t* prev_vec = p->beam_catches;
	    unsigned newsize = p->tabsize*2;

	    p->beam_catches = erts_alloc(alctr, newsize*sizeof(beam_catch_t));
	    sys_memcpy(p->beam_catches, prev_vec,
		       p->tabsize*sizeof(beam_catch_t));
	    gc_old_vec(pool, alctr, prev_vec);
	    p->tabsize = newsize;
	}
	i = p->high_mark++;
    }

    p->beam_catches[i].cp  = cp;
    p->beam_catches[i].cdr = cdr;

    return i;
}

unsigned beam_catches_cons(BeamInstr* cp, unsigned cdr)
{
    return pool_catches_cons(bccix, ERTS_ALC_T_CODE, cp, cdr);
}

#ifdef ERTS_SLAVE_EMU_ENABLED
unsigned slave_catches_cons(BeamInstr* cp, unsigned cdr)
{
    ASSERT(slave_initialized);
    return pool_catches_cons(slave_bccix, ERTS_ALC_T_SLAVE_CODE, cp, cdr);
}
#endif

static void pool_catches_delmod(struct bc_pool pool[ERTS_NUM_CODE_IX],
				unsigned head, BeamInstr *code,
				unsigned code_bytes, ErtsCodeIndex code_ix)
{
    struct bc_pool* p = &pool[code_ix];
    unsigned i, cdr;

    ASSERT((code_ix == erts_active_code_ix()) != pool[erts_staging_code_ix()].is_staging);
    for(i = head; i != (unsigned)-1;) {
	if (i >= p->tabsize) {
	    erl_exit(1, "pool_catches_delmod: index %#x is out of range\r\n", i);
	}
	if( (char*)p->beam_catches[i].cp - (char*)code >= code_bytes ) {
	    erl_exit(1,
		     "pool_catches_delmod: item %#x has cp %p which is not "
		     "in module's range [%p,%p[\r\n",
		     i, p->beam_catches[i].cp, code, ((char*)code + code_bytes));
	}
	p->beam_catches[i].cp = 0;
	cdr = p->beam_catches[i].cdr;
	p->beam_catches[i].cdr = p->free_list;
	p->free_list = i;
	i = cdr;
    }
}

void beam_catches_delmod(unsigned head, BeamInstr *code, unsigned code_bytes,
			 ErtsCodeIndex code_ix)
{
    pool_catches_delmod(bccix, head, code, code_bytes, code_ix);
}

#ifdef ERTS_SLAVE_EMU_ENABLED
void slave_catches_delmod(unsigned head, BeamInstr *code, unsigned code_bytes,
			  ErtsCodeIndex code_ix)
{
    ASSERT(slave_initialized);
    pool_catches_delmod(slave_bccix, head, code, code_bytes, code_ix);
}
#endif

#endif /* !defined(ERTS_SLAVE) */
