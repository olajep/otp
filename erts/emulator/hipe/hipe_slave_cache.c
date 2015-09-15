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
 */

/*
 * Simple caching of hipe-compiled functions in local memory
 */

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "hipe_slave_cache.h"
#include "hipe_stack.h" /* For the unlikely macro */
#include "slave_command.h"

#ifndef ERTS_SLAVE
#  include "slave_syms.h"
#endif

/* Tunables */
#define MAX_CACHED_FUNS 100
#define CACHE_SIZE 4096
#define CACHE_THRESH 1

#define MAX_CORES 16

typedef struct hipe_slave_cache_entry entry_table[MAX_CORES][MAX_CACHED_FUNS];

#define HARDDEBUG 0
#if HARDDEBUG
#  define HTRACE(...) erts_fprintf(stderr, __VA_ARGS__)
#else
#  define HTRACE(...) (void)0
#endif

#ifdef ERTS_SLAVE
#include "epiphany.h"

unsigned hipe_cached_count[MAX_CORES];
entry_table hipe_slave_cache_entries;

EPIPHANY_SRAM_DATA static int remaining_cache_bytes = CACHE_SIZE;
EPIPHANY_SRAM_HEAP static char hipe_cache_area[CACHE_SIZE];

void * __attribute__((section(".local_text"))) /* EPIPHANY_SRAM_HEAP */
hipe_cold_call_c(void *optr, UWord corix) {
     struct fun_entrypoint *ptr = optr - offsetof(struct fun_entrypoint, table);
#ifdef DEBUG
     HTRACE("Calling %T:%T/%d (%p) through cold_call, coreix=%u\n",
		  ptr->dbg_M, ptr->dbg_F, ptr->dbg_A,
		  ptr->cold_address, corix);
#else
     HTRACE("Calling %p through cold_call, coreix=%u\n",
		  ptr->cold_address, corix);
#endif

     corix = corix / 8;
     ptr->table[corix].count++;
     if (unlikely(remaining_cache_bytes >= ptr->size
		  && ptr->table[corix].count > CACHE_THRESH
		  && hipe_cached_count[corix] < MAX_CACHED_FUNS
		  )) {
	 int i;
	 void *hot_address = hipe_cache_area
	     + (CACHE_SIZE - remaining_cache_bytes);
	 unsigned entry_ix = hipe_cached_count[corix]++;
#ifdef DEBUG
	 erts_fprintf(stderr, "Caching %T:%T/%d (%p) at %p+%#x\n",
		      ptr->dbg_M, ptr->dbg_F, ptr->dbg_A,
		      ptr->cold_address, hot_address, ptr->size);
#else
	 erts_fprintf(stderr, "Caching %p at %p+%#x\n",
		      ptr->cold_address, hot_address, ptr->size);
#endif
	 memcpy(hot_address, ptr->cold_address, ptr->size);
	 ptr->table[corix].address = hot_address;
	 hipe_slave_cache_entries[corix][entry_ix].hot_address = hot_address;
	 hipe_slave_cache_entries[corix][entry_ix].entry_info = ptr;
	 remaining_cache_bytes -= ptr->size;

	 /*
	  * ETODO: patch export entries of cached funs to call cache directly
	  * (complicates partial eviction)
	  */
	 HTRACE("Done! %d bytes remaining. Calling local code now...\n",
		remaining_cache_bytes);
	 return hot_address;
     }
  do_cold:
    return ptr->cold_address;
}

void
hipe_slave_cache_empty(void)
{
    int corix = epiphany_coreno();
    unsigned i;
    for (i = 0; i < hipe_cached_count[corix]; i++) {
	struct fun_entrypoint *ptr = hipe_slave_cache_entries[corix][i].entry_info;
	ptr->table[corix].address = hipe_cold_call;
    }
    remaining_cache_bytes = CACHE_SIZE;
    hipe_cached_count[corix] = 0;
}
#endif /* ERTS_SLAVE */

const struct sdesc *hipe_cache_find_sdesc(unsigned long ra)
{
#ifdef ERTS_SLAVE
    int corix = epiphany_coreno();
    struct hipe_slave_cache_entry *entries
	= hipe_slave_cache_entries[corix];
    unsigned count = hipe_cached_count[corix];
#else
    Process *c_p = erts_get_current_process();
    int corix = c_p->slave_host->no;
    struct hipe_slave_cache_entry *entries
	= (*(entry_table*)SLAVE_SYM_hipe_slave_cache_entries)[corix];
    unsigned count = ((unsigned *)SLAVE_SYM_hipe_cached_count)[corix];
#endif
    unsigned min = 0, max = count-1;
    struct hipe_slave_cache_entry *entry;
    struct fun_entrypoint *info;
    long unsigned offset;

    /* Find cache entry */
    HTRACE("Bisecting to find hot address %#x on core %d\n", ra, corix);
    while (1) {
	unsigned i = (max + min) / 2;
	ASSERT(max >= min);
	entry = &entries[i];
	info = entry->entry_info;
	if ((unsigned long)entry->hot_address > ra) {
	    max = i - 1;
	    HTRACE("Before %p at %d\n", entry->hot_address, i);
	    ASSERT(i > 0);
	} else if ((unsigned long)entry->hot_address + info->size <= ra) {
	    min = i + 1;
	    HTRACE("After %p at %d\n", entry->hot_address + info->size, i);
	} else {
	    HTRACE("Found cache entry "
#ifdef DEBUG
		   "(%T:%T/%d) "
#endif
		   "between %p and %p\n",
#ifdef DEBUG
		   info->dbg_M, info->dbg_F, info->dbg_A,
#endif
		   entry->hot_address, entry->hot_address + info->size);
	    break;
	}
    }

    /* Find stack descriptor */
    min = 0;
    max = info->sdesc_count-1;
    offset = ra - (long unsigned)entry->hot_address;
    HTRACE("Bisecting to find sdesc for offset %#x\n", offset);
    while (1) {
	unsigned i = (max + min) / 2;
	const struct sdesc *sdesc = info->sdescs[i];
	unsigned long expected_offset = sdesc->bucket.hvalue
	    - (unsigned long)info->cold_address;
	ASSERT(max >= min);
	if (offset < expected_offset) {
	    max = i - 1;
	    HTRACE("Before %#x at %d\n", expected_offset, i);
	    ASSERT(i > 0);
	} else if (offset > expected_offset) {
	    min = i + 1;
	    HTRACE("After %#x at %d\n", expected_offset, i);
	} else {
	    HTRACE("Found at %#x at %d\n", expected_offset, i);
	    return sdesc;
	}
    }
}
