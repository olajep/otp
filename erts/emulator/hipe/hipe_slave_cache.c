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
#include "epiphany.h"

/* Tunables */
#define MAX_CACHED_FUNS 100
#define CACHE_SIZE 4096
#define CACHE_THRESH 1

#define MAX_CORES 16

EPIPHANY_SRAM_DATA int cached_count;
struct hipe_slave_cache_entry
hipe_slave_cache_entries[MAX_CORES][MAX_CACHED_FUNS];

EPIPHANY_SRAM_DATA static int remaining_cache_bytes = CACHE_SIZE;
EPIPHANY_SRAM_HEAP static char cache[CACHE_SIZE];

void * __attribute__((section(".local_text"))) /* EPIPHANY_SRAM_HEAP */
hipe_cold_call_c(void *optr, UWord corix) {
     struct fun_entrypoint *ptr = optr - offsetof(struct fun_entrypoint, table);
     if (epiphany_coreno() != 0)
	 goto do_cold;

     /* Short-circuit when cache is full */
     if (cached_count == MAX_CACHED_FUNS) goto do_cold;
     corix = corix / 8;
     ptr->table[corix].count++;
     if (unlikely(remaining_cache_bytes >= ptr->size
		  && ptr->table[corix].count > CACHE_THRESH)) {
	 int i;
	 void *hot_address = cache + (CACHE_SIZE - remaining_cache_bytes);
	 memcpy(hot_address, ptr->cold_address, ptr->size);
	 ptr->table[corix].address = hot_address;
	 hipe_slave_cache_entries[corix][cached_count].hot_address = hot_address;
	 hipe_slave_cache_entries[corix][cached_count].entry_info = ptr;
	 remaining_cache_bytes -= ptr->size;

	 /*
	  * ETODO: patch export entries of cached funs to call cache directly
	  * (complicates partial eviction)
	  */
	 return hot_address;
     }
  do_cold:
    return ptr->cold_address;
}

void
hipe_slave_cache_empty(void)
{
    int corix = epiphany_coreno();
    int i;
    for (i = 0; i < cached_count; i++) {
	struct fun_entrypoint *ptr = hipe_slave_cache_entries[corix][i].entry_info;
	ptr->table[corix].address = hipe_cold_call;
    }
    remaining_cache_bytes = CACHE_SIZE;
    cached_count = 0;
}
