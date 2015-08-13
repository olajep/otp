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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#define EPIPHANY_NO_ALLOC_REDIRECT
#include "sys.h"
#include "epiphany.h"
#include <e-lib.h>

static EPIPHANY_SRAM_DATA e_mutex_t alloc_mutex = 0;

static inline void
lock(void)
{
    e_mutex_lock(0, 0, &alloc_mutex);
}

static inline void
unlock(void)
{
    e_mutex_unlock(0, 0, &alloc_mutex);
}

void *
internal_malloc(size_t size)
{
    void *result;
    lock();
    result = malloc(size);
    unlock();
    ASSERT(!result || (epiphany_in_dram(result) && sys_in_slave_heap(result)));
    return result;
}

void
internal_free(void *ptr)
{
    lock();
    free(ptr);
    unlock();
}
void *
internal_calloc(size_t nmemb, size_t size)
{
    void *result;
    lock();
    result = calloc(nmemb, size);
    unlock();
    ASSERT(!result || (epiphany_in_dram(result) && sys_in_slave_heap(result)));
    return result;
}


void *
internal_realloc(void *ptr, size_t size)
{
    void *result;
    lock();
    result = realloc(ptr, size);
    unlock();
    ASSERT(!result || (epiphany_in_dram(result) && sys_in_slave_heap(result)));
    return result;
}

int
sys_in_slave_heap(void *ptr)
{
    extern char __heap_start, __heap_end;
    return (void*)&__heap_start <= ptr && ptr <= (void*)&__heap_end;
}
