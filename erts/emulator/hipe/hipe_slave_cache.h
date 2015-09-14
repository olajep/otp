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

#ifndef __HIPE_SLAVE_CACHE_H__
#define __HIPE_SLAVE_CACHE_H__

#include "sys.h"
#include "erl_process.h"
#include "slave.h"
#include "hipe_slave.h"

struct SLAVE_SHARED_DATA fun_entrypoint_table_entry {
    void *address;
    Sint  count;
};

struct SLAVE_SHARED_DATA fun_entrypoint {
       UWord code[4];
       void *cold_address;
       Uint  size;
       struct sdesc **sdescs;
       Uint  sdesc_count;
#ifdef DEBUG
       Eterm dbg_M, dbg_F;
       Uint  dbg_A;
#endif
       struct fun_entrypoint_table_entry table[];
};

struct SLAVE_SHARED_DATA hipe_slave_cache_entry {
    void *hot_address;
    struct fun_entrypoint *entry_info;
};

#ifdef ERTS_SLAVE
#define HIPE_SLAVE_CACHE_MAX_CORES 16
AEXTERN(void,hipe_cold_call,(void));
extern void *hipe_cold_call_c(void *table_pointer, UWord corix);

extern void hipe_slave_cache_empty(void);
#endif

#endif /* !__HIPE_SLAVE_CACHE_H__ */
