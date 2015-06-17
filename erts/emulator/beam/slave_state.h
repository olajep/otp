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
 *
 */

#ifndef ERL_SLAVE_STATE_H__
#define ERL_SLAVE_STATE_H__

#include "erl_process.h"
#include "slave.h"

/* This is the "X macro" pattern */
#define SLAVE_STATE_VERBATIM_PROXIED_PROC_FIELDS_DEFINER \
    X(ErlMessageQueue, msg)				 \
    X(ErlOffHeap, off_heap)				 \
    X(ErlHeapFragment*, mbuf)				 \
    X(Uint, mbuf_sz)					 \
    X(Eterm*, heap)					 \
    X(Eterm*, htop)					 \
    X(Eterm*, hend)					 \
    X(Eterm*, stop)					 \
    X(Uint, heap_sz)					 \
    X(BeamInstr*, i)					 \
    X(ProcDict*, dictionary)				 \
    X(Uint, freason)					 \
    X(Eterm, fvalue)					 \
    X(Eterm, ftrace)					 \
    X(Sint, fcalls)

#define SLAVE_STATE_PSFLGS ERTS_PSFLG_TRAP_EXIT

struct slave_state {
#define X(T, N) T N;
    SLAVE_STATE_VERBATIM_PROXIED_PROC_FIELDS_DEFINER
#undef X
    /* These are boolean flags. To not screw up alignment, we use int for
     * them. */
    int msg_last_is_first, msg_save_is_first;
    erts_aint32_t state;
} SLAVE_SHARED_DATA;

void slave_state_swapin(Process *p, const struct slave_state *state);
void slave_state_swapout(Process *p, struct slave_state *state);

#endif /* !ERL_SLAVE_STATE_H__ */
