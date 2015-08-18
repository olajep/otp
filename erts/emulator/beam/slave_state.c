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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "slave_state.h"
#include "slave_command.h" /* for struct slave */

#if defined(ERTS_SLAVE) && defined(DEBUG)
#  include "epiphany.h"
#endif

void
slave_state_swapin(Process *p, const struct slave_state *state)
{
#ifndef ERTS_SLAVE
    /* Lock the main lock so lc is happy */
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
    ASSERT(ERTS_SCHEDULER_IS_SLAVE_CMDER(erts_get_scheduler_data()));
    ASSERT(erts_get_scheduler_data()->current_process == NULL);
    erts_get_scheduler_data()->current_process = p;
    erts_get_scheduler_data()->x_reg_array
	= p->slave_host->dummy_esdp->x_reg_array;
#endif

    /* Validate that we can reach everything */
#if defined(ERTS_SLAVE) && defined(DEBUG)
    ErlHeapFragment *mbuf;
    struct erl_off_heap_header *ohh;
    ErlMessage *msg;
    for (mbuf = state->mbuf; mbuf; mbuf = mbuf->next)
	ASSERT(epiphany_in_dram(mbuf));
    for (ohh = state->off_heap.first; ohh; ohh = ohh->next)
	ASSERT(epiphany_in_dram(ohh));
    for (msg = state->msg.first; msg; msg = msg->next)
	ASSERT(epiphany_in_dram(msg));
    ASSERT(epiphany_in_dram(state->heap));
    ASSERT(epiphany_in_dram(state->htop));
    ASSERT(epiphany_in_dram(state->hend));
    ASSERT(epiphany_in_dram(state->stop));
    ASSERT(!state->dictionary || epiphany_in_dram(state->dictionary));
#endif

    erts_atomic32_read_bset_nob(&p->state, SLAVE_STATE_PSFLGS, state->state);

#define X(T, N) S(T, N, N)
#define S(T, F, A) p->A = state->F;
    SLAVE_STATE_VERBATIM_PROXIED_PROC_FIELDS_DEFINER
#undef X
#undef S
    if (state->msg_last_is_first) p->msg.last = &p->msg.first;
    if (state->msg_save_is_first) p->msg.save = &p->msg.first;
}

void
slave_state_swapout(Process *p, struct slave_state *state)
{
#define X(T, N) S(T, N, N)
#define S(T, F, A) state->F = p->A;
    SLAVE_STATE_VERBATIM_PROXIED_PROC_FIELDS_DEFINER
#undef X
#undef S

    state->state = erts_atomic32_read_nob(&p->state) & SLAVE_STATE_PSFLGS;

    state->msg_last_is_first = p->msg.last == &p->msg.first;
    state->msg_save_is_first = p->msg.save == &p->msg.first;

#ifndef ERTS_SLAVE
    if (ERTS_SCHEDULER_IS_SLAVE_CMDER(erts_get_scheduler_data())
	&& erts_get_scheduler_data()->current_process == p) {
	erts_get_scheduler_data()->current_process = NULL;
    }
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
#endif
}
