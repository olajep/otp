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

void
slave_state_swapin(Process *p, struct slave_state *state)
{
#if defined(ERTS_SMP) && !defined(ERTS_SLAVE)
    /* Lock the main lock so lc is happy */
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_MAIN);
#endif

#define X(T, F) p->F = state->F
    SLAVE_STATE_VERBATIM_PROXIED_PROC_FIELDS_DEFINER;
#undef X
}

void
slave_state_swapout(Process *p, struct slave_state *state)
{
#define X(T, F) state->F = p->F
    SLAVE_STATE_VERBATIM_PROXIED_PROC_FIELDS_DEFINER;
#undef X

#if defined(ERTS_SMP) && !defined(ERTS_SLAVE)
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_MAIN);
#endif
}
