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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "bif.h"
#include "big.h"

static erts_smp_atomic_t available_internal_state;

BIF_RETTYPE erts_debug_get_internal_state_1(BIF_ALIST_1)
{
    /*
     * NOTE: Only supposed to be used for testing, and debugging.
     */

    if (!erts_smp_atomic_read_nob(&available_internal_state)) {
	BIF_ERROR(BIF_P, EXC_UNDEF);
    }

    if (is_atom(BIF_ARG_1)) {
	if (ERTS_IS_ATOM_STR("reds_left", BIF_ARG_1)) {
	    /* Used by (emulator) */
	    BIF_RET(make_small((Uint) ERTS_BIF_REDS_LEFT(BIF_P)));
	}
    }
    /* We don't distinguish badarg and notsup */
    BIF_ERROR(BIF_P, EXC_NOTSUP);
}

BIF_RETTYPE erts_debug_set_internal_state_2(BIF_ALIST_2)
{
    /*
     * NOTE: Only supposed to be used for testing, and debugging.
     */
    if (ERTS_IS_ATOM_STR("available_internal_state", BIF_ARG_1)
	&& (BIF_ARG_2 == am_true || BIF_ARG_2 == am_false)) {
	erts_aint_t on = (erts_aint_t) (BIF_ARG_2 == am_true);
	erts_aint_t prev_on = erts_smp_atomic_xchg_nob(&available_internal_state, on);
	if (on) {
	    erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	    erts_dsprintf(dsbufp, "Process %T ", BIF_P->common.id);
	    /* if (erts_is_alive) */
	    /* 	erts_dsprintf(dsbufp, "on node %T ", erts_this_node->sysname); */
	    erts_dsprintf(dsbufp, "on slave %d ", erts_get_scheduler_data()->no - 1);
	    erts_dsprintf(dsbufp,
			  "enabled access to the emulator internal state.\n");
	    erts_dsprintf(dsbufp,
			  "NOTE: This is an erts internal test feature and "
			  "should *only* be used by OTP test-suites.\n");
	    erts_send_warning_to_logger(BIF_P->group_leader, dsbufp);
	}
	BIF_RET(prev_on ? am_true : am_false);
    }
    
    if (!erts_smp_atomic_read_nob(&available_internal_state)) {
	BIF_ERROR(BIF_P, EXC_UNDEF);
    }

    if (is_atom(BIF_ARG_1)) {
	
	if (ERTS_IS_ATOM_STR("reds_left", BIF_ARG_1)) {
	    Sint reds;
	    if (term_to_Sint(BIF_ARG_2, &reds) != 0) {
		if (0 <= reds && reds <= CONTEXT_REDS) {
		    if (!ERTS_PROC_GET_SAVED_CALLS_BUF(BIF_P))
			BIF_P->fcalls = reds;
		    else
			BIF_P->fcalls = reds - CONTEXT_REDS;
		}
		BIF_RET(am_true);
	    }
	}
    }
    /* We don't distinguish badarg and notsup */
    BIF_ERROR(BIF_P, EXC_NOTSUP);
}
