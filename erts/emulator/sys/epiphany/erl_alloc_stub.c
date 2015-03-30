/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2013. All Rights Reserved.
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
 * Description:	Management of memory allocators.
 *
 * Author: 	Rickard Green
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#define ERTS_ALLOC_C__
#define ERTS_ALC_INTERNAL__
#include "sys.h"
#define ERL_THREADS_EMU_INTERNAL__
#include "erl_threads.h"
#include "global.h"
#include "erl_db.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_instrument.h"
#include "erl_mseg.h"
#include "erl_monitors.h"
#include "erl_bif_timer.h"
#include "erl_cpu_topology.h"
#include "erl_thr_queue.h"
#if defined(ERTS_ALC_T_DRV_SEL_D_STATE) || defined(ERTS_ALC_T_DRV_EV_D_STATE)
#include "erl_check_io.h"
#endif

__decl_noreturn void
erts_alc_fatal_error(int error, int func, ErtsAlcType_t n, ...)
{
    switch (error) {
    case ERTS_ALC_E_NOTSUP: {
	char *op_str;
	switch (func) {
	case ERTS_ALC_O_ALLOC:		op_str = "alloc";	break;
	case ERTS_ALC_O_REALLOC:	op_str = "realloc";	break;
	case ERTS_ALC_O_FREE:		op_str = "free";	break;
	default:			op_str = "UNKNOWN";	break;
	}
	erl_exit(ERTS_ABORT_EXIT,
		 "%s operation not supported (memory type: %d)\n",
		 op_str, n);
	break;
    }
    case ERTS_ALC_E_NOMEM: {
	Uint size;
	va_list argp;
	char *op = func == ERTS_ALC_O_REALLOC ? "reallocate" : "allocate";
	

	va_start(argp, n);
	size = va_arg(argp, Uint);
	va_end(argp);
	erl_exit(1,
		 "Cannot %s %lu bytes of memory (of type %d, thread %d).\n",
		 op, size, n, ERTS_ALC_GET_THR_IX());
	break;
    }
    case ERTS_ALC_E_NOALLCTR:
	erl_exit(ERTS_ABORT_EXIT,
		 "erts_alloc: Unknown allocator type: %d\n",
		 ERTS_ALC_T2A(ERTS_ALC_N2T(n)));
	break;
    default:
	erl_exit(ERTS_ABORT_EXIT, "erts_alloc: Unknown error: %d\n", error);
	break;
    }
}

__decl_noreturn void
erts_alloc_enomem(ErtsAlcType_t type, Uint size)
{
    erts_alloc_n_enomem(ERTS_ALC_T2N(type), size);
}

__decl_noreturn void
erts_alloc_n_enomem(ErtsAlcType_t n, Uint size)
{
    erts_alc_fatal_error(ERTS_ALC_E_NOMEM, ERTS_ALC_O_ALLOC, n, size);
}

__decl_noreturn void
erts_realloc_enomem(ErtsAlcType_t type, void *ptr, Uint size)
{
    erts_realloc_n_enomem(ERTS_ALC_T2N(type), ptr, size);
}

__decl_noreturn void
erts_realloc_n_enomem(ErtsAlcType_t n, void *ptr, Uint size)
{
    erts_alc_fatal_error(ERTS_ALC_E_NOMEM, ERTS_ALC_O_REALLOC, n, size);
}
