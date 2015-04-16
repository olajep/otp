/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
 * I/O routines for manipulating ports.
 */

#define ERL_IO_C__

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "big.h"

#ifdef ERTS_SMP

#ifdef ERTS_ENABLE_LOCK_CHECK
int
erts_lc_is_port_locked(Port *prt)
{
    if (!prt)
	return 0;
    ERTS_SMP_LC_ASSERT(prt->lock);
    return erts_smp_lc_mtx_is_locked(prt->lock);
}
#endif

#endif /* #ifdef ERTS_SMP */
