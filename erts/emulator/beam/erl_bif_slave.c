/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 1999-2015. All Rights Reserved.
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
 * BIFs belonging to the 'slave' module.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "bif.h"

#ifdef ERTS_SLAVE_EMU_ENABLED
#include "erl_slave_process.h"
#endif

BIF_RETTYPE slave_spawn_3(BIF_ALIST_3)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    ErlSpawnOpts so;
    Eterm pid;

    so.flags = 0;
    /* ETODO: Somewhere, synchronisation is missing... */
    pid = erl_create_slave_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
    if (is_non_value(pid)) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	BIF_RET(pid);
    }
#else
    BIF_RET(am_undefined);
#endif
}
