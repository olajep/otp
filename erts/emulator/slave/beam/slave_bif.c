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
#include "slave_bif.h"

#ifdef DEBUG
#  include "epiphany.h"
#endif

#define HARDDEBUG 0

/* This is an "X macro" */
#define SLAVE_PROXIED_BIFS_DEFINER		\
    X(whereis_1, 1)				\
    X(send_2, 2)				\
    X(net_kernel_dflag_unicode_io_1, 1)

static Eterm
syscall_bif(Uint bif_no, Process *p, Eterm args[], int arity)
{
    struct slave_syscall_bif *cmd
	= erts_alloc(ERTS_ALC_T_TMP, sizeof(struct slave_syscall_bif));
    Eterm result;
    int i;
    ASSERT(epiphany_in_dram(cmd));

#if HARDDEBUG
    switch (arity) {
    case 0: erts_printf("Proxying BIF %d()...\n", bif_no); break;
    case 1: erts_printf("Proxying BIF %d(%T)...\n", bif_no, args[0]); break;
    case 2: erts_printf("Proxying BIF %d(%T,%T)...\n", bif_no, args[0], args[1]); break;
    case 3: erts_printf("Proxying BIF %d(%T,%T,%T)...\n", bif_no, args[0], args[1], args[2]); break;
    }
#endif

    cmd->bif_no = bif_no;
    for (i = 0; i < arity; i++) cmd->args[i] = args[i];

#define X(F) cmd->F = p->F
    SLAVE_BIF_VERBATIM_PROXIED_PROC_FIELDS_DEFINER;
#undef X

    erts_master_syscall(SLAVE_SYSCALL_BIF, cmd);

    ASSERT(epiphany_in_dram(cmd->heap));
    ASSERT(epiphany_in_dram(cmd->htop));
    ASSERT(epiphany_in_dram(cmd->hend));
    ASSERT(epiphany_in_dram(cmd->stop));

#define X(F) p->F = cmd->F
    SLAVE_BIF_VERBATIM_PROXIED_PROC_FIELDS_DEFINER;
#undef X

    result = cmd->result;
#if HARDDEBUG
    erts_printf("Got result %T\n", result);
#endif
    erts_free(ERTS_ALC_T_TMP, cmd);
    return result;
}

#define X(NAME, ARITY) \
    Eterm \
    NAME(Process *p, Eterm *args) \
    { \
	return syscall_bif(BIF_##NAME, p, args, ARITY);	\
    }
SLAVE_PROXIED_BIFS_DEFINER
#undef X
