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
#include "bif.h"

#ifdef DEBUG
#  include "epiphany.h"
#endif

#ifdef __epiphany__
#  define SHORTCALL __attribute__((short_call))
#else
#  define SHORTCALL
#endif

#define HARDDEBUG 0

extern BeamInstr beam_exit[];

static Eterm SHORTCALL
internal_syscall_bif(Process *p, Eterm args[], int bif_no, Uint arity)
{
    struct slave_syscall_bif *cmd
	= erts_alloc(ERTS_ALC_T_TMP, sizeof(struct slave_syscall_bif));
    Eterm result;
    int i;

#if HARDDEBUG
    if (bif_no >= 0) {
	BifEntry *bif = bif_table + bif_no;
	erts_printf("Proxying BIF %T:%T", bif->module, bif->name);
    } else
	erts_printf("Proxying primop %d", -bif_no);
    switch (arity) {
    case 0: erts_printf("()...\n"); break;
    case 1: erts_printf("(%T)...\n", args[0]); break;
    case 2: erts_printf("(%T,%T)...\n", args[0], args[1]); break;
    case 3: erts_printf("(%T,%T,%T)...\n", args[0], args[1], args[2]); break;
    }
#endif

    ASSERT(epiphany_in_dram(cmd));
    cmd->bif_no = bif_no;
    for (i = 0; i < arity; i++) cmd->args[i] = args[i];

    /*
     * freason is used to detect trap resumption, we clear it here so there will
     * be no mistakes in erts_slave_serve_bif
     */
    p->freason = EXC_NULL;

    slave_state_swapout(p, &cmd->state);

    erts_master_syscall(SLAVE_SYSCALL_BIF, cmd);

    slave_state_swapin(p, &cmd->state);

    erts_smp_atomic32_read_bor_nob(&p->state, cmd->state_flags);
    if (cmd->state_flags & ERTS_PSFLG_EXITING) {
	KILL_CATCHES(p);
	p->i = (BeamInstr *) beam_exit;
	p->pending_exit.reason = THE_NON_VALUE;
	p->pending_exit.bp = NULL;
    }

    result = cmd->result;
    ASSERT(result == THE_NON_VALUE || is_immed(result)
	   || epiphany_in_dram(ptr_val(result)));
#if HARDDEBUG
    if (result == THE_NON_VALUE) {
	erts_printf("Got error %d\n ", p->freason);
    } else {
	erts_printf("Got result (%#x) ", result);
	erts_printf("%T\n", result);
    }
#endif
    erts_free(ERTS_ALC_T_TMP, cmd);
    return result;
}

Eterm
slave_syscall_bif(Uint bif_no, Process *p, Eterm args[], int arity)
{
    return internal_syscall_bif(p, args, bif_no, arity);
}

/* This is the "X macro" pattern */
#define SLAVE_PROXIED_BIFS_DEFINER		\
    X(binary_list_to_bin, 1)			\
    X(binary_to_list, 1)			\
    X(binary_to_list, 3)			\
    X(binary_to_term, 1)			\
    X(binary_to_term, 2)			\
    X(bitstring_to_list, 1)			\
    X(demonitor, 1)				\
    X(demonitor, 2)				\
    X(epiphany_internal_spawn, 3)		\
    X(erts_debug_get_internal_state, 1)		\
    X(erts_debug_set_internal_state, 2)		\
    X(erts_internal_port_control, 3)		\
    X(erts_internal_port_command, 3)		\
    X(ets_insert, 2)				\
    X(ets_lookup, 2)				\
    X(ets_new, 2)				\
    X(ets_select, 2)				\
    X(exit, 2)					\
    X(external_size, 1)				\
    X(external_size, 2)				\
    X(file_native_name_encoding, 0)		\
    X(fun_info, 2)				\
    X(iolist_to_binary, 1)			\
    X(link, 1)					\
    X(list_to_atom, 1)				\
    X(list_to_binary, 1)			\
    X(list_to_bitstring, 1)			\
    X(make_ref, 0)				\
    X(md5, 1)					\
    X(md5_final, 1)				\
    X(md5_init, 0)				\
    X(md5_update, 2)				\
    X(monitor, 2)				\
    X(net_kernel_dflag_unicode_io, 1)		\
    X(node, 0)					\
    X(node, 1)					\
    X(nodes, 1)					\
    X(now, 0)					\
    X(open_port, 2)				\
    X(os_getenv, 1)				\
    X(port_get_data, 1)				\
    X(process_flag, 2)				\
    X(process_flag, 3)				\
    X(process_info, 2)				\
    X(send, 2)					\
    X(send, 3)					\
    X(spawn, 3)					\
    X(spawn_link, 3)				\
    X(spawn_opt, 1)				\
    X(system_info, 1)				\
    X(term_to_binary, 1)			\
    X(term_to_binary, 2)			\
    X(unicode_bin_is_7bit, 1)			\
    X(unicode_characters_to_binary, 2)		\
    X(unicode_characters_to_list, 2)		\
    X(unlink, 1)				\
    X(whereis, 1)

#define X(NAME, ARITY)							\
    Eterm								\
    NAME##_##ARITY(Process *p, Eterm *args)				\
    {									\
	return internal_syscall_bif(p, args, BIF_##NAME##_##ARITY,	\
				    ARITY);				\
    }
SLAVE_PROXIED_BIFS_DEFINER
#undef X

/* Some bif modules also contain these wrappers */
HIPE_WRAPPER_BIF_DISABLE_GC(binary_list_to_bin, 1)

#ifdef HIPE

#define X(NAME, ARITY)							\
    Eterm NAME(Process *p, Eterm *args);				\
    Eterm								\
    NAME(Process *p, Eterm *args)					\
    {									\
	return internal_syscall_bif(p, args, -SLAVE_PRIMOP_##NAME,	\
				    ARITY);				\
    }
SLAVE_PROXIED_PRIMOPS_DEFINER
#undef X

#endif
