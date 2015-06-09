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
 * BIFs belonging to the 'epiphany' module.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "bif.h"
#include "atom.h"

#ifdef ERTS_SLAVE_EMU_ENABLED
#  include "erl_binary.h"
#  include "beam_bp.h"

#  include "slave_process.h"
#  include "slave_load.h"
#  include "slave_module.h"
#  include "slave_io.h" /* For erts_slave_online */
#endif

#ifdef ERTS_SLAVE
#  include "epiphany.h" /* For epiphany_workgroup_size() */
#endif

#ifndef ERTS_SLAVE
BIF_RETTYPE epiphany_internal_spawn_3(BIF_ALIST_3)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    ErlSpawnOpts so;
    Eterm pid;

    so.flags = SPO_SLAVE;
    pid = erl_create_slave_process(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, &so);
    if (is_non_value(pid)) {
	BIF_ERROR(BIF_P, so.error_code);
    } else {
	BIF_RET(pid);
    }
#else
    BIF_ERROR(BIF_P, EXC_NOTSUP);
#endif
}
#endif /* !ERTS_SLAVE */

BIF_RETTYPE epiphany_boot_0(BIF_ALIST_0)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    Eterm can;
    ERTS_DECL_AM(offline);

    if (erts_slave_online) can = erts_slave_can_bootstrap();
    else can = AM_offline;
    if (can != am_yes) {
	Eterm *hp = HAlloc(BIF_P, 3);
	ASSERT(is_atom(can));
	return TUPLE2(hp, am_error, can);
    }

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD0(bif_export[BIF_epiphany_boot_0], BIF_P);
    }
    erts_slave_bootstrap();
    erts_release_code_write_permission();
    BIF_RET(am_ok);
#else
    Eterm *hp = HAlloc(BIF_P, 3);
    return TUPLE2(hp, am_error, am_disabled);
#endif
}

BIF_RETTYPE epiphany_count_0(BIF_ALIST_0)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    if (erts_slave_online) return make_small(slave_workgroup.num_cores);
    else return make_small(0);
#elif defined(ERTS_SLAVE)
    return make_small(epiphany_workgroup_size());
#else
    return make_small(0);
#endif
}

BIF_RETTYPE epiphany_prepare_loading_2(BIF_ALIST_2)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    byte* temp_alloc = NULL;
    byte* code;
    Uint sz;
    Binary* magic;
    Eterm reason;
    Eterm* hp;
    Eterm res;

    if (is_not_atom(BIF_ARG_1)) {
    error:
	erts_free_aligned_binary_bytes(temp_alloc);
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((code = erts_get_aligned_binary_bytes(BIF_ARG_2, &temp_alloc)) == NULL) {
	goto error;
    }

    magic = erts_alloc_loader_state();
    erts_set_loader_target(magic, &loader_target_slave,
			   ERTS_ALC_T_SLAVE_CODE,
			   ERTS_ALC_T_SLAVE_PREPARED_CODE);
    sz = binary_size(BIF_ARG_2);
    reason = erts_prepare_loading(magic, BIF_P, BIF_P->group_leader,
				  &BIF_ARG_1, code, sz);
    erts_free_aligned_binary_bytes(temp_alloc);
    if (reason != NIL) {
	hp = HAlloc(BIF_P, 3);
	res = TUPLE2(hp, am_error, reason);
	BIF_RET(res);
    }
    hp = HAlloc(BIF_P, PROC_BIN_SIZE);
    res = erts_mk_magic_binary_term(&hp, &MSO(BIF_P), magic);
    erts_refc_dec(&magic->refc, 1);
    BIF_RET(res);
#else
    BIF_ERROR(BIF_P, EXC_NOTSUP);
#endif
}

BIF_RETTYPE epiphany_host_0(BIF_ALIST_0)
{
#ifdef ERTS_SLAVE
    BIF_RET(am_slave);
#else
    ERTS_DECL_AM(master);
    BIF_RET(AM_master);
#endif
}

BIF_RETTYPE epiphany_state_0(BIF_ALIST_0)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    ERTS_DECL_AM(offline);
    ERTS_DECL_AM(booting);
    if (erts_slave_online) {
	if (erts_slave_booted) BIF_RET(am_online);
	else BIF_RET(AM_booting);
    } else {
	BIF_RET(AM_offline);
    }
#elif defined(ERTS_SLAVE)
    return am_online;
#else
    ERTS_DECL_AM(unavailable);
    BIF_RET(AM_unavailable);
#endif
}

BIF_RETTYPE epiphany_module_loaded_1(BIF_ALIST_1)
{
#if defined(ERTS_SLAVE_EMU_ENABLED) || defined(ERTS_SLAVE)
    Module* modp;
    ErtsCodeIndex code_ix;
    Eterm res = am_false;

    if (is_not_atom(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    code_ix = erts_active_code_ix();
#ifdef ERTS_SLAVE
    if ((modp = erts_get_module(BIF_ARG_1, code_ix)) != NULL) {
#else
    if ((modp = slave_get_module(BIF_ARG_1, code_ix)) != NULL) {
#endif
	if (modp->curr.code != NULL
	    && modp->curr.code[MI_ON_LOAD_FUNCTION_PTR] == 0) {
	    res = am_true;
	}
    }
    BIF_RET(res);
#else
    BIF_RET(am_false);
#endif
}
