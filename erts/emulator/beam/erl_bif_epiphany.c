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
#  include "big.h"
#endif

#ifdef ERTS_SLAVE_EMU_ENABLED
static Binary *alloc_slave_loader_state(void)
{
    Binary *magic = erts_alloc_loader_state();
    erts_set_loader_target(magic, &loader_target_slave,
			   ERTS_ALC_T_SLAVE_CODE,
			   ERTS_ALC_T_SLAVE_PREPARED_CODE);
    return magic;
}
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

    magic = alloc_slave_loader_state();
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

BIF_RETTYPE epiphany_make_stub_module_3(BIF_ALIST_3)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    Module* modp;
    Binary* magic;
    Eterm res;

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD3(bif_export[BIF_epiphany_make_stub_module_3],
			BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    }

    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_smp_thr_progress_block();

    modp = slave_get_module(BIF_ARG_1, erts_active_code_ix());

    if (modp && modp->curr.num_breakpoints > 0) {
	ASSERT(modp->curr.code != NULL);
	erts_clear_module_break(modp);
	ASSERT(modp->curr.num_breakpoints == 0);
    }

    erts_start_staging_code_ix();

    magic = alloc_slave_loader_state();
    res = erts_make_stub_module(magic, BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3);
    erts_refc_decfree(&magic->refc, 0, ERTS_DECFREE_BIN, magic);

    if (res == BIF_ARG_1) {
	erts_end_staging_code_ix();
	erts_commit_staging_code_ix();
    }
    else {
	erts_abort_staging_code_ix();
    }
    erts_smp_thr_progress_unblock();
    erts_smp_proc_lock(BIF_P, ERTS_PROC_LOCK_MAIN);
    erts_release_code_write_permission();
    return res;
#else
    BIF_ERROR(BIF_P, EXC_NOTSUP);
#endif
}

#ifdef ERTS_SLAVE
static int term_to_timer_config(Eterm term, e_ctimer_config_t *out)
{
    if      (ERTS_IS_ATOM_STR("off",       term)) *out = E_CTIMER_OFF;
    else if (ERTS_IS_ATOM_STR("clk",       term)) *out = E_CTIMER_CLK;
    else if (ERTS_IS_ATOM_STR("ialu_inst", term)) *out = E_CTIMER_IALU_INST;
    else if (ERTS_IS_ATOM_STR("fpu_inst",  term)) *out = E_CTIMER_FPU_INST;
    else if (ERTS_IS_ATOM_STR("dual_inst", term)) *out = E_CTIMER_DUAL_INST;
    else if (ERTS_IS_ATOM_STR("e1_stalls", term)) *out = E_CTIMER_E1_STALLS;
    else if (ERTS_IS_ATOM_STR("ra_stalls", term)) *out = E_CTIMER_RA_STALLS;
    else if (ERTS_IS_ATOM_STR("ext_fetch_stalls", term))
	*out = E_CTIMER_EXT_FETCH_STALLS;
    else if (ERTS_IS_ATOM_STR("ext_load_stalls", term))
	*out = E_CTIMER_EXT_LOAD_STALLS;
    else {
	*out = 0;
	return 0;
    }
    return 1;
}

static EPIPHANY_SRAM_DATA unsigned high_zero, high_one;

static void EPIPHANY_SRAM_FUNC __attribute__((interrupt))
timer_zero_wrap_handler(void)
{
    /* We will spill all caller-save registers to stack if we call anything */
    asm("movts ctimer0, %0" : /* No output */ : "r"(E_CTIMER_MAX));
    high_zero++;
}

static void EPIPHANY_SRAM_FUNC __attribute__((interrupt))
timer_one_wrap_handler(void)
{
    /* We will spill all caller-save registers to stack if we call anything */
    asm("movts ctimer1, %0" : /* No output */ : "r"(E_CTIMER_MAX));
    high_one++;
}

BIF_RETTYPE epiphany_timers_2(BIF_ALIST_2)
{
    e_ctimer_config_t conf_zero, conf_one;
    unsigned low_zero, low_one;
    /* Uint64   val_zero, val_one; */
    Eterm res_zero, res_one, ret, *hp;

    if (!term_to_timer_config(BIF_ARG_1, &conf_zero)
	|| !term_to_timer_config(BIF_ARG_2, &conf_one))
	BIF_ERROR(BIF_P, BADARG);

    /* Stop and fetch old timer values */
    low_zero = E_CTIMER_MAX - e_ctimer_stop(E_CTIMER_0);
    low_one  = E_CTIMER_MAX - e_ctimer_stop(E_CTIMER_1);

    /* Build return value */
    res_zero = erts_make_integer(low_zero, BIF_P);
    if (high_zero)
	res_zero = erts_mixed_plus
	    (BIF_P, res_zero,
	     erts_mixed_times(BIF_P, erts_make_integer(E_CTIMER_MAX, BIF_P),
			      erts_make_integer(high_zero, BIF_P)));
    res_one = erts_make_integer(low_one, BIF_P);
    if (high_one)
	res_one = erts_mixed_plus
	    (BIF_P, res_one,
	     erts_mixed_times(BIF_P, erts_make_integer(E_CTIMER_MAX, BIF_P),
			      erts_make_integer(high_one, BIF_P)));
    hp = HAlloc(BIF_P, 3);
    ret = TUPLE2(hp, res_zero, res_one);

    /* Reset high bits */
    high_zero = high_one = 0;

    /* Reset values, configure interrupt handlers, and restart with new config */
    e_ctimer_set(E_CTIMER_0, E_CTIMER_MAX);
    e_ctimer_set(E_CTIMER_1, E_CTIMER_MAX);
    if (conf_zero != E_CTIMER_OFF) {
	e_irq_attach(E_TIMER0_INT, timer_zero_wrap_handler);
	e_ctimer_start(E_CTIMER_0, conf_zero);
    }
    if (conf_one  != E_CTIMER_OFF) {
	e_irq_attach(E_TIMER1_INT, timer_one_wrap_handler);
	e_ctimer_start(E_CTIMER_1, conf_one);
    }

    return ret;
}
#else /* ERTS_SLAVE */
BIF_RETTYPE epiphany_timers_2(BIF_ALIST_2)
{
    BIF_ERROR(BIF_P, EXC_NOTSUP);
}
#endif
