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
#  include "erl_binary.h"
#  include "beam_bp.h"

#  include "slave_process.h"
#  include "slave_load.h"
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

BIF_RETTYPE slave_print_1(BIF_ALIST_1)
{
    erts_printf("slave:print/1: %T\n", BIF_ARG_1);
    BIF_RET(am_ok);
}

BIF_RETTYPE slave_boot_0(BIF_ALIST_0)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD0(bif_export[BIF_slave_boot_0], BIF_P);
    }
    erts_slave_bootstrap();
    erts_release_code_write_permission();
    BIF_RET(am_ok);
#else
    BIF_RET(am_undefined);
#endif
}

BIF_RETTYPE slave_prepare_loading_2(BIF_ALIST_2)
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
    erts_set_loader_target(magic, loader_target_slave,
			   &export_table_slave,
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
    BIF_RET(am_undefined);
#endif
}

#ifdef ERTS_SLAVE_EMU_ENABLED
struct m {
    Binary* code;
    Eterm module;
    Module* modp;
    Uint exception;
};

static Eterm staging_epilogue(Process* c_p, int, Eterm res, int, struct m*, int);
#ifdef ERTS_SMP
static void smp_code_ix_commiter(void*);

static struct /* Protected by code_write_permission */
{
    Process* stager;
    ErtsThrPrgrLaterOp lop;
}commiter_state;
#endif

static void set_default_trace_pattern(Eterm module);

static Eterm
exception_list(Process* p, Eterm tag, struct m* mp, Sint exceptions)
{
    Eterm* hp = HAlloc(p, 3 + 2*exceptions);
    Eterm res = NIL;

    mp += exceptions - 1;
    while (exceptions > 0) {
	if (mp->exception) {
	    res = CONS(hp, mp->module, res);
	    hp += 2;
	    exceptions--;
	}
	mp--;
    }
    return TUPLE2(hp, tag, res);
}
#endif

BIF_RETTYPE slave_finish_loading_1(BIF_ALIST_1)
{
#ifdef ERTS_SLAVE_EMU_ENABLED
    int i;
    int n;
    struct m* p = NULL;
    Uint exceptions;
    Eterm res;
    int is_blocking = 0;
    int do_commit = 0;

    if (!erts_try_seize_code_write_permission(BIF_P)) {
	ERTS_BIF_YIELD1(bif_export[BIF_finish_loading_1], BIF_P, BIF_ARG_1);
    }

    /*
     * Validate the argument before we start loading; it must be a
     * proper list where each element is a magic binary containing
     * prepared (not previously loaded) code.
     *
     * First count the number of elements and allocate an array
     * to keep the elements in.
     */

    n = erts_list_length(BIF_ARG_1);
    if (n == -1) {
	ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	goto done;
    }
    p = erts_alloc(ERTS_ALC_T_LOADER_TMP, n*sizeof(struct m));

    /*
     * We now know that the argument is a proper list. Validate
     * and collect the binaries into the array.
     */

    for (i = 0; i < n; i++) {
	Eterm* cons = list_val(BIF_ARG_1);
	Eterm term = CAR(cons);
	ProcBin* pb;

	if (!ERTS_TERM_IS_MAGIC_BINARY(term)) {
	    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	    goto done;
	}
	pb = (ProcBin*) binary_val(term);
	p[i].code = pb->val;
	p[i].module = erts_module_for_prepared_code(p[i].code);
	if (p[i].module == NIL) {
	    ERTS_BIF_PREP_ERROR(res, BIF_P, BADARG);
	    goto done;
	}
	BIF_ARG_1 = CDR(cons);
    }

    /*
     * Since we cannot handle atomic loading of a group of modules
     * if one or more of them uses on_load, we will only allow one
     * element in the list. This limitation is intended to be
     * lifted in the future.
     */

    if (n > 1) {
	ERTS_BIF_PREP_ERROR(res, BIF_P, SYSTEM_LIMIT);
	goto done;
    }

    /*
     * All types are correct. There cannot be a BADARG from now on.
     * Before we can start loading, we must check whether any of
     * the modules already has old code. To avoid a race, we must
     * not allow other process to initiate a code loading operation
     * from now on.
     */

    res = am_ok;
    erts_start_staging_code_ix();

    for (i = 0; i < n; i++) {
	p[i].modp = erts_put_module(p[i].module);
    }
    for (i = 0; i < n; i++) {
	if (p[i].modp->curr.num_breakpoints > 0 ||
	    p[i].modp->curr.num_traced_exports > 0 ||
	    erts_is_default_trace_enabled()) {
	    /* tracing involved, fallback with thread blocking */
	    erts_smp_proc_unlock(BIF_P, ERTS_PROC_LOCK_MAIN);
	    erts_smp_thr_progress_block();
	    is_blocking = 1;
	    break;
	}
    }

    if (is_blocking) {
	for (i = 0; i < n; i++) {
	    if (p[i].modp->curr.num_breakpoints) {
		erts_clear_module_break(p[i].modp);
		ASSERT(p[i].modp->curr.num_breakpoints == 0);
	    }
	}
    }

    exceptions = 0;
    for (i = 0; i < n; i++) {
	p[i].exception = 0;
	if (p[i].modp->curr.code && p[i].modp->old.code) {
	    p[i].exception = 1;
	    exceptions++;
	}
    }

    if (exceptions) {
	res = exception_list(BIF_P, am_not_purged, p, exceptions);
    } else {
	/*
	 * Now we can load all code. This can't fail.
	 */

	exceptions = 0;
	for (i = 0; i < n; i++) {
	    Eterm mod;
	    Eterm retval;

	    erts_refc_inc(&p[i].code->refc, 1);
	    retval = slave_finish_loading(p[i].code, BIF_P, 0, &mod);
	    ASSERT(retval == NIL || retval == am_on_load);
	    if (retval == am_on_load) {
		p[i].exception = 1;
		exceptions++;
	    }
	}

	/*
	 * Check whether any module has an on_load_handler.
	 */

	if (exceptions) {
	    res = exception_list(BIF_P, am_on_load, p, exceptions);
	}
	do_commit = 1;
    }

done:
    return staging_epilogue(BIF_P, do_commit, res, is_blocking, p, n);
#else
    BIF_RET(am_undefined);
#endif
}

#ifdef ERTS_SLAVE_EMU_ENABLED
static Eterm
staging_epilogue(Process* c_p, int commit, Eterm res, int is_blocking,
		 struct m* loaded, int nloaded)
{
#  ifdef ERTS_SMP
    if (is_blocking || !commit)
#  endif
    {
	if (commit) {
	    erts_end_staging_code_ix();
	    erts_commit_staging_code_ix();
	    if (loaded) {
		int i;
		for (i=0; i < nloaded; i++) {
		    set_default_trace_pattern(loaded[i].module);
		}
	    }
	}
	else {
	    erts_abort_staging_code_ix();
	}
	if (loaded) {
	    erts_free(ERTS_ALC_T_LOADER_TMP, loaded);
	}
	if (is_blocking) {
	    erts_smp_thr_progress_unblock();
	    erts_smp_proc_lock(c_p, ERTS_PROC_LOCK_MAIN);
	}
	erts_release_code_write_permission();
	return res;
    }
#  ifdef ERTS_SMP
    else {
	ASSERT(is_value(res));

	if (loaded) {
	    erts_free(ERTS_ALC_T_LOADER_TMP, loaded);
	}
	erts_end_staging_code_ix();
	/*
	 * Now we must wait for all schedulers to do a memory barrier before
	 * we can commit and let them access the new staged code. This allows
	 * schedulers to read active code_ix in a safe way while executing
	 * without any memory barriers at all.
	 */
	ASSERT(commiter_state.stager == NULL);
	commiter_state.stager = c_p;
	erts_schedule_thr_prgr_later_op(smp_code_ix_commiter, NULL, &commiter_state.lop);
	erts_smp_proc_inc_refc(c_p);
	erts_suspend(c_p, ERTS_PROC_LOCK_MAIN, NULL);
	/*
	 * smp_code_ix_commiter() will do the rest "later"
	 * and resume this process to return 'res'.
	 */
	ERTS_BIF_YIELD_RETURN(c_p, res);
    }
#  endif
}

#ifdef ERTS_SMP
static void smp_code_ix_commiter(void* null)
{
    Process* p = commiter_state.stager;

    erts_commit_staging_code_ix();
#ifdef DEBUG
    commiter_state.stager = NULL;
#endif
    erts_release_code_write_permission();
    erts_smp_proc_lock(p, ERTS_PROC_LOCK_STATUS);
    if (!ERTS_PROC_IS_EXITING(p)) {
	erts_resume(p, ERTS_PROC_LOCK_STATUS);
    }
    erts_smp_proc_unlock(p, ERTS_PROC_LOCK_STATUS);
    erts_smp_proc_dec_refc(p);
}
#endif /* ERTS_SMP */

static void
set_default_trace_pattern(Eterm module)
{
    int trace_pattern_is_on;
    Binary *match_spec;
    Binary *meta_match_spec;
    struct trace_pattern_flags trace_pattern_flags;
    Eterm meta_tracer_pid;

    erts_get_default_trace_pattern(&trace_pattern_is_on,
				   &match_spec,
				   &meta_match_spec,
				   &trace_pattern_flags,
				   &meta_tracer_pid);
    if (trace_pattern_is_on) {
	Eterm mfa[1];
	mfa[0] = module;
	(void) erts_set_trace_pattern(0, mfa, 1,
				      match_spec,
				      meta_match_spec,
				      1, trace_pattern_flags,
				      meta_tracer_pid, 1);
    }
}
#endif
