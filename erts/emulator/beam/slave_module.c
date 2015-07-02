/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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
#include "erl_vm.h"
#include "global.h"
#include "beam_catches.h"
#include "slave_module.h"
#include "slave_load.h"
#include "slave_syms.h"

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

#define MODULE_SIZE   50
#define MODULE_LIMIT  (64*1024)

static IndexTable *module_tables = (void*)SLAVE_SYM_module_tables;

/* ETODO: does this not collide with module.o? */
erts_smp_rwmtx_t the_old_code_rwlocks[ERTS_NUM_CODE_IX];

static erts_smp_atomic_t tot_module_bytes;

static int initialized = 0;

/* SMP note: Active module table lookup and current module instance can be
 *           read without any locks. Old module instances are protected by
 *           "the_old_code_rwlocks" as purging is done on active module table.
 *           Staging table is protected by the "code_ix lock".
 */

#include "erl_smp.h"

static void delete_code(Module* modp);

static HashValue module_hash(Module* x)
{
    return (HashValue) x->module;
}


static int module_cmp(Module* tmpl, Module* obj)
{
    return tmpl->module != obj->module;
}

static Module* module_alloc(Module* tmpl)
{
    Module* obj = (Module*) erts_alloc(ERTS_ALC_T_SLAVE_MODULE, sizeof(Module));
    erts_smp_atomic_add_nob(&tot_module_bytes, sizeof(Module));

    obj->module = tmpl->module;
    obj->curr.code = 0;
    obj->old.code = 0;
    obj->curr.code_length = 0;
    obj->old.code_length = 0;
    obj->slot.index = -1;
    obj->curr.nif = NULL;
    obj->old.nif = NULL;
    obj->curr.num_breakpoints = 0;
    obj->old.num_breakpoints  = 0;
    obj->curr.num_traced_exports = 0;
    obj->old.num_traced_exports = 0;
    return obj;
}

static void module_free(Module* mod)
{
    erts_free(ERTS_ALC_T_SLAVE_MODULE, mod);
    erts_smp_atomic_add_nob(&tot_module_bytes, -sizeof(Module));
}

void slave_init_module_table(void)
{
    HashFunctions f;
    int i;

    f.hash = (H_FUN) module_hash;
    f.cmp  = (HCMP_FUN) module_cmp;
    f.alloc = (HALLOC_FUN) module_alloc;
    f.free = (HFREE_FUN) module_free;

    for (i = 0; i < ERTS_NUM_CODE_IX; i++) {
	erts_index_init(ERTS_ALC_T_SLAVE_MODULE_TABLE, &module_tables[i],
			"slave_module_code", MODULE_SIZE, MODULE_LIMIT, f);
    }

    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	erts_smp_rwmtx_init_x(&the_old_code_rwlocks[i], "slave_old_code", make_small(i));
    }
    erts_smp_atomic_init_nob(&tot_module_bytes, 0);
    initialized = 1;
}

Module*
slave_get_module(Eterm mod, ErtsCodeIndex code_ix)
{
    Module e;
    int index;
    IndexTable* mod_tab;

    ASSERT(is_atom(mod));

    if (!initialized) return NULL;

    mod_tab = &module_tables[code_ix];

    e.module = atom_val(mod);
    index = index_get(mod_tab, (void*) &e);
    if (index == -1) {
	return NULL;
    } else {
	return (Module*) erts_index_lookup(mod_tab, index);
    }
}

Module*
slave_put_module(Eterm mod)
{
    Module e;
    IndexTable* mod_tab;
    int oldsz, newsz;
    Module* res;

    ASSERT(is_atom(mod));
    ASSERT(initialized);
    ERTS_SMP_LC_ASSERT(erts_initialized == 0
		       || erts_has_code_write_permission());

    mod_tab = &module_tables[erts_staging_code_ix()];
    e.module = atom_val(mod);
    oldsz = index_table_sz(mod_tab);
    res = (Module*) index_put_entry(mod_tab, (void*) &e);
    newsz = index_table_sz(mod_tab);
    erts_smp_atomic_add_nob(&tot_module_bytes, (newsz - oldsz));
    return res;
}

Module *slave_module_code(int i, ErtsCodeIndex code_ix)
{
    return (Module*) erts_index_lookup(&module_tables[code_ix], i);
}

int slave_module_code_size(ErtsCodeIndex code_ix)
{
    return module_tables[code_ix].entries;
}

int slave_module_table_sz(void)
{
    return erts_smp_atomic_read_nob(&tot_module_bytes);
}

#ifdef DEBUG
static ErtsCodeIndex dbg_load_code_ix = -1;
#endif

static int entries_at_start_staging = 0;

void slave_module_start_staging(void)
{
    IndexTable* src = &module_tables[erts_active_code_ix()];
    IndexTable* dst = &module_tables[erts_staging_code_ix()];
    Module* src_mod;
    Module* dst_mod;
    int i, oldsz, newsz;

    if (!initialized) return;

    ASSERT(dbg_load_code_ix == -1);
    ASSERT(dst->entries <= src->entries);

    /*
     * Make sure our existing modules are up-to-date
     */
    for (i = 0; i < dst->entries; i++) {
	src_mod = (Module*) erts_index_lookup(src, i);
	dst_mod = (Module*) erts_index_lookup(dst, i);
	ASSERT(src_mod->module == dst_mod->module);

	dst_mod->curr = src_mod->curr;
	dst_mod->old = src_mod->old;
    }

    /*
     * Copy all new modules from active table
     */
    oldsz = index_table_sz(dst);
    for (i = dst->entries; i < src->entries; i++) {
	src_mod = (Module*) erts_index_lookup(src, i);
	dst_mod = (Module*) index_put_entry(dst, src_mod);
	ASSERT(dst_mod != src_mod);

	dst_mod->curr = src_mod->curr;
	dst_mod->old = src_mod->old;
    }
    newsz = index_table_sz(dst);
    erts_smp_atomic_add_nob(&tot_module_bytes, (newsz - oldsz));

    entries_at_start_staging = dst->entries;
    IF_DEBUG(dbg_load_code_ix = erts_staging_code_ix());
}

void slave_module_end_staging(int commit)
{
    if (!initialized) return;

    ASSERT(dbg_load_code_ix == erts_staging_code_ix());

    if (!commit) { /* abort */
	IndexTable* tab = &module_tables[erts_staging_code_ix()];
	int oldsz, newsz;

	ASSERT(entries_at_start_staging <= tab->entries);
	oldsz = index_table_sz(tab);
	index_erase_latest_from(tab, entries_at_start_staging);
	newsz = index_table_sz(tab);
	erts_smp_atomic_add_nob(&tot_module_bytes, (newsz - oldsz));
    }

    IF_DEBUG(dbg_load_code_ix = -1);
}

Eterm
slave_make_current_old(Process *c_p, ErtsProcLocks c_p_locks, Eterm module)
{
    Module* modp = slave_put_module(module);

    /*
     * Check if the previous code has been already deleted;
     * if not, delete old code; error if old code already exists.
     */

    if (modp->curr.code != NULL && modp->old.code != NULL)  {
	return am_not_purged;
    } else if (modp->old.code == NULL) { /* Make the current version old. */
	delete_code(modp);
    }
    return NIL;
}

/*
 * Move code from current to old and null all export entries for the module
 */

static void
delete_code(Module* modp)
{
    ErtsCodeIndex code_ix = erts_staging_code_ix();
    Eterm module = make_atom(modp->module);
    int i;

    for (i = 0; i < export_list_size(code_ix); i++) {
	Export *ep = export_list(i, code_ix);
	if (ep != NULL && (ep->slave_code[0] == module)) {
	    if (ep->slave_addressv[code_ix] == ep->slave_code+3) {
		if (ep->slave_code[3] == (BeamInstr) SlaveOp(op_apply_bif)) {
		    continue;
		}
		else if (ep->slave_code[3] ==
			 (BeamInstr) SlaveOp(op_i_generic_breakpoint)) {
		    ERTS_SMP_LC_ASSERT(erts_smp_thr_progress_is_blocking());
		    ASSERT(modp->curr.num_traced_exports > 0);
		    /* There's no breakpoints yet! */
		    ASSERT(!"breakpoints");
		    /* erts_clear_export_break(modp, ep->code+3); */
		}
		else ASSERT(ep->slave_code[3] ==
			    (BeamInstr) SlaveOp(op_call_error_handler));
	    }
	    ep->slave_addressv[code_ix] = ep->slave_code+3;
	    ep->slave_code[3] = (BeamInstr) SlaveOp(op_call_error_handler);
	    ep->slave_code[4] = 0;
	}
    }

    ASSERT(modp->curr.num_breakpoints == 0);
    ASSERT(modp->curr.num_traced_exports == 0);
    modp->old = modp->curr;
    modp->curr.code = NULL;
    modp->curr.code_length = 0;
    modp->curr.catches = BEAM_CATCHES_NIL;
    modp->curr.nif = NULL;
}
