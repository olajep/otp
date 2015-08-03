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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_term.h"
#include "beam_bp.h"
#include "erl_binary.h"
#include "beam_load.h"
#include "beam_catches.h"
#include "slave_load.h"
#include "slave_ix.h"
#include "slave_module.h"

#include "slave_syms.h"

LoaderTarget loader_target_slave = {
#ifndef NO_JUMP_TABLE
    NULL,
#endif
    offsetof(Export, slave_addressv),
    offsetof(Export, slave_code),
    slave_put_module,
    slave_catches_cons,
    slave_make_current_old,
    slave_update_ranges,
    offsetof(ErlFunEntry, slave_address),
};
BifEntry slave_bif_table[BIF_SIZE];
static Export *SLAVE_SHARED_DATA* const slave_bif_export = (Export *SLAVE_SHARED_DATA*)SLAVE_SYM_bif_export;

int erts_slave_booted = 0;
static int slave_load_initialised = 0;
struct master_command_setup *setup_cmd;

static void
enter_slave_bifs(struct master_command_setup *cmd)
{
    int i;
    ASSERT(BIF_SIZE == cmd->bif_size);
    for (i = 0; i < BIF_SIZE; i++) {
	BifEntry *bif = &cmd->bif_table[i];
	Export *ep = bif_export[i];

	/* The export entries need to be in shared DRAM or they won't be
	 * accessible from the Epiphany */
	ASSERT(0x8e000000 <= (unsigned)ep && (unsigned)ep < 0x90000000);

	ASSERT(bif->module == bif_table[i].module);
	ASSERT(bif->name   == bif_table[i].name);
	ASSERT(bif->arity  == bif_table[i].arity);
	ASSERT(0x8e000000 <= (unsigned)bif->f && (unsigned)bif->f < 0x90000000);

	slave_bif_table[i].module = bif->module;
	slave_bif_table[i].name   = bif->name;
	slave_bif_table[i].arity  = bif->arity;
	slave_bif_table[i].f      = bif->f;
	slave_bif_table[i].traced = bif->traced;

	ep->slave_code[3] = (BeamInstr) SlaveOp(op_apply_bif);
	ep->slave_code[4] = (BeamInstr) bif->f;
	/* XXX: set func info for bifs */
	ep->slave_fake_op_func_info_for_hipe[0]
	    = (BeamInstr) SlaveOp(op_i_func_info_IaaI);
    }
    memcpy(slave_bif_export, bif_export, BIF_SIZE*sizeof(Export*));
}

static Eterm
slave_preload_module(Process *c_p,
		     ErtsProcLocks c_p_locks,
		     Eterm group_leader, /* Group leader or NIL if none. */
		     Eterm* modp,	/*
					 * Module name as an atom (NIL to not check).
					 * On return, contains the actual module name.
					 */
		     byte* code,	/* Points to the code to load */
		     Uint size)	/* Size of code to load. */
{
    Binary* magic = erts_alloc_loader_state();
    Eterm retval;
    erts_set_loader_target(magic, &loader_target_slave,
			   ERTS_ALC_T_SLAVE_CODE,
			   ERTS_ALC_T_SLAVE_PREPARED_CODE);

    retval = erts_prepare_loading(magic, c_p, group_leader, modp,
				  code, size);
    if (retval != NIL) {
	return retval;
    }
    return erts_finish_loading(magic, c_p, c_p_locks, modp);
}

static void
load_preloaded(void)
{
    int i;
    Eterm res;
    Preload* preload_p;
    Eterm module_name;
    byte* code;
    char* name;
    int length;

    if ((preload_p = sys_preloaded()) == NULL) {
	return;
    }
    i = 0;
    while ((name = preload_p[i].name) != NULL) {
	erts_start_staging_code_ix();
	length = preload_p[i].size;
	module_name = erts_atom_put((byte *) name, sys_strlen(name), ERTS_ATOM_ENC_LATIN1, 1);
	if ((code = sys_preload_begin(&preload_p[i])) == 0)
	    erl_exit(1, "Failed to find preloaded code for module %s\n",
		     name);
	res = slave_preload_module(NULL, 0, NIL, &module_name, code, length);
	sys_preload_end(&preload_p[i]);
	if (res != NIL)
	    erl_exit(1,"Failed loading preloaded module %s (%T)\n",
		     name, res);
	i++;
	erts_end_staging_code_ix();
	erts_commit_staging_code_ix();
    }
}

Eterm
erts_slave_can_bootstrap(void)
{
    if (erts_slave_booted) {
	ERTS_DECL_AM(already_online);
	return AM_already_online;
    }
    if (!slave_load_initialised) {
	ERTS_DECL_AM(wait);
	return AM_wait;
    }
    return am_yes;
}

void
erts_slave_bootstrap(void)
{
    ASSERT(slave_load_initialised && setup_cmd && !erts_slave_booted);

    erts_start_staging_code_ix();
    enter_slave_bifs(setup_cmd);
    erts_end_staging_code_ix();
    erts_commit_staging_code_ix();

    /* beam_ranges does not seem to support commiting more than one module at
     * once; we let load_preloaded commit after each module instead. */
    load_preloaded();

    free(setup_cmd);
    erts_slave_booted = 1;
}

void
erts_slave_init_load(struct master_command_setup *cmd)
{
    if (slave_load_initialised) return;

#ifndef NO_JUMP_TABLE
    /* Must come before slave_init_export_table() */
    loader_target_slave.beam_ops = cmd->target->beam_ops;
#endif

    slave_init_export_table();
    slave_init_module_table();
    slave_code_ix_init();
    slave_catches_init();
    slave_init_ranges();
    slave_init_atom_table();
    slave_init_node_tables();
#ifdef HIPE
    erts_start_staging_code_ix();
    hipe_mode_switch_slave_init();
    erts_end_staging_code_ix();
    erts_commit_staging_code_ix();
#endif

    if (cmd->num_instructions != num_instructions) {
	erl_exit(1, "Error: Got %d instructions from slave emulator, expected %d\n",
		 cmd->num_instructions, num_instructions);
    }
    if (cmd->bif_size != BIF_SIZE) {
	erl_exit(1, "Error: Got %d bifs from slave emulator, expected %d\n",
		 cmd->bif_size, BIF_SIZE);
    }

    setup_cmd = malloc(sizeof(struct master_command_setup));
    ASSERT(setup_cmd);
    memcpy(setup_cmd, cmd, sizeof(struct master_command_setup));
    slave_load_initialised = 1;
}
