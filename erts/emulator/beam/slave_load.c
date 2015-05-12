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
#include "slave_export.h"
#include "slave_load.h"
#include "slave_ix.h"
#include "slave_module.h"

const LoaderTarget *loader_target_slave;
const TargetExportTab export_table_slave = {
    slave_export_put,
    slave_active_export_entry,
    slave_bif_export,
    slave_put_module,
};
BifEntry slave_bif_table[BIF_SIZE];
Export *slave_bif_export[BIF_SIZE];

static int slave_load_initialised = 0;
struct master_command_setup *setup_cmd;

static void
enter_slave_bifs(struct master_command_setup *cmd)
{
    int i;
    ASSERT(BIF_SIZE == cmd->bif_size);
    for (i = 0; i < BIF_SIZE; i++) {
	SlaveBifEntry *bif = &cmd->bif_table[i];
	Export *ep = slave_export_put(bif->module,
				      bif->name,
				      bif->arity);
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

	slave_bif_export[i] = ep;
	ep->code[3] = (BeamInstr) SlaveOp(op_apply_bif);
	ep->code[4] = (BeamInstr) bif->f;
	/* XXX: set func info for bifs */
	ep->fake_op_func_info_for_hipe[0]
	    = (BeamInstr) SlaveOp(op_i_func_info_IaaI);
    }
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
    erts_set_loader_target(magic, loader_target_slave,
			   &export_table_slave,
			   ERTS_ALC_T_SLAVE_CODE,
			   ERTS_ALC_T_SLAVE_PREPARED_CODE);

    retval = erts_prepare_loading(magic, c_p, group_leader, modp,
				  code, size);
    if (retval != NIL) {
	return retval;
    }
    return slave_finish_loading(magic, c_p, c_p_locks, modp);
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
    }
}

void
erts_slave_bootstrap(void)
{
    ASSERT(slave_load_initialised && setup_cmd);

    erts_start_staging_code_ix();
    enter_slave_bifs(setup_cmd);
    erts_end_staging_code_ix();
    erts_commit_staging_code_ix();

    erts_start_staging_code_ix();
    load_preloaded();
    erts_end_staging_code_ix();
    erts_commit_staging_code_ix();

    free(setup_cmd);
}

void
erts_slave_init_load(struct master_command_setup *cmd)
{
    if (slave_load_initialised) return;
    slave_init_export_table();
    slave_init_module_table();
    slave_code_ix_init();

    if (cmd->num_instructions != num_instructions) {
	erl_exit(1, "Error: Got %d instructions from slave emulator, expected %d\n",
		 cmd->num_instructions, num_instructions);
    }
    if (cmd->bif_size != BIF_SIZE) {
	erl_exit(1, "Error: Got %d bifs from slave emulator, expected %d\n",
		 cmd->bif_size, BIF_SIZE);
    }

    loader_target_slave = cmd->target;

    setup_cmd = malloc(sizeof(struct master_command_setup));
    ASSERT(setup_cmd);
    memcpy(setup_cmd, cmd, sizeof(struct master_command_setup));
    slave_load_initialised = 1;
}
