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
#include "beam_load.h"
#include "slave_export.h"
#include "erl_slave_load.h"

const LoaderTarget *loader_target_slave;
BeamInstr *slave_demo_prog;
BifEntry slave_bif_table[BIF_SIZE];
Export *slave_bif_export[BIF_SIZE];

static int slave_load_initialised = 0;

static void
enter_slave_bifs(struct master_command_setup *cmd)
{
    int i;
    ASSERT(BIF_SIZE >= cmd->bif_size);
    for (i = 0; i < cmd->bif_size; i++) {
	SlaveBifEntry *bif = &cmd->bif_table[i];
	Export *ep = slave_export_put(bif->module,
				      bif->name,
				      bif->arity);
	/* The export entries need to be in shared DRAM or they won't be
	 * accessible from the Epiphany */
	ASSERT(0x8e000000 <= (unsigned)ep && (unsigned)ep < 0x90000000);

	ASSERT(is_atom(bif->module) && is_atom(bif->name) &&
	       0x8e000000 <= (unsigned)bif->f && (unsigned)bif->f < 0x90000000);
	slave_bif_table[i].module = bif->module;
	slave_bif_table[i].name   = bif->name;
	slave_bif_table[i].arity  = bif->arity;
	slave_bif_table[i].f      = bif->f;
	slave_bif_table[i].traced = bif->traced;

	slave_bif_export[i] = ep;
	ep->code[3] = (BeamInstr) SlaveOp(op_apply_bif);
	ep->code[4] = (BeamInstr) bif->f;
	/* XXX: set func info for bifs */
	ep->fake_op_func_info_for_hipe[0] = (BeamInstr) SlaveOp(op_i_func_info_IaaI);
    }
    erts_printf("%d slave bifs loaded (%d host bifs)\n", cmd->bif_size, BIF_SIZE);
}

void
erts_slave_init_load(struct master_command_setup *cmd)
{
    if (slave_load_initialised) return;
    slave_init_export_table();

    if (cmd->num_instructions != num_instructions) {
	erl_exit(1, "Error: Got %d instructions from slave emulator, expected %d\n",
		 cmd->num_instructions, num_instructions);
    }

    loader_target_slave = cmd->target;
    slave_demo_prog = cmd->demo_prog;

    enter_slave_bifs(cmd);

    slave_load_initialised = 1;
}
