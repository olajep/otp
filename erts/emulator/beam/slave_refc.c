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

/*
 * Reference counters can only be modified on master (due to lack of efficient
 * atomics in the slave). This module provides the expected methods via the
 * asynchronous command MASTER_COMMAND_REFC.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "slave_command.h"

#ifdef ERTS_SLAVE
void
erts_refc_init(erts_refc_t *refcp, erts_aint_t val)
{
    struct master_command_refc cmd = {
	.op = MASTER_REFC_OP_INIT,
	.refcp = refcp,
	.arg = val,
    };
    erts_master_send_command(MASTER_COMMAND_REFC, &cmd, sizeof(cmd));
}

void
erts_refc_inc(erts_refc_t *refcp, erts_aint_t min_val)
{
    struct master_command_refc cmd = {
	.op = MASTER_REFC_OP_INC,
	.refcp = refcp,
	.min_val = min_val,
    };
    erts_master_send_command(MASTER_COMMAND_REFC, &cmd, sizeof(cmd));
}

void
erts_refc_dec(erts_refc_t *refcp, erts_aint_t min_val)
{
    struct master_command_refc cmd = {
	.op = MASTER_REFC_OP_DEC,
	.refcp = refcp,
	.min_val = min_val,
    };
    erts_master_send_command(MASTER_COMMAND_REFC, &cmd, sizeof(cmd));
}

void
erts_refc_add(erts_refc_t *refcp, erts_aint_t diff, erts_aint_t min_val)
{
    struct master_command_refc cmd = {
	.op = MASTER_REFC_OP_ADD,
	.refcp = refcp,
	.arg = diff,
	.min_val = min_val,
    };
    erts_master_send_command(MASTER_COMMAND_REFC, &cmd, sizeof(cmd));
}

void
erts_refc_decfree(erts_refc_t *refcp, erts_aint_t min_val,
		  enum erts_decfree_kind kind, void *objp)
{
    struct master_command_refc cmd = {
	.op = MASTER_REFC_OP_DECFREE,
	.refcp = refcp,
	.min_val = min_val,
	.kind = kind,
	.objp = objp,
    };
    erts_master_send_command(MASTER_COMMAND_REFC, &cmd, sizeof(cmd));
}

#else
void
erts_slave_serve_refc(struct master_command_refc *cmd)
{
    switch (cmd->op) {
    case MASTER_REFC_OP_INIT:
	erts_refc_init(cmd->refcp, cmd->arg);
	break;
    case MASTER_REFC_OP_INC:
	erts_refc_inc(cmd->refcp, cmd->min_val);
	break;
    case MASTER_REFC_OP_DEC:
	erts_refc_dec(cmd->refcp, cmd->min_val);
	break;
    case MASTER_REFC_OP_ADD:
	erts_refc_add(cmd->refcp, cmd->arg, cmd->min_val);
	break;
    case MASTER_REFC_OP_DECFREE:
	erts_refc_decfree(cmd->refcp, cmd->min_val, cmd->kind, cmd->objp);
	break;
    default:
	erl_exit(1, "Unexpected op %d in erts_slave_serve_refc", cmd->op);
    }
}
#endif /* ERTS_SLAVE */
