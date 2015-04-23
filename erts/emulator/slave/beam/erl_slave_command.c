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
#include "erl_slave_command.h"
#include "epiphany.h"

/* Set by the master */
EPIPHANY_SRAM_DATA struct slave_command_buffers *volatile slave_command_buffers;

static void send_command(enum master_command code, void *data, size_t size);

void
erts_master_ready(void)
{
    extern BeamInstr *demo_prog;
    struct master_command_ready cmd = {
	.demo_prog = demo_prog,
    };
#ifndef NO_JUMP_TABLE
    extern void **beam_ops;
    cmd.beam_ops = beam_ops;
#endif
    send_command(MASTER_COMMAND_READY, &cmd, sizeof(cmd));
}

static void
send_command(enum master_command code, void *data, size_t size)
{
    if (slave_command_buffers == NULL)
	erts_printf("Waiting for command buffers to become available\n");
    while (slave_command_buffers == NULL);
    erts_fifo_write_blocking(&slave_command_buffers->master, &code, sizeof(code));
    erts_fifo_write_blocking(&slave_command_buffers->master, data, size);
}

void
erts_master_await_run(struct slave_command_run *cmd)
{
    enum slave_command code;
    erts_printf("Awaiting program from master\n");
    while (slave_command_buffers == NULL);
    erts_fifo_read_blocking(&slave_command_buffers->slave, &code, sizeof(code));
    ASSERT(code == SLAVE_COMMAND_RUN);
    erts_fifo_read_blocking(&slave_command_buffers->slave, cmd,
			    sizeof(struct slave_command_run));
}
