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

#ifndef ERL_SLAVE_COMMAND_H__
#define ERL_SLAVE_COMMAND_H__

#include "erl_term.h"
#include "erl_process.h"
#include "erl_fifo.h"

#ifndef ERTS_SLAVE
extern void **slave_beam_ops;
extern BeamInstr *slave_demo_prog;

void erts_init_slave_command(void);
void erts_stop_slave_command(void);
#endif

int erts_dispatch_slave_commands(void);

/*
 * We use the shared-memory fifos for command buffers.
 *
 * Messages are always a command word, followed by the corresponding struct.
 */
struct slave_command_buffers {
    /* Commands to the master */
    struct erl_fifo master;
    /* Commands to the slave */
    struct erl_fifo slave;
};

enum master_command {
    MASTER_COMMAND_READY
};

struct master_command_ready {
#ifndef NO_JUMP_TABLE
    void** beam_ops;
#endif
    BeamInstr *demo_prog;
};

enum slave_command {
    SLAVE_COMMAND_RUN
};

struct slave_command_run {
    BeamInstr *entry;
    Eterm parent;
};

#ifdef ERTS_SLAVE
void erts_master_ready(void);
void erts_master_await_run(struct slave_command_run*);
#endif

#ifndef ERTS_SLAVE
Eterm erts_slave_run(Process*, BeamInstr*);
#endif

#endif /* ERL_SLAVE_COMMAND_H__ */
