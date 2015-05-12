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
#include "slave_fifo.h"
#include "erl_bif_table.h"

/*
 * We override the default alignment of structs that are shared between slave
 * and master.
 */
#define SHARED_DATA __attribute__((aligned(8)))

typedef struct {
    Eterm module;
    Eterm name;
    int arity;
    BifFunction f;
    BifFunction traced;
} SHARED_DATA SlaveBifEntry;

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
} SHARED_DATA;

struct slave {
    struct slave_command_buffers *buffers;
    Process *c_p;
    int available;
};

enum master_command {
    MASTER_COMMAND_SETUP,
    MASTER_COMMAND_READY,
};

struct master_command_setup {
    LoaderTarget *target;
    int num_instructions;
    SlaveBifEntry *bif_table;
    int bif_size;
} SHARED_DATA;

struct master_command_ready {
} SHARED_DATA;

enum slave_command {
    SLAVE_COMMAND_RUN,
};

struct slave_command_run {
    BeamInstr *entry;
    Eterm id, parent_id;
    Eterm mod, func, args;
    Eterm *heap, *htop, *stop;
} SHARED_DATA;

#ifndef ERTS_SLAVE
void erts_init_slave_command(void);
void erts_stop_slave_command(void);

struct slave* erts_slave_pop_free(void);
void erts_slave_push_free(struct slave*);

void erts_slave_send_command(struct slave * slave, enum slave_command code,
			     const void *data, size_t size);
#else

void erts_master_send_command(struct slave * slave, enum master_command code,
			      const void *data, size_t size);

void erts_master_setup(void);
void erts_master_await_run(const struct master_command_ready *, struct slave_command_run*);
#endif

int erts_dispatch_slave_commands(void);

#endif /* ERL_SLAVE_COMMAND_H__ */
