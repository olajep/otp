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
#include "erl_bif_table.h"
#include "slave.h"
#include "slave_fifo.h"
#include "slave_state.h"

enum slave_syscall {
    /* 0 must be NONE, which means no syscall is pending */
    SLAVE_SYSCALL_NONE,
    SLAVE_SYSCALL_READY,
    SLAVE_SYSCALL_BIF,
    SLAVE_SYSCALL_GC,
};

struct slave_syscall_ready {
    /* To master */
    Eterm exit_reason;
    /* To slave */
    Eterm id, parent_id, group_leader;
    Eterm mod, func, args;
    Eterm *heap, *htop, *stop;
} SLAVE_SHARED_DATA;

/* Declared in slave_bif.h */
struct slave_syscall_bif;

struct slave_syscall_gc {
    /* To master */
    int need;
    Eterm* objv;
    int nobj;
    /* Bidirectional */
    struct slave_state state;
    /* To slave */
    int ret;
} SLAVE_SHARED_DATA;

/*
 * We use the shared-memory fifos for command buffers.
 *
 * Messages are always a command word, followed by the corresponding struct.
 *
 * Syscalls work similarly, but are synchronous. Slaves alocate the appropriate
 * argument struct from their heap, and set syscall_arg strictly before (in
 * memory order) syscall. The master polls these fields from it's slave command
 * thread and serves any pending syscalls.
 */
struct slave_command_buffers {
    /* Commands to the master */
    struct erl_fifo master;
    /* Commands to the slave */
    struct erl_fifo slave;
    /* Syscalls */
    enum slave_syscall volatile syscall;
    void *volatile syscall_arg;
} SLAVE_SHARED_DATA;

struct slave {
    struct slave_command_buffers *buffers;
    Process *c_p;
    ErtsSchedulerData *dummy_esdp;
    int available;
    int pending_syscall;
    erts_smp_atomic32_t msgq_len;
};

enum master_command {
    MASTER_COMMAND_SETUP,
    MASTER_COMMAND_FREE_MESSAGE,
};

struct master_command_setup {
    LoaderTarget *target;
    int num_instructions;
    BifEntry *bif_table;
    int bif_size;
} SLAVE_SHARED_DATA;

struct master_command_free_message {
    ErlMessage *m;
} SLAVE_SHARED_DATA;

enum slave_command {
    SLAVE_COMMAND_MESSAGE,
};

struct slave_command_message {
    ErlMessage *m;
    Eterm receiver;
} SLAVE_SHARED_DATA;

#ifndef ERTS_SLAVE
void erts_init_slave_command(void);
void erts_signal_slave_command(void);
void erts_stop_slave_command(void);

struct slave* erts_slave_pop_free(void);
void erts_slave_push_free(struct slave*);

void erts_slave_send_command(struct slave *slave, enum slave_command code,
			     const void *data, size_t size);
void *erts_slave_syscall_arg(struct slave *slave, enum slave_syscall no);
void erts_slave_finish_syscall(struct slave *slave, enum slave_syscall no);

void erts_slave_serve_gc(struct slave *slave, struct slave_syscall_gc *arg);

int erts_dispatch_slave_commands(void);
#else

void erts_master_send_command(enum master_command code, const void *data,
			      size_t size);
void erts_master_syscall(enum slave_syscall no, void *arg);

void erts_master_setup(void);

void slave_serve_message(Process *c_p, struct slave_command_message *cmd);

int erts_dispatch_slave_commands(Process *c_p);
#endif

#endif /* ERL_SLAVE_COMMAND_H__ */
