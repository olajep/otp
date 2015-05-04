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

/*
 * E-Hal is compiled without large file support, and makes use of off_t for
 * memory offsets. We compile the files that interface with e-hal without it.
 *
 * Beware: off_t might be a different size in these file compared than all other
 * files.
 */
#ifdef _FILE_OFFSET_BITS
#  undef _FILE_OFFSET_BITS
#endif

#include "sys.h"
#include "slave_syms.h"
#include "slave_io.h"
#include "slave_command.h"
#include "slave_load.h"
#include "slave_bif.h"

static int num_slaves = 0;
static struct slave *slaves;
#define SLAVE_COMMAND_BUFFER_SIZE 512

static void alloc_slave_buffer(struct erl_fifo *buffer) {
    void *data = erts_alloc(ERTS_ALC_T_SLAVE_COMMAND,
			    SLAVE_COMMAND_BUFFER_SIZE);
    ASSERT(data);
    erts_fifo_init(buffer, data, SLAVE_COMMAND_BUFFER_SIZE);
}

static struct slave_command_buffers *alloc_slave_buffers(void) {
    struct slave_command_buffers *buffers
	= erts_alloc(ERTS_ALC_T_SLAVE_COMMAND,
		     sizeof(struct slave_command_buffers));
    buffers->master.size = buffers->slave.size = SLAVE_COMMAND_BUFFER_SIZE;
    alloc_slave_buffer(&buffers->master);
    alloc_slave_buffer(&buffers->slave);
    buffers->syscall = SLAVE_SYSCALL_NONE;
    buffers->syscall_arg = NULL;
    return buffers;
}

static void
erts_await_slave_init(void)
{
    const off_t barrier_addr = SLAVE_SYM_start_barrier;
    unsigned x, y;
    for (y = 0; y < slave_workgroup.rows; y++) {
	for (x = 0; x < slave_workgroup.cols; x++) {
	    while(1) {
		int core_barrier;
#ifdef DEBUG
		ssize_t ret =
#endif
		e_read(&slave_workgroup, y, x, barrier_addr,
		       &core_barrier, sizeof(core_barrier));
		ASSERT(ret == 4);
		if (core_barrier == 1) break;
		erts_milli_sleep(1);
	    };
	}
    }
}

void
erts_init_slave_command(void)
{
    const off_t buffers_addr = SLAVE_SYM_slave_command_buffers;
    unsigned x, y;
    /* Since we're paranoid, we make sure all the cores are ready */
    erts_await_slave_init();

    num_slaves = slave_workgroup.num_cores;
    slaves = calloc(num_slaves, sizeof(struct slave));
    for (y = 0; y < slave_workgroup.rows; y++) {
	for (x = 0; x < slave_workgroup.cols; x++) {
	    struct slave_command_buffers *buffers = alloc_slave_buffers();
#ifdef DEBUG
	    ssize_t ret =
#endif
	    e_write(&slave_workgroup, y, x, buffers_addr,
		    &buffers, sizeof(buffers));
	    ASSERT(ret == 4);
	    slaves[x + y * slave_workgroup.rows].buffers = buffers;
	}
    }
}

struct slave*
erts_slave_pop_free(void)
{
    int slave;
    for (slave = 0; slave < num_slaves; slave++)
	if (slaves[slave].available) break;
    if (slave == num_slaves) return NULL;
    slaves[slave].available = 0;
    return slaves + slave;
}

void
erts_slave_push_free(struct slave *slave)
{
    slave->available = 1;
}

void
erts_slave_send_command(struct slave *slave, enum slave_command code,
			const void *data, size_t size)
{
    erts_fifo_write_blocking(&slave->buffers->slave, &code, sizeof(code));
    erts_fifo_write_blocking(&slave->buffers->slave, data, size);
}

void *
erts_slave_syscall_arg(struct slave *slave, enum slave_syscall no)
{
    struct slave_command_buffers *buffers = slave->buffers;
    enum slave_syscall actual_no = buffers->syscall;
    ETHR_MEMBAR(ETHR_LoadLoad); /* buffers->syscall -> buffers->syscall_arg */

    if (!slave->pending_syscall || no != actual_no) {
	erl_exit(1, "Trying to get arg of nonpending syscall %d"
		 " (%d is pending)", no, actual_no);
    }
    return buffers->syscall_arg;
}

void
erts_slave_finish_syscall(struct slave *slave, enum slave_syscall no)
{
    struct slave_command_buffers *buffers = slave->buffers;
    enum slave_syscall actual_no = buffers->syscall;

    if (!slave->pending_syscall || no != actual_no) {
	erl_exit(1, "Trying to get arg of nonpending syscall %d"
		 " (%d is pending)", no, actual_no);
    }
    ETHR_MEMBAR(ETHR_StoreStore); /* *buffers->syscall_arg -> buffers->syscall */
    buffers->syscall = SLAVE_SYSCALL_NONE;
    slave->pending_syscall = 0;
}

static int
serve_ready(int i, struct slave_syscall_ready *arg)
{
    erts_slave_push_free(slaves + i);
    if (slaves[i].c_p) {
	/* ETODO: Terminate c_p */
	slaves[i].c_p = NULL;
    }
    return 0;
}

static int
serve_syscalls(int i)
{
    int served = 0;
    struct slave_command_buffers *buffers = slaves[i].buffers;
    enum slave_syscall no;
    void *arg;
    /* This one is already being served asynchronously */
    if (slaves[i].pending_syscall) return 0;

    no = buffers->syscall;
    if (no == SLAVE_SYSCALL_NONE) return 0; /* Avoid the memory barrier */
    ETHR_MEMBAR(ETHR_LoadLoad); /* buffers->syscall -> buffers->syscall_arg */
    arg = buffers->syscall_arg;

    switch (buffers->syscall) {
    case SLAVE_SYSCALL_READY: served = serve_ready(i, arg); break;
    case SLAVE_SYSCALL_BIF:
	erts_slave_serve_bif(slaves + i, arg);
	served = 1;
	break;
    default:
	erl_exit(1, "Cannot serve unrecognized syscall %d from slave %d\n",
		 (int)no, i);
    }

    if (served) {
	ETHR_MEMBAR(ETHR_StoreStore); /* *buffers->syscall_arg -> buffers->syscall */
	buffers->syscall = SLAVE_SYSCALL_NONE;
    } else {
	slaves[i].pending_syscall = 1;
    }
    return served;
}

static int
dispatch_commands(int slave)
{
    struct erl_fifo *fifo = &slaves[slave].buffers->master;
    enum master_command cmd;
    size_t available = erts_fifo_available(fifo);
    if (available < sizeof(enum master_command)) return 0;
    erts_fifo_peek(fifo, &cmd, sizeof(enum master_command));
    available -= sizeof(enum master_command);
    switch (cmd) {
    case MASTER_COMMAND_SETUP: {
	struct master_command_setup msg;
	if (available < sizeof(struct master_command_setup)) break;
	erts_fifo_skip(fifo, sizeof(enum master_command));
	erts_fifo_read_blocking(fifo, &msg, sizeof(struct master_command_setup));
	erts_slave_init_load(&msg);
	return 1;
	break;
    }
    default:
	erl_exit(1, "Cannot pop unrecognized message %d from slave %d fifo\n",
		 (int)cmd, slave);
    }
    return 0;
}

int
erts_dispatch_slave_commands(void)
{
    int i, dispatched = 0;
    for (i = 0; i < num_slaves; i++) {
	dispatched += dispatch_commands(i);
	dispatched += serve_syscalls(i);
    }
    return dispatched;
}
