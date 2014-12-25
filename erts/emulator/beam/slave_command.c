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
#include "erl_thr_progress.h"
#include "slave_syms.h"
#include "slave_io.h"
#include "slave_command.h"
#include "slave_load.h"
#include "slave_bif.h"
#include "slave_process.h"

static int num_slaves = 0;
static struct slave *slaves;
#define SLAVE_COMMAND_BUFFER_SIZE 512

static void
alloc_slave_buffer(struct erl_fifo *buffer)
{
    void *data = erts_alloc(ERTS_ALC_T_SLAVE_COMMAND,
			    SLAVE_COMMAND_BUFFER_SIZE);
    ASSERT(data);
    erts_fifo_init(buffer, data, SLAVE_COMMAND_BUFFER_SIZE);
}

static struct slave_command_buffers *
alloc_slave_buffers(void)
{
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

static ErtsSchedulerData *
alloc_slave_scheduler_data(void)
{
    ErtsSchedulerData *esdp = erts_alloc(ERTS_ALC_T_SCHDLR_DATA,
					 sizeof(ErtsSchedulerData));
    memzero(esdp, sizeof(*esdp));
    esdp->cpu_id = -1;

    return esdp;
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

		/* Since we're already a managed thread, we're required to
		 * frequently report progress */
		if (erts_thr_progress_update(NULL)) {
		    erts_thr_progress_leader_update(NULL);
		}
		erts_milli_sleep(1);
	    };
	}
    }
}

static int tpr_wait_flag;
static erts_smp_mtx_t tpr_mtx;
static erts_smp_cnd_t tpr_cnd;

static void
tpr_wakeup(void __attribute__((unused)) *unused)
{
    erts_smp_mtx_lock(&tpr_mtx);
    tpr_wait_flag = 0;
    erts_smp_cnd_signal(&tpr_cnd);
    erts_smp_mtx_unlock(&tpr_mtx);
}

static void
tpr_prep_wait(void __attribute__((unused)) *unused)
{
    erts_smp_mtx_lock(&tpr_mtx);
    tpr_wait_flag = 1;
    erts_smp_mtx_unlock(&tpr_mtx);
}

static void
tpr_fin_wait(void __attribute__((unused)) *unused)
{
    erts_smp_mtx_lock(&tpr_mtx);
    tpr_wait_flag = 0;
    erts_smp_mtx_unlock(&tpr_mtx);
}

static void
tpr_wait(void __attribute__((unused)) *unused)
{
    erts_smp_mtx_lock(&tpr_mtx);
    while (tpr_wait_flag)
	erts_smp_cnd_wait(&tpr_cnd, &tpr_mtx);
    erts_smp_mtx_unlock(&tpr_mtx);
}

static ErtsSchedulerData command_thead_esd;
static volatile int slave_has_started_flag = 0;

static void *
command_thread_loop(void __attribute__((unused)) *arg)
{
    ErtsThrPrgrCallbacks callbacks;
    const off_t buffers_addr = SLAVE_SYM_slave_command_buffers;
    unsigned x, y;

    /* We *need* to register as a managed thread immediately, as we are blocking
     * startup of the emulator */
    erts_smp_mtx_init(&tpr_mtx, "slave_command_tpr_mtx");
    erts_smp_cnd_init(&tpr_cnd);
    callbacks.arg = NULL;
    callbacks.wakeup = tpr_wakeup;
    callbacks.prepare_wait = tpr_prep_wait;
    callbacks.wait = tpr_wait;
    callbacks.finalize_wait = tpr_fin_wait;
    memzero(&command_thead_esd, sizeof(command_thead_esd));
    command_thead_esd.cpu_id = -1;
    command_thead_esd.ssi
	= erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_SLP_INFO,
					     sizeof(ErtsSchedulerSleepInfo));
#ifdef ERTS_SMP
    erts_smp_atomic32_init_nob(&command_thead_esd.ssi->flags, 0);
    command_thead_esd.ssi->event = erts_tse_fetch();
#endif
    erts_smp_atomic32_init_nob(&command_thead_esd.ssi->aux_work, 0);
    erts_thr_progress_register_managed_thread(NULL, &callbacks, 0);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("slave command thread");
#endif
    erts_proc_register_slave_command_thread(&command_thead_esd);

    while(!slave_has_started_flag) {
	if (erts_thr_progress_update(NULL)) {
	    erts_thr_progress_leader_update(NULL);
	}
	erts_milli_sleep(10);
    }

    /* Since we're paranoid, we make sure all the cores are ready. */
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
	    slaves[x + y * slave_workgroup.rows].dummy_esdp
		= alloc_slave_scheduler_data();
	}
    }

    while(1) {
	if (erts_dispatch_slave_commands() == 0) {
	    /* ETODO: We might need to limit the number of operations to
	     * ensure a good rate of thread progress reports. */
	    if (erts_thr_progress_update(NULL)) {
		erts_thr_progress_leader_update(NULL);
	    }
	    erts_milli_sleep(1);
	}
    }
    erl_exit(1, "Slave commander thread exited");
    return NULL;
}

static ethr_tid command_thread_tid;

void
erts_init_slave_command(void)
{
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;
    if (0 != ethr_thr_create(&command_thread_tid,
			     command_thread_loop,
			     NULL,
			     &opts)) {
	erl_exit(1, "Failed to create slave commander thread\n");
    }
}

void
erts_signal_slave_command(void)
{
    slave_has_started_flag = 1;
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
    if (slaves[i].c_p && !slave_do_exit_process(slaves[i].c_p, arg))
	slaves[i].c_p = NULL;

    if (!slaves[i].c_p) {
	erts_slave_push_free(slaves + i);
	slaves[i].pending_syscall = 1;
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
    case SLAVE_SYSCALL_GC:
	erts_slave_serve_gc(slaves + i, arg);
	served = 1;
	break;
    default:
	erl_exit(1, "Cannot serve unrecognized syscall %d from slave %d\n",
		 (int)no, i);
    }

    if (served) {
	ETHR_MEMBAR(ETHR_StoreStore); /* *buffers->syscall_arg -> buffers->syscall */
	buffers->syscall = SLAVE_SYSCALL_NONE;
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
	if (available < sizeof(msg)) break;
	erts_fifo_skip(fifo, sizeof(enum master_command));
	erts_fifo_read_blocking(fifo, &msg, sizeof(msg));
	erts_slave_init_load(&msg);
	return 1;
	break;
    }
    case MASTER_COMMAND_FREE_MESSAGE: {
	struct master_command_free_message msg;
	if (available < sizeof(msg)) break;
	erts_fifo_skip(fifo, sizeof(enum master_command));
	erts_fifo_read_blocking(fifo, &msg, sizeof(msg));
	slave_free_message(slaves + slave, msg.m);
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
