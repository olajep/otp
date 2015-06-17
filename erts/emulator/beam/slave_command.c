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
static erts_smp_mtx_t freeq_mtx;

static void *
command_thread_loop(void __attribute__((unused)) *arg)
{
    int restart_tries = 0;
    ErtsThrPrgrCallbacks callbacks;
    const off_t buffers_addr = SLAVE_SYM_slave_command_buffers;
    unsigned x, y;
    int sleep_schecule_point = 0;
    int sleep_schedule[] = {
	0,
	1,
	2,
	5,
	10,
	20,
	50,
	100,
	200,
	500,
	1000,
    };

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
    command_thead_esd.current_process = NULL;
    command_thead_esd.ssi
	= erts_alloc_permanent_cache_aligned(ERTS_ALC_T_SCHDLR_SLP_INFO,
					     sizeof(ErtsSchedulerSleepInfo));
    erts_smp_atomic32_init_nob(&command_thead_esd.ssi->flags, 0);
    command_thead_esd.ssi->event = erts_tse_fetch();
    erts_smp_atomic32_init_nob(&command_thead_esd.ssi->aux_work, 0);
    erts_thr_progress_register_managed_thread(NULL, &callbacks, 0);
#ifdef ERTS_ENABLE_LOCK_CHECK
    erts_lc_set_thread_name("slave command thread");
#endif
    erts_proc_register_slave_command_thread(&command_thead_esd);
    erts_smp_mtx_init(&freeq_mtx, "slave_command_freeq_mtx");

 restart:
    while(!slave_has_started_flag) {
	if (erts_thr_progress_update(NULL)) {
	    erts_thr_progress_leader_update(NULL);
	}
	erts_milli_sleep(10);
    }

    if (num_slaves == 0) {
	num_slaves = slave_workgroup.num_cores;
	slaves = calloc(num_slaves, sizeof(struct slave));
	for (y = 0; y < slave_workgroup.rows; y++) {
	    for (x = 0; x < slave_workgroup.cols; x++) {
		struct slave_command_buffers *buffers = alloc_slave_buffers();
		int ix = x + y * slave_workgroup.rows;
		slaves[ix].no = ix;
		slaves[ix].buffers = buffers;
		slaves[ix].dummy_esdp = alloc_slave_scheduler_data();
		erts_smp_mtx_init(&slaves[ix].command_mtx, "slave_command_mtx");
	    }
	}
    } else {
	ASSERT(num_slaves == slave_workgroup.num_cores);
    }
    for (y = 0; y < slave_workgroup.rows; y++) {
	for (x = 0; x < slave_workgroup.cols; x++) {
	    int ix = x + y * slave_workgroup.rows;
	    struct slave_command_buffers *buffers = slaves[ix].buffers;
#ifdef DEBUG
	    ssize_t ret =
#endif
	    e_write(&slave_workgroup, y, x, buffers_addr,
		    &buffers, sizeof(buffers));
	    ASSERT(ret == 4);
	}
    }

    {
	int count = 0;
	/* Await the initial SETUP message */
	while (erts_dispatch_slave_commands() == 0) {
	    erts_milli_sleep(10);
	    if (++count > 200) {
		slave_has_started_flag = 0;
		if (++restart_tries > 5) {
		    erts_fprintf(stderr, "Slaves timed out sending SETUP too "
				 "many times, giving up.\n");
		    erts_stop_slave_io();
		} else {
		    erts_fprintf(stderr, "Slaves timed out sending SETUP, "
				 "restarting.\n");
		    erts_restart_slave_io();
		}
		goto restart;
	    }
	}
    }

    while(erts_slave_online) {
	if (erts_dispatch_slave_commands() == 0) {
	    struct timespec rqtp;
	    /* ETODO: We might need to limit the number of operations to
	     * ensure a good rate of thread progress reports. */
	    if (erts_thr_progress_update(NULL)) {
		erts_thr_progress_leader_update(NULL);
	    }

	    rqtp.tv_sec = 0;
	    rqtp.tv_nsec = sleep_schedule[sleep_schecule_point] * 1000;
	    nanosleep(&rqtp, NULL);
	    sleep_schecule_point
		= MIN(sleep_schecule_point + 1,
		      sizeof(sleep_schedule) / sizeof(*sleep_schedule) - 1);
	} else {
	    sleep_schecule_point = 0;
	}
    }
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
    erts_smp_mtx_lock(&freeq_mtx);
    for (slave = 0; slave < num_slaves; slave++)
	if (slaves[slave].available) break;
    if (slave == num_slaves) {
	erts_smp_mtx_unlock(&freeq_mtx);
	return NULL;
    } else {
	slaves[slave].available = 0;
	erts_smp_mtx_unlock(&freeq_mtx);
	return slaves + slave;
    }
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
    /* ETODO: Use linked list intermediary buffer so we never need to block
     * here. */
    erts_smp_mtx_lock(&slave->command_mtx);
    erts_fifo_write_blocking(&slave->buffers->slave, &code, sizeof(code));
    erts_fifo_write_blocking(&slave->buffers->slave, data, size);
    erts_smp_mtx_unlock(&slave->command_mtx);
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
    if (slaves[i].c_p && !slave_do_exit_process(slaves[i].c_p, arg)) {
	slaves[i].c_p = NULL;
	command_thead_esd.current_process = NULL;
    }

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
    /* buffers->syscall -> buffers->master, buffers->syscall_arg */
    ETHR_MEMBAR(ETHR_LoadLoad);

    /* We need to have served all pending commands before serving a syscall. */
    if (erts_fifo_available(&buffers->master)) return 0;
    arg = buffers->syscall_arg;

    switch (buffers->syscall) {
    case SLAVE_SYSCALL_READY: served = serve_ready(i, arg); break;
    case SLAVE_SYSCALL_BIF:
	served = erts_slave_serve_bif(slaves + i, arg);
	break;
    case SLAVE_SYSCALL_GC:
	erts_slave_serve_gc(slaves + i, arg);
	served = 1;
	break;
    case SLAVE_SYSCALL_BIN:
	erts_slave_serve_bin(slaves + i, arg);
	served = 1;
	break;
    default:
	erl_exit(1, "Cannot serve unrecognized syscall %d from slave %d\n",
		 (int)no, i);
    }

    if (served) {
	/* *buffers->syscall_arg -> buffers->syscall */
	ETHR_MEMBAR(ETHR_StoreStore);
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

#define MESSAGE(TYPE, NAME)					\
    TYPE NAME;							\
    if (available < sizeof(NAME)) return 0;			\
    erts_fifo_skip(fifo, sizeof(enum master_command));		\
    erts_fifo_read_blocking(fifo, &NAME, sizeof(NAME))

    switch (cmd) {
    case MASTER_COMMAND_SETUP: {
	MESSAGE(struct master_command_setup, msg);
	erts_slave_init_load(&msg);
	return 1;
    }
    case MASTER_COMMAND_SETUP_CORE: {
	MESSAGE(struct master_command_setup_core, msg);
	slaves[slave].dummy_esdp->x_reg_array = msg.x_reg_array;
	return 1;
    }
    case MASTER_COMMAND_FREE_MESSAGE: {
	MESSAGE(struct master_command_free_message, msg);
	slave_free_message(slaves + slave, msg.m);
	return 1;
    }
    case MASTER_COMMAND_FREE_HFRAG: {
	MESSAGE(struct master_command_free_hfrag, msg);
	slave_free_message_buffer(slaves + slave, msg.bp);
	return 1;
    }
    case MASTER_COMMAND_REFC: {
	MESSAGE(struct master_command_refc, msg);
	erts_slave_serve_refc(&msg);
	return 1;
    }
    case MASTER_COMMAND_TIMER: {
	MESSAGE(struct master_command_timer, msg);
	erts_slave_serve_timer(slaves + slave, &msg);
	return 1;
    }
    default:
	erl_exit(1, "Cannot pop unrecognized message %d from slave %d fifo\n",
		 (int)cmd, slave);
    }
#undef MESSAGE
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
