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

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/mman.h>

#include "sys.h"
#include "slave_fifo.h"
#include "slave_syms.h"
#include "slave_io.h"
#include "slave_command.h"
#include "slave_alloc.h"

static int map_shm(void);
static int spoof_mmap(void);
static int start_pump_thread(void);
static void *pump_thread_loop(void *);
static int pump_output(void);

static struct erl_fifo * const slave_io_fifo = (void*)SLAVE_SYM_slave_io_fifo;

/*
 * Copies data from the log output ringbuffer to stdout.
 * Returns the number of bytes copied, or -1 if an error occurred.
 */
static int pump_output(void) {
    size_t to_pump = erts_fifo_available(slave_io_fifo);
    char buffer[to_pump];
    ssize_t written;

    erts_fifo_read_blocking(slave_io_fifo, buffer, to_pump);
    written = write(STDOUT_FILENO, buffer, to_pump);
    if (written == -1) {
	perror("write");
	return -1;
    }
    ASSERT((ssize_t)to_pump == written);
    return written;
}

static int memfd = 0;

/*
 * Maps the memory area shared with the epiphany chip to the same address as the
 * chip sees it at.
 * Returns 0 on success and -1 on failure.
 */
static int map_shm(void) {
    void *ret;
    /* ETODO: parse the HDF ourselves (we can't use the e_platform symbol since
     * the library is dynamically linked) */
    unsigned phy_base  = 0x3e000000;
    unsigned size      = 0x02000000;
    unsigned ephy_base = 0x8e000000;
    void *slave_data_end = (void*)SLAVE_SYM_end;
    void *slave_heap_start = (void*)SLAVE_SYM___heap_start;
    ASSERT((void*)ephy_base < slave_data_end);
    ASSERT(slave_data_end < slave_heap_start);
    ASSERT(slave_heap_start < (void*)(ephy_base + size));

    memfd = open("/dev/epiphany", O_RDWR | O_SYNC);
    if (memfd == -1) {
	/* The old way, for devices without the Epiphany kernel driver */
	memfd = open("/dev/mem", O_RDWR | O_SYNC);
    }
    if (memfd == -1) return -1;

    // We avoid using MAP_FIXED because we'd rather know if there is another
    // mapping in the way than overwrite it.
    ret = mmap((void*)ephy_base, size,
	       PROT_READ|PROT_WRITE, MAP_SHARED,
	       memfd, phy_base);
    if (ret == MAP_FAILED) {
	perror("mmap");
	return -1;
    }
    if (ret != (void*)ephy_base) {
	fprintf(stderr, "mmap: wanted 0x%x, got 0x%x\n",
		(unsigned)ephy_base, (unsigned)ret);
	return -1;
    }
    erl_slave_alloc_submit(slave_data_end, slave_heap_start - slave_data_end);
    return 0;
}

// We reserve the shared memory segment when we're not actually using the slave
// emulator so issues with the mmap will be more apparent.
static int spoof_mmap(void) {
    void *ret;
    unsigned size      = 0x02000000;
    unsigned ephy_base = 0x8e000000;

    ret = mmap((void*)ephy_base, size,
	       PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS,
	       0, 0);
    if (ret == MAP_FAILED) {
	perror("mmap");
	return -1;
    }
    if (ret != (void*)ephy_base) {
	fprintf(stderr, "mmap: wanted 0x%x, got 0x%x\n",
		(unsigned)ephy_base, (unsigned)ret);
	return -1;
    }
    erl_slave_alloc_fallback();
    return 0;
}

static erts_tid_t pump_thread_tid;

static int start_pump_thread(void) {
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;
    if (ethr_thr_create(&pump_thread_tid, pump_thread_loop, NULL, &opts)) {
	return 1;
    }
    return 0;
}

int erts_slave_online = 0;
static int ehal_initialised = 0;
e_epiphany_t slave_workgroup;

#define ROWS 4
#define COLS 4
static void *pump_thread_loop(void __attribute__((unused)) *arg) {
    char *binary;
    e_platform_t platform;

    if (e_init(NULL) != E_OK) { return NULL; }
    ehal_initialised = 1;

    if (e_get_platform_info(&platform) != E_OK) {
	perror("Not loading slave emulator: e_get_platform_info");
	erts_stop_slave_io();
	return NULL;
    }

    if (e_reset_system() != E_OK) {
	perror("Not loading slave emulator: e_reset_system");
	erts_stop_slave_io();
	return NULL;
    }
    if (e_open(&slave_workgroup, 0, 0, ROWS, COLS) != E_OK) {
	perror("Not loading slave emulator: e_open");
	erts_stop_slave_io();
	return NULL;
    }

    binary = getenv("SLAVE_BINARY");
    if (binary == NULL) {
	fprintf(stderr, "Not loading slave emulator: "
		"SLAVE_BINARY environment variable unset\n");
	erts_stop_slave_io();
	return NULL;
    }

    if (e_load_group(binary, &slave_workgroup, 0, 0, ROWS, COLS, E_TRUE) != E_OK) {
	perror("Not loading slave emulator: e_load");
	erts_stop_slave_io();
	return NULL;
    }

    erts_signal_slave_command();

    while(1) {
	if (pump_output() == 0) {
	    /* ETODO: Exponential sleep duration increase */
	    erts_milli_sleep(10);
	}
    }
    return NULL;
}

void erts_init_slave_io(void) {
    // We make things easy for ourselves by mapping in the shared memory area at
    // *the same* address as it is observed by the Epiphany chip.
    if (getenv("SLAVE_BINARY") == NULL || map_shm()) {
	spoof_mmap();
	return;
    }

    erts_slave_online = 1;
    if (start_pump_thread()) {
	fprintf(stderr, "Could not spin up pump thread\n");
	erts_stop_slave_io();
    }
}

void erts_stop_slave_io(void) {
    int ret;
    erts_slave_online = 0;

    if (memfd > 0) {
        const unsigned size      = 0x02000000;
        const unsigned ephy_base = 0x8e000000;
        ret = munmap((void*)ephy_base, size);
	if (ret != 0) perror("munmap");
	ret = close(memfd);
	if (ret != 0) perror("close: memfd");
        spoof_mmap();
    }

    if (!ehal_initialised) return;
    ret = e_finalize();
    if (ret != E_OK) perror("e_finalize");
    ehal_initialised = 0;
}
