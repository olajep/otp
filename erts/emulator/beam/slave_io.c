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

#ifdef DEBUG
#  define VERIFY(E) ERTS_ASSERT(E)
#else
#  define VERIFY(E) ((void)(E))
#endif

static int map_shm(void);
static int spoof_mmap(void);
static int start_pump_thread(void);
static void *pump_thread_loop(void *);

static struct erl_fifo * const slave_io_fifo = (void*)SLAVE_SYM_slave_io_fifo;

/*
 * Copies data from the log output ringbuffer to stdout.
 * Returns the number of bytes copied, or -1 if an error occurred.
 */
int erts_slave_io_pump(void) {
    size_t to_pump = erts_fifo_available(slave_io_fifo);
    char buffer[to_pump];
    ssize_t written;

    erts_fifo_peek(slave_io_fifo, buffer, to_pump);
    written = write(STDERR_FILENO, buffer, to_pump);
    if (written == -1) {
	if (errno == EAGAIN) {
	    written = 0;
	} else {
	    perror("erts_slave_io_pump(), write()");
	    return -1;
	}
    }
    ASSERT(written >= 0 && written <= (ssize_t)to_pump);
    erts_fifo_skip(slave_io_fifo, written);
    return written;
}

struct desired_map {
    void *virt_addr;
    unsigned phy_addr;
    unsigned size;
};

e_epiphany_t slave_workgroup;
#define ROWS 4
#define COLS 4
#define O_ROW 040
#define O_COL 010

static int get_desired_maps(struct desired_map **mapsp, int phy_is_ephy) {
    int count = ROWS * COLS + 1;
    unsigned row, col;
    struct desired_map *maps = *mapsp
	= erts_alloc(ERTS_ALC_T_TMP, count * sizeof(*maps));

    /* ETODO: parse the HDF ourselves (we can't use the e_platform symbol since
     * the library is dynamically linked) */
    unsigned phy_base  = phy_is_ephy ? 0x8e000000 : 0x3e000000;
    unsigned size      = 0x02000000;
    unsigned ephy_base = 0x8e000000;
#ifdef DEBUG
    void *slave_data_end = (void*)SLAVE_SYM_end;
    void *slave_heap_start = (void*)SLAVE_SYM___heap_start;
    ASSERT((void*)ephy_base < slave_data_end);
    ASSERT(slave_data_end < slave_heap_start);
    ASSERT(slave_heap_start < (void*)(ephy_base + size));
#endif

    maps->virt_addr = (void*)ephy_base;
    maps->phy_addr = phy_base;
    maps->size = size;
    maps++;

    for (row = 0; row < ROWS; row++) {
	for (col = 0; col < COLS; col++) {
	    e_coreid_t coreid = ((col + O_COL) | ((row + O_ROW) << 6));
	    maps->virt_addr = (void*)((coreid << 20) | 0x0);
	    maps->phy_addr  =         (coreid << 20) | 0x0;
	    maps->size      = 0x8000;
	    maps++;
	 }
    }

    ASSERT(maps == *mapsp + count);
    return count;
}

static int memfd = 0;

/*
 * Maps the memory area shared with the epiphany chip to the same address as the
 * chip sees it at.
 * Returns 0 on success and -1 on failure.
 */
static int map_shm(void) {
    void *ret;
    int i, count, phy_is_ephy = 0;
    struct desired_map *maps;

    memfd = open("/dev/epiphany/mesh0", O_RDWR | O_SYNC);
    if (memfd >= 0) {
	/* use epiphany address space as offset */
	phy_is_ephy = 1;
    }
    if (memfd == -1) {
        /* Old epiphany driver */
        memfd = open("/dev/epiphany", O_RDWR | O_SYNC);
    }
    if (memfd == -1) {
	/* The old way, for devices without the Epiphany kernel driver */
	memfd = open("/dev/mem", O_RDWR | O_SYNC);
    }
    if (memfd == -1) return -1;

    count = get_desired_maps(&maps, phy_is_ephy);

    for (i = 0; i < count; i++) {
	/* We avoid using MAP_FIXED because we'd rather know if there is another
	 * mapping in the way than overwrite it. */
	ret = mmap(maps[i].virt_addr, maps[i].size,
		   PROT_READ|PROT_WRITE, MAP_SHARED,
		   memfd, maps[i].phy_addr);
	if (ret == MAP_FAILED) {
	    perror("mmap");
	    fprintf(stderr, "%s: Tried to map %p to %#x, size %#x\n",__func__,
		    maps[i].virt_addr, maps[i].phy_addr, maps[i].size);
	    goto cleanup_exit;
	}
	if (ret != maps[i].virt_addr) {
	    fprintf(stderr, "%s: mmap: wanted %p, got %p\n", __func__,
		    maps[i].virt_addr, ret);
	    /* So this mmap is cleaned up too */
	    maps[i++].virt_addr = ret;
	    goto cleanup_exit;
	}
    }

    erts_free(ERTS_ALC_T_TMP, maps);
    return 0;

 cleanup_exit:
    for (i--; i >= 0; i--) {
	munmap(maps[i].virt_addr, maps[i].size);
    }
    erts_free(ERTS_ALC_T_TMP, maps);
    return -1;
}

static void submit_smem_to_slave_alloc(void) {
    void *slave_data_end = (void*)SLAVE_SYM_end;
    void *slave_heap_start = (void*)SLAVE_SYM___heap_start;
    erl_slave_alloc_submit(slave_data_end, slave_heap_start - slave_data_end);
}

/*
 * We reserve the shared memory segment when we're not actually using the slave
 * emulator so issues with the mmap will be more apparent.
 */
static int spoof_mmap(void) {
    void *ret;
    int i, count, result = 0;
    struct desired_map *maps;

    count = get_desired_maps(&maps, 0);

    for (i = 0; i < count; i++) {
	ret = mmap(maps[i].virt_addr, maps[i].size,
		   PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS,
		   0, 0);
	if (ret == MAP_FAILED) {
	    perror("mmap");
	    result = -1;
	    /* No reason not to just barrel on, since we're only doing this to guard
	     * against mistakes. */
	    continue;
	}
	if (ret != maps[i].virt_addr) {
	    fprintf(stderr, "%s: mmap: wanted %p, got %p\n", __func__,
		    maps[i].virt_addr, ret);
	    result = -1;
	}
    }
    erts_free(ERTS_ALC_T_TMP, maps);
    return result;
}

static erts_tid_t pump_thread_tid;

/*
 * The flag is set before the event. If the flag is 1, a reset is requested. If
 * the flag is 0, startup was successfull, and the pump thread exits.
 */
static int restart_slave_io_flag = 0;
static ethr_event pump_thread_event;

static int start_pump_thread(void) {
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;
    if (ethr_thr_create(&pump_thread_tid, pump_thread_loop, NULL, &opts)) {
	return 1;
    }
    return 0;
}

int erts_slave_online = 0;
static int ehal_initialised = 0;

static void *
pump_thread_loop(void __attribute__((unused)) *arg)
{
    int restart_tries = 0;
    char *binary;
    e_platform_t platform;

    VERIFY(!ethr_event_init(&pump_thread_event));

    if (e_init(NULL) != E_OK) { return NULL; }
    ehal_initialised = 1;

    if (e_get_platform_info(&platform) != E_OK) {
	perror("Not loading slave emulator: e_get_platform_info");
	erts_stop_slave_io();
	return NULL;
    }

 restart_2:
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

    {
	int count = 0;
	const off_t barrier_addr = SLAVE_SYM_start_barrier;
	unsigned x, y;
	for (y = 0; y < slave_workgroup.rows; y++) {
	    for (x = 0; x < slave_workgroup.cols; x++) {
		while(1) {
		    int core_barrier;
		    VERIFY(e_read(&slave_workgroup, y, x, barrier_addr,
				  &core_barrier, sizeof(core_barrier)) == 4);
		    if (core_barrier == 1) break;
		    while (erts_slave_io_pump() != 0);

		    count++;
		    if (count > 100) {
			if (++restart_tries > 5) {
			    erts_fprintf(stderr, "Couldn't start slaves\n");
			    erts_stop_slave_io();
			    return NULL;
			}
			erts_fprintf(stderr, "Timed out waiting for slaves to "
				     "come online. Resetting.\n");
			goto restart;
		    }
		    erts_milli_sleep(1);
		}
	    }
	}
    }

    ethr_event_reset(&pump_thread_event);
    erts_signal_slave_command();

    ethr_event_wait(&pump_thread_event);
    if (!restart_slave_io_flag) {
	return NULL;
    }
    /* else fallthrough; */

 restart:
    if (e_close(&slave_workgroup) != E_OK) {
	erts_stop_slave_io();
	return NULL;
    }
    goto restart_2;
}

void erts_init_slave_io(void) {
    // We make things easy for ourselves by mapping in the shared memory area at
    // *the same* address as it is observed by the Epiphany chip.
    if (getenv("SLAVE_BINARY") == NULL || map_shm()) {
	erl_slave_alloc_fallback();
	spoof_mmap();
	return;
    }
    submit_smem_to_slave_alloc();

    erts_slave_online = 1;
    if (start_pump_thread()) {
	erl_exit(1, "Could not spin up pump thread\n");
    }
}

void erts_restart_slave_io(void) {
    restart_slave_io_flag = 1;
    ethr_event_set(&pump_thread_event);
}

void erts_finish_slave_io(void) {
    restart_slave_io_flag = 0;
    ethr_event_set(&pump_thread_event);
}

void erts_stop_slave_io(void) {
    int ret;
    erts_slave_online = 0;

    if (!ehal_initialised) return;
    ret = e_finalize();
    if (ret != E_OK) perror("e_finalize");
    ehal_initialised = 0;
}
