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

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <assert.h>
#include <sys/mman.h>
#include <time.h>

// These cause warnings with -Wstrict-prototypes
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-prototypes"
#include <e-hal.h>
#include <e-loader.h>
#pragma GCC diagnostic pop

#include "sys.h"
#include "slave_syms.h"
#include "erl_slave_io.h"

static int map_shm(void);
static int spoof_mmap(void);
static int start_pump_thread(void);
static void *pump_thread_loop(void *);
static int pump_output(void);

#define OUTBUF_SZ 1024
static char * const outbuf = (char*) SLAVE_SYM_outbuf;
static char * volatile * const out_start = (char * volatile *)SLAVE_SYM_out_start;
static char * volatile * const out_end = (char * volatile *)SLAVE_SYM_out_end;

/* Copies data from the log output ringbuffer to stdout.
 * Returns the number of bytes copied, or -1 if an error occurred.
 */
static int pump_output(void) {
    int pumped = 0;
    ssize_t written;

    char *captured_start = *out_start;
    char *captured_end = *out_end;
    ASSERT(outbuf <= captured_start && captured_start < outbuf + OUTBUF_SZ);
    ASSERT(outbuf <= captured_end && captured_end < outbuf + OUTBUF_SZ);

    if (captured_start > captured_end) {
        written = write(STDOUT_FILENO, captured_start, OUTBUF_SZ - (outbuf - captured_start));
        if (written == -1) goto write_error;
        pumped += written;
        captured_start += written;
        if (captured_start == outbuf + OUTBUF_SZ) captured_start = outbuf;
    }
    written = write(STDOUT_FILENO, captured_start, captured_end - captured_start);
    if (written == -1) goto write_error;
    pumped += written;
    *out_start = captured_start + written;
    return pumped;

 write_error:
    perror("write");
    return -1;
}

static int memfd = 0;

/* Maps the memory area shared with the epiphany chip to the same address as the
 * chip sees it at.
 * Returns 0 on success and -1 on failure.
 */
static int map_shm(void) {
    void *ret;
    // ETODO: parse the HDF ourselves (we can't use the e_platform symbol or
    // *very* bad things happens)
    unsigned phy_base  = 0x3e000000;
    unsigned size      = 0x02000000;
    unsigned ephy_base = 0x8e000000;

    // The old way, for devices without the Epiphany kernel driver
    memfd = open("/dev/mem", O_RDWR | O_SYNC);
    if (memfd == -1) {
        perror("open: /dev/mem");
        return -1;
    }

    printf("Mapping 0x%x+0x%x to 0x%x\n", phy_base, size, ephy_base);
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
    return 0;
}

// We reserve the shared memory segment when we're not actually using the slave
// emulator so issues with the mmap will be more apparent.
static int spoof_mmap(void) {
    void *ret;
    unsigned size      = 0x02000000;
    unsigned ephy_base = 0x8e000000;

    printf("Reserving 0x%x+0x%x\n", ephy_base, size);
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
    return 0;
}

erts_tid_t pump_thread_tid;

static int start_pump_thread(void) {
    ethr_thr_opts opts = ETHR_THR_OPTS_DEFAULT_INITER;
    if (ethr_thr_create(&pump_thread_tid, pump_thread_loop, NULL, &opts)) {
        return 1;
    }
    return 0;
}

static int pump_thread_flag = 1;

static void *pump_thread_loop(void *arg) {
    erts_printf("Hi from pump thread\n");
    while(pump_thread_flag) {
        if (pump_output() == 0) {
            struct timespec ms = {0, 1000};
            nanosleep(&ms, NULL);
        }
    }
    return NULL;
}

static int slave_emu_online = 0;
static e_epiphany_t workgroup;

#define ROWS 1
#define COLS 1
void erts_init_slave_io(void) {
    char *binary;
    // We make things easy for ourselves by mapping in the shared memory area at
    // *the same* address as it is observed by the Epiphany chip.
    if (map_shm()) {
        spoof_mmap();
        return;
    }

    e_set_host_verbosity(H_D1);
    if (e_init(NULL) != E_OK) { return; }
    slave_emu_online = 1;

    if (e_reset_system() != E_OK) {
        perror("Not loading slave emulator: e_reset_system");
        erts_stop_slave_io();
        return;
    }
    printf("Opening %dx%d workgroup\n", ROWS, COLS);
    if (e_open(&workgroup, 0, 0, ROWS, COLS) != E_OK) {
        perror("Not loading slave emulator: e_open");
        erts_stop_slave_io();
        return;
    }

    binary = getenv("SLAVE_BINARY");
    if (binary == NULL) {
        fprintf(stderr, "Not loading slave emulator: "
                "SLAVE_BINARY environment variable unset\n");
        return;
    }

    printf("Loading and starting program\n");
    if (e_load_group(binary, &workgroup, 0, 0, ROWS, COLS, E_TRUE) != E_OK) {
        perror("Not loading slave emulator: e_load");
        erts_stop_slave_io();
        return;
    }

    printf("Slave emulator online\n");
    if (start_pump_thread()) {
        fprintf(stderr, "Could not spin up pump thread, killing slaves\n");
        if (e_reset_system() != E_OK)
            perror("Could not stop slave: e_reset_system");
        erts_stop_slave_io();
        return;
    }
}

void erts_stop_slave_io(void) {
    int ret;
    if (!slave_emu_online) return;

    pump_thread_flag = 0;
    if (ethr_thr_join(pump_thread_tid, NULL)) {
        fprintf(stderr, "Could not join slave IO pump thread!\n");
    }

    if (memfd > 0) {
        ret = close(memfd);
        if (ret != 0) perror("free: memfd");
    }
    
    ret = e_finalize();
    if (ret != E_OK) perror("e_finalize");

    slave_emu_online = 0;
}
