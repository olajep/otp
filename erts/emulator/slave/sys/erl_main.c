/*
 * %CopyrightBegin%
 * 
 * Copyright Ericsson AB 2000-2009. All Rights Reserved.
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
 */
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#define EPIPHANY_NO_WRITE_REDIRECT
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_printf_format.h"
#include "epiphany.h"
#include <e-lib.h>

// erl_epiphany_sys.h redeclares write(int, const void*, size_t) with the same
// symbol as internal_write. We can't make it static, or that won't work.
ssize_t internal_write(int, const void*, size_t);

static int in_emulator(void);
static int is_leader(void);
static void grab_barrier(void);
static int sys_epiphany_printf(char*, va_list);
EPIPHANY_SRAM_FUNC static void __attribute__((interrupt)) handl(int);

EPIPHANY_SRAM_DATA e_mutex_t global_mutex;

#define OUTBUF_SZ 1024
char outbuf[OUTBUF_SZ];
char * volatile out_start = outbuf;
char * volatile out_end = outbuf;

static int in_emulator() {
    return e_group_config.group_rows == 0
        && e_group_config.group_cols == 0;
}

static int is_leader() {
    if (in_emulator()) {
        // Hardcoded coordinates 32,8
        return e_get_coreid() == 04010;
    } else {
        return e_group_config.core_row == 0
            && e_group_config.core_col == 0;
    }
}

ssize_t internal_write(int __attribute__((unused)) fildes,
		       const void *data, const size_t initial_count) {
    size_t count = initial_count;
    if (in_emulator()) return write(STDOUT_FILENO, data, count);

    e_mutex_lock(0, 0, &global_mutex);

    while (count > 0) {
	char *captured_end = out_end;
	char *captured_start = out_start;
	size_t to_write = count;
	if (captured_end < captured_start && captured_end + to_write >= captured_start)
	    to_write = captured_start - captured_end - 1;
	if (captured_end + to_write > outbuf + OUTBUF_SZ)
	    to_write = (outbuf + OUTBUF_SZ) - captured_end;
	if (captured_end + to_write == outbuf + OUTBUF_SZ
	    && captured_start == outbuf)
	    to_write -= 1;
	memcpy(captured_end, data, to_write);
	captured_end += to_write;
	data += to_write;
	count -= to_write;
	if (captured_end == outbuf + OUTBUF_SZ)
	    captured_end = outbuf;
	out_end = captured_end;
    }

    e_mutex_unlock(0, 0, &global_mutex);
    return initial_count;
}

EPIPHANY_SRAM_DATA static char in_line;

struct print_buffer {
    int count;
    char buffer[512];
};

static int buffer_write(void *arg, char *data, size_t count) {
    struct print_buffer *buf = (struct print_buffer*)arg;
    size_t to_write = MIN(count, sizeof(buf->buffer) - buf->count);
    memcpy(buf->buffer + buf->count, data, to_write);
    buf->count += to_write;
    return count; // Let's lie so nobody loops forever
}

static int sys_epiphany_printf(char *format, va_list args) {
    struct print_buffer buf = { .count = 0 };
    if (!in_line) {
	buf.count +=
            snprintf(buf.buffer + buf.count, sizeof(buf.buffer) - buf.count,
		     "[%d] ", epiphany_coreno());
    }
    // We mustn't hold the lock while calling complex functions like
    // erts_printf_format, since they might rely on printing or stubbed
    // code. Thus, we build the entire output first and print it all in one go.
    erts_printf_format(buffer_write, &buf, format, args);
    in_line = format[strlen(format)-1] != '\n';
    if (!in_line) buffer_write(&buf, "\r", 1);
    internal_write(0, buf.buffer, buf.count);
    return buf.count;
}

#ifdef ERTS_SMP
static int slave_flag = 0;
#endif

static volatile int start_barrier __attribute__((section(".data_bank0")));
static void grab_barrier() {
    int row, col;
    const int rows = e_group_config.group_rows;
    const int cols = e_group_config.group_cols;
    if (in_emulator()) {
	// The barrier does not fix the problem in the emulator, and we don't
	// know the workgroup size anyway (the fields above are always 0).
	return;
    }
    start_barrier = 1;
    for (row = 0; row < rows; row++) {
	for (col = 0; col < cols; col++) {
	    volatile int *other
		= e_get_global_address(row, col, (void*)&start_barrier);
	    while (*other == 0);
	}
    }
}

struct workgroup_coords epiphany_workgroup_origin(void) {
    if (!in_emulator()) {
	struct workgroup_coords origin = { .row = 0, .col = 0 };
	return origin;
    } else {
	// The emulator does not set e_group_config. Assume a P16 4x4 workgroup.
	struct workgroup_coords origin = { .row = 32, .col = 8 };
#ifdef DEBUG
	e_coreid_t id = e_get_coreid();
	unsigned row, col;
	e_coords_from_coreid(id, &row, &col);
	ASSERT(32 <= row && row < 36);
	ASSERT(8 <= col && col < 12);
#endif
	return origin;
    }
}

int epiphany_coreno(void) {
    e_coreid_t id = e_get_coreid();
    unsigned row, col;
    struct workgroup_coords origin = epiphany_workgroup_origin();
    struct workgroup_dimens dimens = epiphany_workgroup_dimens();
    e_coords_from_coreid(id, &row, &col);

    return (row - origin.row) * dimens.cols + (col - origin.col);
}

struct workgroup_dimens epiphany_workgroup_dimens(void) {
    if (!in_emulator()) {
	struct workgroup_dimens dimens = {
	    .rows = e_group_config.group_rows,
	    .cols = e_group_config.group_cols,
	};
	return dimens;
    } else {
	// The emulator does not set e_group_config. Assume a P16 4x4 workgroup.
	struct workgroup_dimens dimens = { .rows = 4, .cols = 4, };
	return dimens;
    }
}

int epiphany_workgroup_size(void) {
    struct workgroup_dimens dimens = epiphany_workgroup_dimens();
    return dimens.rows * dimens.cols;
}

int epiphany_in_dram(void *addr) {
    return 0x8e000000 <= (unsigned)addr && (unsigned)addr < 0x90000000;
}

int epiphany_sane_address(void *addrp) {
    e_coreid_t owning;
    unsigned row, col, min_row, min_col, max_row, max_col;
    unsigned addr = (unsigned)addrp;

    // Shared DRAM
    if (epiphany_in_dram(addrp)) return 1;

    // In memory space of core in workgroup
    owning = addr >> (32 - 6);
    e_coords_from_coreid(owning, &row, &col);
    min_row = e_group_config.group_row;
    max_row = min_row + e_group_config.group_rows;
    min_col = e_group_config.group_col;
    max_col = min_col + e_group_config.group_cols;
    if ((row == 0 && col == 0)
	|| ((min_row <= row && row < max_row)
	    && (min_col <= col && col < max_col))) {
	unsigned corespc = addr & (1024 * 1024);
	// Local SRAM
	if (corespc < 32 * 1024) return 1;

	// Memory-mapped registers
	if (corespc >= 0x000F0000) return 1;
    }
    return 0;
}

int
main(int argc, char **argv)
{
    // Dram behaves strangely until all cores have started
    grab_barrier();

    erts_printf_stdout_func = sys_epiphany_printf;
    erts_printf_stderr_func = sys_epiphany_printf;

    erts_printf("Hi from epiphany\n");
    e_irq_attach(E_SYNC, handl);
    e_irq_attach(E_SW_EXCEPTION, handl);
    e_irq_attach(E_MEM_FAULT, handl);
    e_irq_attach(E_TIMER0_INT, handl);
    e_irq_attach(E_TIMER1_INT, handl);
    e_irq_attach(E_DMA0_INT, handl);
    e_irq_attach(E_DMA1_INT, handl);
    e_irq_attach(E_USER_INT, handl);

    // We start completely masked from interrupts, it seems
    e_reg_write(E_REG_IMASK, 0);

    // Bits 2 and 3 protects the lower and upper half of data_bank1 from writing
    e_reg_write(E_REG_MEMPROTECT,
		e_reg_read(E_REG_MEMPROTECT)
		| (1 << 2)
		| (1 << 3));

    if (is_leader()) {
        erl_start(argc, argv);
    } else {
#ifdef ERTS_SMP
	unsigned sched_no = epiphany_coreno();
        while (slave_flag == 0);
	erts_printf("Scheduler %d flagged!\n", sched_no);
	enter_scheduler(sched_no);
#endif
    }

    erts_printf("Terminating normally\n");
    return 0;
}

#ifdef ERTS_SMP
void erts_sys_main_thread() {
    // We want to run BEAM on the main thread
    enter_scheduler(0);
}

void erts_start_schedulers() {
    slave_flag = 1;
}
#endif

EPIPHANY_SRAM_FUNC static void __attribute__((interrupt))
handl(int __attribute__((unused)) crap) {
    erts_printf("Interrupted! IPEND=%x\n", e_reg_read(E_REG_IPEND));
};
