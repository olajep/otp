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
#include <e-lib.h>

static int is_leader(void);
static ssize_t internal_write(int, const void*, const size_t);
static int internal_vprintf(char*, va_list);
static int internal_printf(char*, ...);
static int sys_epiphany_printf(char*, va_list);
static void __attribute__((interrupt, section(".data_bank1"))) handl(int);

volatile int goflag = 0;
e_mutex_t global_mutex __attribute__((section(".data_bank0")));

#define OUTBUF_SZ 1024
char outbuf[OUTBUF_SZ];
char * volatile out_start = outbuf;
char * volatile out_end = outbuf;

static int is_leader() {
    // Hardcoded coordinates 32,8
    // ETODO: use workgroup config instead
    return e_get_coreid() == 04010;
}

static ssize_t internal_write(int __attribute__((unused)) fildes,
                              const void *data, const size_t initial_count) {
    size_t count = initial_count;
    e_mutex_lock(0, 0, &global_mutex);

    while (count > 0) {
        char *captured_end = out_end;
        char *captured_start = out_start;
        size_t to_write = count;
        if (captured_end < captured_start && captured_end + to_write > captured_start)
            to_write = captured_start - captured_end;
        if (captured_end + to_write > outbuf + OUTBUF_SZ)
            to_write = OUTBUF_SZ - (outbuf - captured_end);
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

static int internal_vprintf(char *format, va_list args) {
    int count = vsnprintf(NULL, 0, format, args);
    char buf[count+1];
    vsnprintf(buf, count+1, format, args);
    internal_write(STDOUT_FILENO, buf, count);
    return count;
}

static int internal_printf(char *format, ...) {
    va_list args;
    int count;
    va_start(args, format);
    count = internal_vprintf(format, args);
    va_end(args);
    return count;
}

static int sys_epiphany_printf(char *format, va_list args) {
    e_coreid_t id = e_get_coreid();
    unsigned row, col;
    int count;
    if (goflag != 1) {
        // We need the global mutex to be initialised to print, but we mustn't
        // assert or we'll loop infinitely.
        return -1;
    }
    e_coords_from_coreid(id, &row, &col);
    count = internal_printf("[%d,%d] ", row, col);
    // We mustn't hold the lock while calling complex functions like
    // erts_printf_format, since they might rely on printing or stubbed code
    count += erts_printf_format((fmtfn_t)internal_write, NULL, format, args);
    fflush(stdout);
    return count;
}

int
main(int argc, char **argv)
{
    if (is_leader()) {
        e_mutex_init(0, 0, &global_mutex, NULL);
        erts_printf_stdout_func = sys_epiphany_printf;
        erts_printf_stderr_func = sys_epiphany_printf;
        goflag = 1;
    } else {
        while (goflag == 0);
    }
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

    erl_start(argc, argv);
    erts_printf("Terminating normally\n");
    return 0;
}

static void __attribute__((interrupt, section(".data_bank1")))
handl(int __attribute__((unused)) crap) {
    erts_printf("Interrupted! IPEND=%x\n", e_reg_read(E_REG_IPEND));
};
