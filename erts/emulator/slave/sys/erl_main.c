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

volatile int goflag = 0;
e_mutex_t global_mutex __attribute__((section(".data_bank0")));

#define OUTBUF_SZ 1024
char outbuf[OUTBUF_SZ];
char * volatile out_start = outbuf;
char * volatile out_end = outbuf;

int is_leader() {
    ASSERT(04010 == 0xfaa);
    return e_get_coreid() == 04010;
}

ssize_t internal_write(int __attribute__((unused)) fildes, const void *data,
                       const size_t initial_count) {
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

int internal_vprintf(char *format, va_list args) {
    int count = vsnprintf(NULL, 0, format, args);
    char buf[count+1];
    vsnprintf(buf, count+1, format, args);
    internal_write(STDOUT_FILENO, buf, count);
    return count;
}

int internal_printf(char *format, ...) {
    va_list args;
    va_start(args, format);
    int count = internal_vprintf(format, args);
    va_end(args);
    return count;
}

int sys_epiphany_printf(char *format, va_list args) {
    e_coreid_t id = e_get_coreid();
    unsigned row, col;
    int count;
    e_coords_from_coreid(id, &row, &col);
    ASSERT (goflag == 1);
    count = internal_printf("[%d,%d] ", row, col);
    // We mustn't hold the lock while calling complex functions like
    // erts_printf_format, since they might rely on printing or stubbed code
    count += erts_printf_format(internal_write, STDOUT_FILENO, format, args);
    fflush(stdout);
    return count;
}

void pump_output() {
    char *captured_end = out_end;
    char *captured_start = out_start;
    if (captured_start > captured_end) {
        write(STDOUT_FILENO, captured_start, OUTBUF_SZ - (outbuf - captured_start));
        captured_start = outbuf;
    }
    write(STDOUT_FILENO, captured_start, captured_end - captured_start);
    out_start = captured_end;
};

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
    erl_start(argc, argv);
    erts_fprintf(stdout, "Terminating normally\n");
    return 0;
}
