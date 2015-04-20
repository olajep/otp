/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2000-2015. All Rights Reserved.
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
#include "erl_printf_format.h"
#include "epiphany.h"
#include "epiphany_io.h"
#include <e-lib.h>

// erl_epiphany_sys.h redeclares write(int, const void*, size_t) with the same
// symbol as internal_write. We can't make it static, or that won't work.
ssize_t internal_write(int, const void*, size_t);

static int sys_epiphany_printf(char*, va_list);

#define OUTBUF_SZ 1024
char outbuf[OUTBUF_SZ];
char * volatile out_start = outbuf;
char * volatile out_end = outbuf;
static EPIPHANY_SRAM_DATA e_mutex_t global_mutex;

ssize_t
internal_write(int __attribute__((unused)) fildes,
	       const void *data, const size_t initial_count)
{
    size_t count = initial_count;
    if (epiphany_in_emulator()) return write(STDOUT_FILENO, data, count);

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

void
epiphany_backtrace(void)
{
    // ESTUB
}

void
epiphany_init_io(void)
{
    erts_printf_stdout_func = sys_epiphany_printf;
    erts_printf_stderr_func = sys_epiphany_printf;
}
