#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_fifo.h"

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

void
erts_fifo_init(struct erl_fifo *fifo, void *buffer, size_t size)
{
    fifo->size = size;
    fifo->buffer = fifo->start = fifo->end = buffer;
}

size_t
erts_fifo_available(const struct erl_fifo *fifo)
{
    const void *captured_end = fifo->end;
    const void *captured_start = fifo->start;
    const void *buffer_end = fifo->buffer + fifo->size;
    if (captured_start > captured_end) {
	return (buffer_end - captured_start) + (captured_end - fifo->buffer);
    } else {
	return captured_end - captured_start;
    }
}

size_t
erts_fifo_free(const struct erl_fifo *fifo)
{
    return fifo->size - erts_fifo_available(fifo) - 1;
}

size_t
erts_fifo_write(struct erl_fifo *fifo, const void* buf, size_t count)
{
    void *captured_end = fifo->end;
    const void *captured_start = fifo->start;
    const void *buffer_end = fifo->buffer + fifo->size;
    size_t to_write = count;
    ASSERT(fifo->buffer <= captured_start && captured_start <= buffer_end);
    ASSERT(fifo->buffer <= captured_end   && captured_end   <= buffer_end);
    ETHR_MEMBAR(ETHR_LoadLoad); /* fifo->start => *fifo->buffer */

    if (captured_end < captured_start && captured_end + to_write >= captured_start)
	to_write = captured_start - captured_end - 1;
    if (captured_end + to_write > buffer_end)
	to_write = buffer_end - captured_end;
    if (captured_end + to_write == buffer_end
	&& captured_start == fifo->buffer)
	to_write -= 1;

    memcpy(captured_end, buf, to_write);
    captured_end += to_write;
    if (captured_end == fifo->buffer + fifo->size)
	captured_end = fifo->buffer;

    ETHR_MEMBAR(ETHR_LoadStore); /* *fifo->buffer => fifo->end */
    fifo->end = captured_end;
    return to_write;
}

void
erts_fifo_write_blocking(struct erl_fifo *fifo, const void* buf, size_t count)
{
    while (count > 0) {
	size_t written = erts_fifo_write(fifo, buf, count);
	buf += written;
	count -= written;
    }
}

size_t
erts_fifo_read(struct erl_fifo *fifo, void* buf, size_t count)
{
    void *captured_start = fifo->start;
    const void *captured_end = fifo->end;
    const void *buffer_end = fifo->buffer + fifo->size;
    size_t to_read = count;
    ASSERT(fifo->buffer <= captured_start && captured_start <= buffer_end);
    ASSERT(fifo->buffer <= captured_end   && captured_end   <= buffer_end);

    if (captured_start > captured_end) {
	to_read = MIN(to_read, buffer_end - captured_start);
    } else {
	to_read = MIN(to_read, captured_end - captured_start);
    }

    memcpy(buf, captured_start, to_read);
    captured_start += to_read;
    if (captured_start == buffer_end)
	captured_start = fifo->buffer;
    fifo->start = captured_start;
    return to_read;
}

void
erts_fifo_peek(struct erl_fifo *fifo, void* buf, size_t count)
{
    void *captured_start = fifo->start;
    ASSERT(fifo->size > count);
    while (erts_fifo_available(fifo) < count);
    while (count > 0) {
	const void *captured_end = fifo->end;
	const void *buffer_end = fifo->buffer + fifo->size;
	size_t to_read = count;
	ASSERT(fifo->buffer <= captured_start && captured_start <= buffer_end);
	ASSERT(fifo->buffer <= captured_end   && captured_end   <= buffer_end);

	if (captured_start > captured_end) {
	    to_read = MIN(to_read, buffer_end - captured_start);
	} else {
	    to_read = MIN(to_read, captured_end - captured_start);
	}

	memcpy(buf, captured_start, to_read);
	captured_start += to_read;
	buf += to_read;
	count -= to_read;
	if (captured_start == buffer_end)
	    captured_start = fifo->buffer;
    }
}

void
erts_fifo_skip(struct erl_fifo *fifo, size_t count)
{
    void *captured_start = fifo->start;
    const void *buffer_end = fifo->buffer + fifo->size;
    ASSERT(fifo->size > count);
    while (erts_fifo_available(fifo) < count);
    captured_start += count;
    if (captured_start > buffer_end)
	captured_start = fifo->buffer + (captured_start - buffer_end);
    fifo->start = captured_start;
}

void
erts_fifo_read_blocking(struct erl_fifo *fifo, void* buf, size_t count)
{
    while (count > 0) {
	size_t read = erts_fifo_read(fifo, buf, count);
	buf += read;
	count -= read;
    }
}
