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

#ifndef __ERL_FIFO_H__
#define __ERL_FIFO_H__

/*
 * A FIFO ringbuffer for a single producer and a single consumer, without any
 * atomics or mutexes. It is currently used for communication with the slave
 * emulator.
 *
 * Multiple consumers or producers can share a FIFO, but they need to use
 * external synchronisation.
 *
 * The contents of the buffer is the region that begins at start and ends at
 * end. The consumer advances start, and the producer advances end.
 * When start == end, the buffer is empty.
 */
struct erl_fifo {
    size_t size;
    void *buffer, *volatile start, *volatile end;
};

void erts_fifo_init(struct erl_fifo *fifo, void *buffer, size_t size);

size_t erts_fifo_available(const struct erl_fifo *fifo);
size_t erts_fifo_free(const struct erl_fifo *fifo);

size_t erts_fifo_write(struct erl_fifo *fifo, const void* buf, size_t count);
void erts_fifo_write_blocking(struct erl_fifo *fifo, const void* buf, size_t count);

/* These always block. Use erts_fifo_available to avoid that. */
void erts_fifo_peek(struct erl_fifo *fifo, void* buf, size_t count);
void erts_fifo_skip(struct erl_fifo *fifo, size_t count);

size_t erts_fifo_read(struct erl_fifo *fifo, void* buf, size_t count);
void erts_fifo_read_blocking(struct erl_fifo *fifo, void* buf, size_t count);

#endif /* defined(__ERL_FIFO_H__) */
