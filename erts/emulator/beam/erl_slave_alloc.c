/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2013. All Rights Reserved.
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

/*
 * Description:	Management of memory shared with the slave emulator.
 *
 * Purposefully oversimplistic; hopefully one of the built-in allocators can
 * replace this one eventually.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif
#include "sys.h"
#include "erl_slave_alloc.h"

#define ALIGN(X, A)				\
    ({						\
	__typeof(X) _x = (X);			\
	__typeof(A) _a = (A);			\
	_x % _a ? _x + _a - _x % _a : _x;	\
    })

#define ALIGNMENT 16
#define SPLIT_THRESHOLD 64
#define HEADER_SZ ALIGN(sizeof(struct segment), ALIGNMENT)

struct segment {
    size_t length;
    struct segment *prev, *next;
};

static struct segment *first_free;

static void insert_free(struct segment *);
static void split_free(struct segment *, size_t);
static void unlink_free(struct segment *);

static void
insert_free(struct segment *seg)
{
    seg->prev = NULL;
    seg->next = first_free;
    first_free = seg;
    if (seg->next) {
	seg->next->prev = seg;
    }
}

static void
split_free(struct segment *seg, size_t size)
{
    struct segment *second = ((void*)seg) + HEADER_SZ + size;
    second->length = seg->length - HEADER_SZ - size;
    seg->length = size;
    second->next = seg->next;
    second->prev = seg;
    seg->next = second;
}

static void
unlink_free(struct segment *seg)
{
    if (seg->prev) {
	seg->prev->next = seg->next;
    } else {
	ASSERT(first_free == seg);
	first_free = seg->next;
    }
    seg->next->prev = seg->prev;
}

void
erl_slave_alloc_submit(void *seg, size_t size)
{
    struct segment *free;
    if (size <= HEADER_SZ) return;
    free = seg;
    sys_memzero(free, HEADER_SZ);
    free->length = size - HEADER_SZ;
    insert_free(free);
}

void *
erl_slave_alloc(ErtsAlcType_t t, void *extra, Uint size)
{
    struct segment *free = first_free;
    size = ALIGN(size, 16);
    while(free) {
	if (free->length >= size) {
	    if (free->length - size >= SPLIT_THRESHOLD)
		split_free(free, size);
	    unlink_free(free);
	    return ((void*)free) + HEADER_SZ;
	}
	free = free->next;
    }
    erts_printf("Failed to allocate %#x bytes shared DRAM!\n", size);
    return NULL;
}

#define SEGMENT(Ptr) ((struct segment *)((Ptr) - HEADER_SZ))

void
erl_slave_free(ErtsAlcType_t t, void *extra, void *ptr)
{
    struct segment *seg = SEGMENT(ptr);
    insert_free(seg);
}

void *erl_slave_realloc(ErtsAlcType_t t, void *extra, void *block, Uint size) {
    void *newblock = erl_slave_alloc(t, extra, size);
    Uint tocopy = MIN(SEGMENT(block)->length, size);
    if (newblock) sys_memcpy(newblock, block, tocopy);
    return newblock;
}
