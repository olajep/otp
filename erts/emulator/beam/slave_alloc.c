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
#include "slave_alloc.h"
#include "erl_smp.h"

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
static erts_smp_mtx_t alloc_mtx;

static void insert_free(struct segment *);
static void split_free(struct segment *, size_t);
static void unlink_free(struct segment *);

#ifdef DEBUG
static Uint available, used;
#endif

#define HARDDEBUG 0
#if HARDDEBUG

static void debug_verify(const char *file, int line, const char *func) {
    Uint count = 0, die = 0;
    struct segment *free = first_free, *last = NULL;
    while(free) {
	ASSERT(0x8e000000 <= (unsigned)free && (unsigned)free <= 0x90000000);
	count += free->length + HEADER_SZ;
	if(free->prev != last) {
	    die = 1;
	    erts_fprintf(stderr, "%s:%d:%s(): List consistency broken! "
			 "%#x->prev = %#x, should be %#x\n", file, line, func,
			 free, free->prev, last);
	}
	last = free;
	free = free->next;
    }
    if (count != (available - used) || die) {
	erl_exit(1, "%s:%d:%s(): %#x lost! free is %#x (should be %#x) "
		 "used=%#x avail=%#x\n", file, line, func,
		 (available-used) - count, count, available-used,
		 used, available);
    }
}

#  define DEBUG_VERIFY() debug_verify(__FILE__, __LINE__, __FUNCTION__)
#else
#  define DEBUG_VERIFY() ((void)0)
#endif

static void
insert_free(struct segment *seg)
{
    ASSERT(seg->prev == NULL);
    ASSERT(seg->next == NULL);
    ASSERT(seg->length < 32 * 1024 * 1024);
    seg->prev = NULL;
    seg->next = first_free;
    first_free = seg;
    if (seg->next) {
	ASSERT(seg->next->prev == NULL);
	seg->next->prev = seg;
    }
#ifdef DEBUG
    used -= seg->length + HEADER_SZ;
#endif
}

static void
split_free(struct segment *seg, size_t size)
{
    struct segment *second = ((void*)seg) + HEADER_SZ + size;
    second->length = seg->length - HEADER_SZ - size;
    seg->length = size;
    second->next = seg->next;
    if (second->next) {
	ASSERT(second->next->prev = seg);
	second->next->prev = second;
    }
    second->prev = seg;
    seg->next = second;
    DEBUG_VERIFY();
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
    ASSERT(seg->next);
    seg->next->prev = seg->prev;
#ifdef DEBUG
    seg->next = NULL;
    seg->prev = NULL;
    used += seg->length + HEADER_SZ;
#endif
}

static int fallback_enabled = 0;

void
erl_slave_alloc_submit(void *seg, size_t size)
{
    struct segment *free;
    ASSERT(!fallback_enabled);
    if (size <= HEADER_SZ) return;
    free = seg;
    sys_memzero(free, HEADER_SZ);
    free->length = size - HEADER_SZ;
#ifdef DEBUG
    available += size;
    used += size;
#endif
    insert_free(free);
    DEBUG_VERIFY();
    erts_smp_mtx_init(&alloc_mtx, "slave_alloc_mtx");
}

void erl_slave_alloc_fallback(void)
{
    ASSERT(available == 0);
    fallback_enabled = 1;
}

#define LOCK() erts_smp_mtx_lock(&alloc_mtx)
#define UNLOCK() erts_smp_mtx_unlock(&alloc_mtx)

#define SEGMENT(Ptr) ((struct segment *)((Ptr) - HEADER_SZ))

/*
 * Takes an allocator number and returns the type of the assigned fallback
 * allocator.
 */
static ErtsAlcType_t
fallback(ErtsAlcType_t n)
{
    switch(n) {
    case ERTS_ALC_T2N(ERTS_ALC_T_FUN_ENTRY):
	return ERTS_ALC_T_FUN_ENTRY_FALLBACK;
    case ERTS_ALC_T2N(ERTS_ALC_T_ATOM):
	return ERTS_ALC_T_ATOM_FALLBACK;
    case ERTS_ALC_T2N(ERTS_ALC_T_ATOM_TXT):
	return ERTS_ALC_T_ATOM_TXT_FALLBACK;
    case ERTS_ALC_T2N(ERTS_ALC_T_ATOM_TABLE):
	return ERTS_ALC_T_ATOM_TABLE_FALLBACK;
    default:
	erl_exit(1, "Bad allocator number %d in slave_alloc", n);
    }
}

void *
erl_slave_alloc(ErtsAlcType_t t, void *extra, Uint size)
{
    struct segment *free;
#ifdef DEBUG
    int max = 0, count = 0;
#endif
    void *res = NULL;
    if (fallback_enabled) {
	return erts_alloc_fnf(fallback(t), size);
    }

    LOCK();
    free = first_free;
    size = ALIGN(size, 16);
    while(free) {
	if (free->length >= size) {
	    if (free->length - size >= SPLIT_THRESHOLD)
		split_free(free, size);
	    unlink_free(free);
	    res = ((void*)free) + HEADER_SZ;
	    break;
	}
#ifdef DEBUG
	count ++;
	max = MAX(max, free->length);
#endif
	free = free->next;
    }
    if (!res) {
	erts_printf("Failed to allocate %d bytes shared DRAM!\n", size);
#ifdef DEBUG
	erts_printf("%d free blocks (largest is %d bytes)\n", count, max);
	erts_printf("%d / %d used\n", used, available);
#endif
    }
    DEBUG_VERIFY();
    UNLOCK();
    return res;
}

void
erl_slave_free(ErtsAlcType_t t, void *extra, void *ptr)
{
    if (fallback_enabled) {
	erts_free(fallback(t), ptr);
    } else {
	struct segment *seg = SEGMENT(ptr);
	LOCK();
	insert_free(seg);
	DEBUG_VERIFY();
	UNLOCK();
    }
}

void *erl_slave_realloc(ErtsAlcType_t t, void *extra, void *block, Uint size) {
    if (fallback_enabled) {
	return erts_realloc(fallback(t), block, size);
    } else {
	void *newblock = erl_slave_alloc(t, extra, size);
	Uint tocopy = MIN(SEGMENT(block)->length, size);
	if (newblock) {
	    sys_memcpy(newblock, block, tocopy);
	    erl_slave_free(t, extra, block);
	}
	return newblock;
    }
}
