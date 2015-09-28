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

#define MISALIGNMENT(X, A) (X % A ? A - X % A : 0)

#define ALIGN(X, A)				\
    ({						\
	__typeof(X) _x = (X);			\
	__typeof(A) _a = (A);			\
	_x + MISALIGNMENT(_x, _a);		\
    })

#define ALIGNMENT 16
#define SPLIT_THRESHOLD 64
#define HEADER_SZ ALIGN(sizeof(struct segment), ALIGNMENT)

struct segment {
    size_t length;
    struct segment *prev, *next;
    struct segment *prev_free, *next_free;
    int is_free;
};

#define IS_SEGMENT_FREE(S) ((S)->is_free)
#define SET_SEGMENT_FREE(S, V) ((S)->is_free = (V))

#define ARE_SEGMENTS_CONTIGUOUS(S1, S2) \
    ((void*)(S1) + (S1)->length + HEADER_SZ == (S2))

#define SORTED_FREELIST 1

/*
 * The list that starts at first_seg and continues with the ->next pointers
 * is sorted by memory address to facilitate block merging.
 */
static struct segment *first_seg;

/*
 * The free list is a queue rather than a stack to reduce fragmentation (sorting
 * it by memory address would be even better, but would make free more
 * expensive).
 */
static struct segment *first_free;
static struct segment *last_free;

static erts_smp_mtx_t alloc_mtx;

static void insert_new(struct segment *);
static void insert_free(struct segment *);
static void split_free(struct segment *, size_t);
static void unlink_free(struct segment *);
static void try_merge_segment(struct segment *);
static void merge_segments(struct segment *, struct segment *);

static Uint available, used;

#define HARDDEBUG 0
#if HARDDEBUG

static void debug_verify(const char *file, int line, const char *func) {
    Uint count = 0, die = 0;
    struct segment *free = first_free, *last = NULL, *seg = first_seg;
    while(free) {
	ASSERT(0x8e000000 <= (unsigned)free && (unsigned)free <= 0x90000000);
	count += free->length + HEADER_SZ;
	if(free->prev_free != last) {
	    die = 1;
	    erts_fprintf(stderr, "%s:%d:%s(): List consistency broken! "
			 "%#x->prev_free = %#x, should be %#x\n",
			 file, line, func,
			 free, free->prev_free, last);
	}
#if SORTED_FREELIST
	if (last >= free) {
	    die = 1;
	    erts_fprintf(stderr, "%s:%d:%s(): List consistency broken! "
			 "Segment %#x comes after %#x\n", file, line, func,
			 last, free);
	}
#endif
	last = free;
	free = free->next_free;
    }
    if (last != last_free) {
	die = 1;
	erts_fprintf(stderr, "%s:%d:%s(): List consistency broken! "
		     "last_free = %#x, should be %#x\n", file, line, func,
		     last_free, last);
    }
    last = NULL;
    while(seg) {
	ASSERT(0x8e000000 <= (unsigned)seg && (unsigned)seg <= 0x90000000);
	if(seg->prev != last) {
	    die = 1;
	    erts_fprintf(stderr, "%s:%d:%s(): List consistency broken! "
			 "%#x->prev = %#x, should be %#x\n", file, line, func,
			 seg, seg->prev, last);
	}
	if (last >= seg) {
	    die = 1;
	    erts_fprintf(stderr, "%s:%d:%s(): List consistency broken! "
			 "Segment %#x comes after %#x\n", file, line, func,
			 last, seg);
	}
	last = seg;
	seg = seg->next;
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
insert_new(struct segment *new)
{
    struct segment *seg = first_seg;
    ASSERT(new->prev == NULL);
    ASSERT(new->next == NULL);
    ASSERT(first_seg != new);
    if (!first_seg) {
	first_seg = new;
	return;
    }

    /* Find last segment that comes before new */
    while (seg->next && seg->next < new) seg = seg->next;

    new->prev = seg;
    new->next = seg->next;
    new->prev->next = new;
    if (new->next) new->next->prev = new;
}

static void
insert_free(struct segment *seg)
{
#if SORTED_FREELIST
    struct segment *after;
#endif

    ASSERT(seg->prev != NULL || first_seg == seg);
    ASSERT(!IS_SEGMENT_FREE(seg));
    ASSERT(seg->prev_free == NULL);
    ASSERT(seg->next_free == NULL);
    ASSERT(seg->length < 32 * 1024 * 1024);

#if !SORTED_FREELIST
    seg->next_free = NULL;
    seg->prev_free = last_free;
    last_free = seg;
    if (seg->prev_free) {
	ASSERT(seg->prev_free->next_free == NULL);
	seg->prev_free->next_free = seg;
    } else {
	ASSERT(first_free == NULL);
	first_free = seg;
    }
#else
    after = seg->prev;
    while(after && !after->is_free) after = after->prev;
    if (!after) {
	seg->next_free = first_free;
	seg->prev_free = NULL;
	first_free = seg;
	if (seg->next_free) {
	    ASSERT(seg->next_free > seg);
	    ASSERT(seg->next_free->prev_free == NULL);
	    seg->next_free->prev_free = seg;
	} else {
	    ASSERT(last_free == NULL);
	    last_free = seg;
	}
    } else {
	ASSERT(after < seg);
	seg->next_free = after->next_free;
	seg->prev_free = after;
	after->next_free = seg;
	if (seg->next_free) {
	    ASSERT(seg->next_free > seg);
	    ASSERT(seg->next_free->prev_free == after);
	    seg->next_free->prev_free = seg;
	} else {
	    ASSERT(last_free == after);
	    last_free = seg;
	}
    }
#endif

    used -= seg->length + HEADER_SZ;
    SET_SEGMENT_FREE(seg, 1);
}

static void
split_free(struct segment *seg, size_t size)
{
    struct segment *second = ((void*)seg) + HEADER_SZ + size;
    unlink_free(seg);

    second->length = seg->length - HEADER_SZ - size;
    seg->length = size;

    second->next = seg->next;
    if (second->next) {
	ASSERT(second->next->prev = seg);
	second->next->prev = second;
    }
    second->prev = seg;
    seg->next = second;

#ifdef DEBUG
    second->is_free = 0;
    second->next_free = NULL;
    second->prev_free = NULL;
#endif

    insert_free(seg);
    insert_free(second);
    DEBUG_VERIFY();
}

#if SORTED_FREELIST
static struct segment *
rev_split_free(struct segment *seg, size_t size)
{
    size_t other_size = seg->length - HEADER_SZ - size;
    split_free(seg, other_size);
    ASSERT(seg->next->length == size);
    return seg->next;
}
#endif

static void
unlink_free(struct segment *seg)
{
    ASSERT(IS_SEGMENT_FREE(seg));
    if (seg->prev_free) {
	seg->prev_free->next_free = seg->next_free;
    } else {
	ASSERT(first_free == seg);
	first_free = seg->next_free;
    }
    if (seg->next_free) {
	seg->next_free->prev_free = seg->prev_free;
    } else {
	ASSERT(last_free == seg);
	last_free = seg->prev_free;
    }
#ifdef DEBUG
    seg->prev_free = NULL;
    seg->next_free = NULL;
#endif
    used += seg->length + HEADER_SZ;
    SET_SEGMENT_FREE(seg, 0);
}

static void
try_merge_segment(struct segment *seg)
{
    ASSERT(IS_SEGMENT_FREE(seg));
    while (seg->prev && IS_SEGMENT_FREE(seg->prev)
	   && ARE_SEGMENTS_CONTIGUOUS(seg->prev, seg)) {
	seg = seg->prev;
	merge_segments(seg, seg->next);
    }
    while (seg->next && IS_SEGMENT_FREE(seg->next)
	   && ARE_SEGMENTS_CONTIGUOUS(seg, seg->next)) {
	merge_segments(seg, seg->next);
    }
}

static void
merge_segments(struct segment *first, struct segment *second)
{
    ASSERT(ARE_SEGMENTS_CONTIGUOUS(first, second));
    unlink_free(first);
    unlink_free(second);

    first->next = second->next;
    if (first->next)
	first->next->prev = first;
    first->length += second->length + HEADER_SZ;

    insert_free(first);
}

static int fallback_enabled = 0;

void
erl_slave_alloc_submit(void *seg, size_t size)
{
    struct segment *free;
    size_t misalignment = MISALIGNMENT((size_t)seg, ALIGNMENT);
    ASSERT(!fallback_enabled);
    seg += misalignment;
    size -= misalignment;
    if (size <= HEADER_SZ) return;
    free = seg;
    sys_memzero(free, HEADER_SZ);
    free->length = size - HEADER_SZ;

    available += size;
    used += size;

    insert_new(free);
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
    case ERTS_ALC_T2N(ERTS_ALC_T_SLAVE_MISC):
	return ERTS_ALC_T_SLAVE_MISC_FALLBACK;
    case ERTS_ALC_T2N(ERTS_ALC_T_LL_TEMP_TERM):
	return ERTS_ALC_T_LL_TEMP_TERM_FALLBACK;
    case ERTS_ALC_T2N(ERTS_ALC_T_EXPORT_TABLE):
	return ERTS_ALC_T_EXPORT_TABLE_FALLBACK;
    case ERTS_ALC_T2N(ERTS_ALC_T_NIF_TRAP_EXPORT):
	return ERTS_ALC_T_NIF_TRAP_EXPORT_FALLBACK;
    case ERTS_ALC_T2N(ERTS_ALC_T_EXPORT):
	return ERTS_ALC_T_EXPORT_FALLBACK;
#ifdef HIPE
    case ERTS_ALC_T2N(ERTS_ALC_T_HIPE_DATA):
	return ERTS_ALC_T_HIPE_DATA_FALLBACK;
#endif
    default:
	erl_exit(1, "Bad allocator number %d in slave_alloc", n);
    }
}

void *
erl_slave_alloc(ErtsAlcType_t n, void *extra, Uint size)
{
    struct segment *free;
#ifdef DEBUG
    int max = 0, count = 0;
#endif
    void *res = NULL;
    if (fallback_enabled) {
	return erts_alloc_fnf(fallback(n), size);
    }

    size = ALIGN(size, 16);
    LOCK();
#if SORTED_FREELIST
    if (ERTS_ALC_N_MIN_A_SLAVE_LL <= n && n <= ERTS_ALC_N_MAX_A_SLAVE_LL) {
	free = last_free;
	while(free) {
	    if (free->length >= size) {
		if (free->length - size >= SPLIT_THRESHOLD)
		    free = rev_split_free(free, size);
		unlink_free(free);
		res = ((void*)free) + HEADER_SZ;
		break;
	    }
#  ifdef DEBUG
	    count ++;
	    max = MAX(max, free->length);
#  endif
	    free = free->prev_free;
	}
    } else
#endif /* SORTED_FREELIST */
    {
	free = first_free;
	while(free) {
	    if (free->length >= size) {
		if (free->length - size >= SPLIT_THRESHOLD)
		    split_free(free, size);
		unlink_free(free);
		res = ((void*)free) + HEADER_SZ;
		break;
	    }
#  ifdef DEBUG
	    count ++;
	    max = MAX(max, free->length);
#  endif
	    free = free->prev_free;
	}
    }
#endif /* SORTED_FREELIST */
    if (!res) {
	erts_printf("Failed to allocate %d bytes shared DRAM!\n", size);
#ifdef DEBUG
	erts_printf("%d free blocks (largest is %d bytes)\n", count, max);
#endif
	erts_printf("%d / %d used\n", used, available);
    }
    DEBUG_VERIFY();
    UNLOCK();
    return res;
}

void
erl_slave_free(ErtsAlcType_t n, void *extra, void *ptr)
{
    if (fallback_enabled) {
	erts_free(fallback(n), ptr);
    } else {
	struct segment *seg = SEGMENT(ptr);
	LOCK();
	insert_free(seg);
	try_merge_segment(seg);
	DEBUG_VERIFY();
	UNLOCK();
    }
}

void *erl_slave_realloc(ErtsAlcType_t n, void *extra, void *block, Uint size) {
    if (fallback_enabled) {
	return erts_realloc(fallback(n), block, size);
    } else {
	void *newblock = erl_slave_alloc(n, extra, size);
	Uint tocopy = MIN(SEGMENT(block)->length, size);
	if (newblock) {
	    sys_memcpy(newblock, block, tocopy);
	    erl_slave_free(n, extra, block);
	}
	return newblock;
    }
}
