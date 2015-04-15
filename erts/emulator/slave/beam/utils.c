/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2014. All Rights Reserved.
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

#ifndef ERTS_NO_INLINE
#  define ERTS_DO_INCL_GLB_INLINE_FUNC_DEF
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "big.h"
#include "bif.h"
#include "erl_binary.h"
#include "erl_bits.h"
#include "erl_map.h"
#include "packet_parser.h"
#include "erl_gc.h"
#define ERTS_WANT_DB_INTERNAL__
#include "erl_db.h"
#include "erl_threads.h"
#include "register.h"
#include "dist.h"
#include "erl_printf.h"
#include "erl_threads.h"
#include "erl_smp.h"
#include "erl_time.h"
#include "erl_thr_progress.h"
#include "beam_bp.h"
#include "erl_ptab.h"

#undef M_TRIM_THRESHOLD
#undef M_TOP_PAD
#undef M_MMAP_THRESHOLD
#undef M_MMAP_MAX

#if (defined(__GLIBC__) || defined(__epiphany__)) && defined(HAVE_MALLOC_H)
#include <malloc.h>
#endif

#if !defined(HAVE_MALLOPT)
#undef  HAVE_MALLOPT
#define HAVE_MALLOPT 0
#endif

/* profile_scheduler mini message queue */

typedef struct {
    Uint scheduler_id;
    Uint no_schedulers;
    Uint Ms;
    Uint s;
    Uint us;
    Eterm state;
} profile_sched_msg;

typedef struct {
    profile_sched_msg msg[2];
    Uint n;
} profile_sched_msg_q;

#ifdef ERTS_SMP

#if 0 /* Unused */
static void 
dispatch_profile_msg_q(profile_sched_msg_q *psmq)
{
    int i = 0;
    profile_sched_msg *msg = NULL;
    ASSERT(psmq != NULL);
    for (i = 0; i < psmq->n; i++) {
        msg = &(psmq->msg[i]);
	profile_scheduler_q(make_small(msg->scheduler_id), msg->state, am_undefined, msg->Ms, msg->s, msg->us);
    }
}
#endif

#endif


Eterm*
erts_heap_alloc(Process* p, Uint need, Uint xtra)
{
    ErlHeapFragment* bp;
    Eterm* htop;
    Uint n;
#if defined(DEBUG) || defined(CHECK_FOR_HOLES)
    Uint i;
#endif

#ifdef FORCE_HEAP_FRAGS
    if (p->space_verified && p->space_verified_from!=NULL
	&& HEAP_TOP(p) >= p->space_verified_from
	&& HEAP_TOP(p) + need <= p->space_verified_from + p->space_verified
	&& HEAP_LIMIT(p) - HEAP_TOP(p) >= need) {
	
	Uint consumed = need + (HEAP_TOP(p) - p->space_verified_from);
	ASSERT(consumed <= p->space_verified);
	p->space_verified -= consumed;
	p->space_verified_from += consumed;
	HEAP_TOP(p) = p->space_verified_from;
	return HEAP_TOP(p) - need;
    }
    p->space_verified = 0;
    p->space_verified_from = NULL;
#endif /* FORCE_HEAP_FRAGS */

    n = need + xtra;
    bp = MBUF(p);
    if (bp != NULL && need <= (bp->alloc_size - bp->used_size)) {
	Eterm* ret = bp->mem + bp->used_size;
	bp->used_size += need;
	return ret;
    }
#ifdef DEBUG
    n++;
#endif
    bp = (ErlHeapFragment*)
	ERTS_HEAP_ALLOC(ERTS_ALC_T_HEAP_FRAG, ERTS_HEAP_FRAG_SIZE(n));

#if defined(DEBUG) || defined(CHECK_FOR_HOLES)
    for (i = 0; i < n; i++) {
	bp->mem[i] = ERTS_HOLE_MARKER;
    }
#endif

#ifdef DEBUG
    n--;
#endif

    /*
     * When we have created a heap fragment, we are no longer allowed
     * to store anything more on the heap. 
     */
    htop = HEAP_TOP(p);
    if (htop < HEAP_LIMIT(p)) {
	*htop = make_pos_bignum_header(HEAP_LIMIT(p)-htop-1);
	HEAP_TOP(p) = HEAP_LIMIT(p);
    }

    bp->next = MBUF(p);
    MBUF(p) = bp;
    bp->alloc_size = n;
    bp->used_size = need;
    MBUF_SIZE(p) += n;
    bp->off_heap.first = NULL;
    bp->off_heap.overhead = 0;
    return bp->mem;
}

#ifdef CHECK_FOR_HOLES
Eterm*
erts_set_hole_marker(Eterm* ptr, Uint sz)
{
    Eterm* p = ptr;
    int i;

    for (i = 0; i < sz; i++) {
	*p++ = ERTS_HOLE_MARKER;
    }
    return ptr;
}
#endif

/*
 * Helper function for the ESTACK macros defined in global.h.
 */
void
erl_grow_estack(ErtsEStack* s, Eterm* default_estack)
{
    Uint old_size = (s->end - s->start);
    Uint new_size = old_size * 2;
    Uint sp_offs = s->sp - s->start;
    if (s->start != default_estack) {
	s->start = erts_realloc(s->alloc_type, s->start,
				new_size*sizeof(Eterm));
    } else {
	Eterm* new_ptr = erts_alloc(s->alloc_type, new_size*sizeof(Eterm));
	sys_memcpy(new_ptr, s->start, old_size*sizeof(Eterm));
	s->start = new_ptr;
    }
    s->end = s->start + new_size;
    s->sp = s->start + sp_offs;
}
/*
 * Helper function for the WSTACK macros defined in global.h.
 */
void
erl_grow_wstack(ErtsWStack* s, UWord* default_wstack)
{
    Uint old_size = (s->wend - s->wstart);
    Uint new_size = old_size * 2;
    Uint sp_offs = s->wsp - s->wstart;
    if (s->wstart != default_wstack) {
	s->wstart = erts_realloc(s->alloc_type, s->wstart,
				 new_size*sizeof(UWord));
    } else {
	UWord* new_ptr = erts_alloc(s->alloc_type, new_size*sizeof(UWord));
	sys_memcpy(new_ptr, s->wstart, old_size*sizeof(UWord));
	s->wstart = new_ptr;
    }
    s->wend = s->wstart + new_size;
    s->wsp = s->wstart + sp_offs;
}

/* CTYPE macros */

#define LATIN1

#define IS_DIGIT(c)  ((c) >= '0' && (c) <= '9')
#ifdef LATIN1
#define IS_LOWER(c)  (((c) >= 'a' && (c) <= 'z') \
		      || ((c) >= 128+95 && (c) <= 255 && (c) != 247))
#define IS_UPPER(c)  (((c) >= 'A' && (c) <= 'Z') \
		      || ((c) >= 128+64 && (c) <= 128+94 && (c) != 247-32))
#else
#define IS_LOWER(c)  ((c) >= 'a' && (c) <= 'z')
#define IS_UPPER(c)  ((c) >= 'A' && (c) <= 'Z')
#endif

#define IS_ALNUM(c)  (IS_DIGIT(c) || IS_LOWER(c) || IS_UPPER(c))

/* We don't include 160 (non-breaking space). */
#define IS_SPACE(c)  (c == ' ' || c == '\n' || c == '\t' || c == '\r')

#ifdef LATIN1
#define IS_CNTRL(c)  ((c) < ' ' || (c) == 127 \
		      || ((c) >= 128 && (c) < 128+32))
#else
/* Treat all non-ASCII as control characters */
#define IS_CNTRL(c)  ((c) < ' ' || (c) >= 127)
#endif

#define IS_PRINT(c)  (!IS_CNTRL(c))

/*
 * Calculate length of a list.
 * Returns -1 if not a proper list (i.e. not terminated with NIL)
 */
int
erts_list_length(Eterm list)
{
    int i = 0;

    while(is_list(list)) {
	i++;
	list = CDR(list_val(list));
    }
    if (is_not_nil(list)) {
	return -1;
    }
    return i;
}

static const struct {
    Sint64 mask;
    int bits;
} fib_data[] = {{ERTS_I64_LITERAL(0x2), 1},
		{ERTS_I64_LITERAL(0xc), 2},
		{ERTS_I64_LITERAL(0xf0), 4},
		{ERTS_I64_LITERAL(0xff00), 8},
		{ERTS_I64_LITERAL(0xffff0000), 16},
		{ERTS_I64_LITERAL(0xffffffff00000000), 32}};

static ERTS_INLINE int
fit_in_bits(Sint64 value, int start)
{
    int bits = 0;
    int i;

    for (i = start; i >= 0; i--) {
	if (value & fib_data[i].mask) {
	    value >>= fib_data[i].bits;
	    bits |= fib_data[i].bits;
	}
    }

    bits++;

    return bits;
}

int erts_fit_in_bits_int64(Sint64 value)
{
    return fit_in_bits(value, 5);
}

int erts_fit_in_bits_int32(Sint32 value)
{
    return fit_in_bits((Sint64) (Uint32) value, 4);
}

int
erts_print(int to, void *arg, char *format, ...)
{
    int res;
    va_list arg_list;
    va_start(arg_list, format);

    if (to < ERTS_PRINT_MIN)
	res = -EINVAL;
    else {
	switch (to) {
	case ERTS_PRINT_STDOUT:
	    res = erts_vprintf(format, arg_list);
	    break;
	case ERTS_PRINT_STDERR:
	    res = erts_vfprintf(stderr, format, arg_list);
	    break;
	case ERTS_PRINT_FILE:
	    res = erts_vfprintf((FILE *) arg, format, arg_list);
	    break;
	case ERTS_PRINT_SBUF:
	    res = erts_vsprintf((char *) arg, format, arg_list);
	    break;
	case ERTS_PRINT_SNBUF:
	    res = erts_vsnprintf(((erts_print_sn_buf *) arg)->buf,
				 ((erts_print_sn_buf *) arg)->size,
				 format,
				 arg_list);
	    break;
	case ERTS_PRINT_DSBUF:
	    res = erts_vdsprintf((erts_dsprintf_buf_t *) arg, format, arg_list);
	    break;
	case ERTS_PRINT_INVALID:
	    res = -EINVAL;
	    break;
	default:
	    res = erts_vfdprintf((int) to, format, arg_list);
	    break;
	}
    }

    va_end(arg_list);
    return res;
}

int
erts_putc(int to, void *arg, char c)
{
    return erts_print(to, arg, "%c", c);
}

Eterm
erts_bld_uint(Uint **hpp, Uint *szp, Uint ui)
{
    Eterm res = THE_NON_VALUE;
    if (IS_USMALL(0, ui)) {
	if (hpp)
	    res = make_small(ui);
    }
    else {
	if (szp)
	    *szp += BIG_UINT_HEAP_SIZE;
	if (hpp) {
	    res = uint_to_big(ui, *hpp);
	    *hpp += BIG_UINT_HEAP_SIZE;
	}
    }
    return res;
}

/*
 * Erts_bld_uword is more or less similar to erts_bld_uint, but a pointer
 * can safely be passed.
 */

Eterm
erts_bld_uword(Uint **hpp, Uint *szp, UWord uw)
{
    Eterm res = THE_NON_VALUE;
    if (IS_USMALL(0, uw)) {
	if (hpp)
	    res = make_small((Uint) uw);
    }
    else {
	if (szp)
	    *szp += BIG_UWORD_HEAP_SIZE(uw);
	if (hpp) {
	    res = uword_to_big(uw, *hpp);
	    *hpp += BIG_UWORD_HEAP_SIZE(uw);
	}
    }
    return res;
}


Eterm
erts_bld_uint64(Uint **hpp, Uint *szp, Uint64 ui64)
{
    Eterm res = THE_NON_VALUE;
    if (IS_USMALL(0, ui64)) {
	if (hpp)
	    res = make_small((Uint) ui64);
    }
    else {
	if (szp)
	    *szp += ERTS_UINT64_HEAP_SIZE(ui64);
	if (hpp)
	    res = erts_uint64_to_big(ui64, hpp);
    }
    return res;
}

Eterm
erts_bld_sint64(Uint **hpp, Uint *szp, Sint64 si64)
{
    Eterm res = THE_NON_VALUE;
    if (IS_SSMALL(si64)) {
	if (hpp)
	    res = make_small((Sint) si64);
    }
    else {
	if (szp)
	    *szp += ERTS_SINT64_HEAP_SIZE(si64);
	if (hpp)
	    res = erts_sint64_to_big(si64, hpp);
    }
    return res;
}


Eterm
erts_bld_cons(Uint **hpp, Uint *szp, Eterm car, Eterm cdr)
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += 2;
    if (hpp) {
	res = CONS(*hpp, car, cdr);
	*hpp += 2;
    }
    return res;
}

Eterm
erts_bld_tuple(Uint **hpp, Uint *szp, Uint arity, ...)
{
    Eterm res = THE_NON_VALUE;

    ASSERT(arity < (((Uint)1) << (sizeof(Uint)*8 - _HEADER_ARITY_OFFS)));

    if (szp)
	*szp += arity + 1;
    if (hpp) {
	res = make_tuple(*hpp);
	*((*hpp)++) = make_arityval(arity);

	if (arity > 0) {
	    Uint i;
	    va_list argp;

	    va_start(argp, arity);
	    for (i = 0; i < arity; i++) {
                *((*hpp)++) = va_arg(argp, Eterm);
            }
	    va_end(argp);
	}
    }
    return res;
}


Eterm erts_bld_tuplev(Uint **hpp, Uint *szp, Uint arity, Eterm terms[])
{
    Eterm res = THE_NON_VALUE;
    /*
     * Note callers expect that 'terms' is *not* accessed if hpp == NULL.
     */

    ASSERT(arity < (((Uint)1) << (sizeof(Uint)*8 - _HEADER_ARITY_OFFS)));

    if (szp)
	*szp += arity + 1;
    if (hpp) {

	res = make_tuple(*hpp);
	*((*hpp)++) = make_arityval(arity);

	if (arity > 0) {
	    Uint i;
	    for (i = 0; i < arity; i++)
		*((*hpp)++) = terms[i];
	}
    }
    return res;
}

Eterm
erts_bld_string_n(Uint **hpp, Uint *szp, const char *str, Sint len)
{
    Eterm res = THE_NON_VALUE;
    Sint i = len;
    if (szp)
	*szp += len*2;
    if (hpp) {
	res = NIL;
	while (--i >= 0) {
	    res = CONS(*hpp, make_small((byte) str[i]), res);
	    *hpp += 2;
	}
    }
    return res;
}

Eterm
erts_bld_list(Uint **hpp, Uint *szp, Sint length, Eterm terms[])
{
    Eterm list = THE_NON_VALUE;
    if (szp)
	*szp += 2*length;
    if (hpp) {
	Sint i = length;
	list = NIL;

	while (--i >= 0) {
	    list = CONS(*hpp, terms[i], list);
	    *hpp += 2;
	}
    }
    return list;
}

Eterm
erts_bld_2tup_list(Uint **hpp, Uint *szp,
		   Sint length, Eterm terms1[], Uint terms2[])
{
    Eterm res = THE_NON_VALUE;
    if (szp)
	*szp += 5*length;
    if (hpp) {
	Sint i = length;
	res = NIL;

	while (--i >= 0) {
	    res = CONS(*hpp+3, TUPLE2(*hpp, terms1[i], terms2[i]), res);
	    *hpp += 5;
	}
    }
    return res;
}

Eterm
erts_bld_atom_uword_2tup_list(Uint **hpp, Uint *szp,
                              Sint length, Eterm atoms[], UWord uints[])
{
    Sint i;
    Eterm res = THE_NON_VALUE;
    if (szp) {
	*szp += 5*length;
	i = length;
	while (--i >= 0) {
	    if (!IS_USMALL(0, uints[i]))
		*szp += BIG_UINT_HEAP_SIZE;
	}
    }
    if (hpp) {
	i = length;
	res = NIL;

	while (--i >= 0) {
	    Eterm ui;

	    if (IS_USMALL(0, uints[i]))
		ui = make_small(uints[i]);
	    else {
		ui = uint_to_big(uints[i], *hpp);
		*hpp += BIG_UINT_HEAP_SIZE;
	    }
	    
	    res = CONS(*hpp+3, TUPLE2(*hpp, atoms[i], ui), res);
	    *hpp += 5;
	}
    }
    return res;
}

Eterm
erts_bld_atom_2uint_3tup_list(Uint **hpp, Uint *szp, Sint length,
			      Eterm atoms[], Uint uints1[], Uint uints2[])
{
    Sint i;
    Eterm res = THE_NON_VALUE;
    if (szp) {
	*szp += 6*length;
	i = length;
	while (--i >= 0) {
	    if (!IS_USMALL(0, uints1[i]))
		*szp += BIG_UINT_HEAP_SIZE;
	    if (!IS_USMALL(0, uints2[i]))
		*szp += BIG_UINT_HEAP_SIZE;
	}
    }
    if (hpp) {
	i = length;
	res = NIL;

	while (--i >= 0) {
	    Eterm ui1;
	    Eterm ui2;

	    if (IS_USMALL(0, uints1[i]))
		ui1 = make_small(uints1[i]);
	    else {
		ui1 = uint_to_big(uints1[i], *hpp);
		*hpp += BIG_UINT_HEAP_SIZE;
	    }
	    
	    if (IS_USMALL(0, uints2[i]))
		ui2 = make_small(uints2[i]);
	    else {
		ui2 = uint_to_big(uints2[i], *hpp);
		*hpp += BIG_UINT_HEAP_SIZE;
	    }
	    
	    res = CONS(*hpp+4, TUPLE3(*hpp, atoms[i], ui1, ui2), res);
	    *hpp += 6;
	}
    }
    return res;
}

/*                                                                           *\
 *                                                                           *
\* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* make a hash index from an erlang term */

/*
** There are three hash functions.
** make_broken_hash: the one used for backward compatibility
** is called from the bif erlang:hash/2. Should never be used
** as it a) hashes only a part of binaries, b) hashes bignums really poorly,
** c) hashes bignums differently on different endian processors and d) hashes 
** small integers with different weights on different bytes.
**
** make_hash: A hash function that will give the same values for the same
** terms regardless of the internal representation. Small integers are 
** hashed using the same algorithm as bignums and bignums are hashed 
** independent of the CPU endianess. 
** Make_hash also hashes pids, ports and references like 32 bit numbers 
** (but with different constants). 
** make_hash() is called from the bif erlang:phash/2
**
** The idea behind the hash algorithm is to produce values suitable for 
** linear dynamic hashing. We cannot choose the range at all while hashing 
** (it's not even supplied to the hashing functions). The good old algorithm
** [H = H*C+X mod M, where H is the hash value, C is a "random" constant(or M),
** M is the range, preferably a prime, and X is each byte value] is therefore 
** modified to:
** H = H*C+X mod 2^32, where C is a large prime. This gives acceptable 
** "spreading" of the hashes, so that later modulo calculations also will give
** acceptable "spreading" in the range. 
** We really need to hash on bytes, otherwise the 
** upper bytes of a word will be less significant than the lower ones. That's 
** not acceptable at all. For internal use one could maybe optimize by using
** another hash function, that is less strict but faster. That is, however, not
** implemented.
**
** Short semi-formal description of make_hash:
**
** In make_hash, the number N is treated like this:
**  Abs(N) is hashed bytewise with the least significant byte, B(0), first.
**  The number of bytes (J) to calculate hash on in N is 
**  (the number of _32_ bit words needed to store the unsigned 
**   value of abs(N)) * 4.
**  X = FUNNY_NUMBER2
**  If N < 0, Y = FUNNY_NUMBER4 else Y = FUNNY_NUMBER3.
**  The hash value is Y*h(J) mod 2^32 where h(J) is calculated like
**  h(0) = <initial hash> 
**  h(i) = h(i-i)*X + B(i-1)
** The above should hold regardless of internal representation.
** Pids are hashed like small numbers but with differrent constants, as are
** ports.
** References are hashed like ports but only on the least significant byte.
** Binaries are hashed on all bytes (not on the 15 first as in 
** make_broken_hash()).
** Bytes in lists (possibly text strings) use a simpler multiplication inlined
** in the handling of lists, that is an optimization.
** Everything else is like in the old hash (make_broken_hash()).
**
** make_hash2() is faster than make_hash, in particular for bignums
** and binaries, and produces better hash values. 
*/

Uint32 make_hash(Eterm term_arg)
{
    EPIPHANY_STUB_FUN();
}



/* Hash function suggested by Bob Jenkins. */

#define MIX(a,b,c)                 \
do {                               \
  a -= b; a -= c; a ^= (c>>13);    \
  b -= c; b -= a; b ^= (a<<8);     \
  c -= a; c -= b; c ^= (b>>13);    \
  a -= b; a -= c; a ^= (c>>12);    \
  b -= c; b -= a; b ^= (a<<16);    \
  c -= a; c -= b; c ^= (b>>5);     \
  a -= b; a -= c; a ^= (c>>3);     \
  b -= c; b -= a; b ^= (a<<10);    \
  c -= a; c -= b; c ^= (b>>15);    \
} while(0)

#define HCONST 0x9e3779b9UL /* the golden ratio; an arbitrary value */

Uint32
block_hash(byte *k, unsigned length, Uint32 initval)
{
   Uint32 a,b,c;
   unsigned len;

   /* Set up the internal state */
   len = length;
   a = b = HCONST;
   c = initval;           /* the previous hash value */

   while (len >= 12)
   {
      a += (k[0] +((Uint32)k[1]<<8) +((Uint32)k[2]<<16) +((Uint32)k[3]<<24));
      b += (k[4] +((Uint32)k[5]<<8) +((Uint32)k[6]<<16) +((Uint32)k[7]<<24));
      c += (k[8] +((Uint32)k[9]<<8) +((Uint32)k[10]<<16)+((Uint32)k[11]<<24));
      MIX(a,b,c);
      k += 12; len -= 12;
   }

   c += length;
   switch(len)              /* all the case statements fall through */
   {
   case 11: c+=((Uint32)k[10]<<24);
   case 10: c+=((Uint32)k[9]<<16);
   case 9 : c+=((Uint32)k[8]<<8);
      /* the first byte of c is reserved for the length */
   case 8 : b+=((Uint32)k[7]<<24);
   case 7 : b+=((Uint32)k[6]<<16);
   case 6 : b+=((Uint32)k[5]<<8);
   case 5 : b+=k[4];
   case 4 : a+=((Uint32)k[3]<<24);
   case 3 : a+=((Uint32)k[2]<<16);
   case 2 : a+=((Uint32)k[1]<<8);
   case 1 : a+=k[0];
     /* case 0: nothing left to add */
   }
   MIX(a,b,c);
   return c;
}

Uint32
make_hash2(Eterm term)
{
    EPIPHANY_STUB_FUN();
}

Uint32 make_broken_hash(Eterm term)
{
    EPIPHANY_STUB_FUN();
}

static int do_send_to_logger(Eterm tag, Eterm gleader, char *buf, int len)
{
    EPIPHANY_STUB_FUN();
}

static ERTS_INLINE int
send_info_to_logger(Eterm gleader, char *buf, int len) 
{
    return do_send_to_logger(am_info_msg, gleader, buf, len);
}

static ERTS_INLINE int
send_warning_to_logger(Eterm gleader, char *buf, int len) 
{
    EPIPHANY_STUB_FUN();
}

static ERTS_INLINE int
send_error_to_logger(Eterm gleader, char *buf, int len) 
{
    return do_send_to_logger(am_error, gleader, buf, len);
}

#define LOGGER_DSBUF_INC_SZ 256

static erts_dsprintf_buf_t *
grow_logger_dsbuf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    size_t size;
    size_t free_size = dsbufp->size - dsbufp->str_len;

    ASSERT(dsbufp && dsbufp->str);

    if (need <= free_size)
	return dsbufp;

    size = need - free_size + LOGGER_DSBUF_INC_SZ;
    size = (((size + LOGGER_DSBUF_INC_SZ - 1) / LOGGER_DSBUF_INC_SZ)
	    * LOGGER_DSBUF_INC_SZ);
    size += dsbufp->size;
    ASSERT(dsbufp->str_len + need <= size);
    dsbufp->str = (char *) erts_realloc(ERTS_ALC_T_LOGGER_DSBUF,
					(void *) dsbufp->str,
					size);
    dsbufp->size = size;
    return dsbufp;
}

erts_dsprintf_buf_t *
erts_create_logger_dsbuf(void)
{
    erts_dsprintf_buf_t init = ERTS_DSPRINTF_BUF_INITER(grow_logger_dsbuf);
    erts_dsprintf_buf_t *dsbufp = erts_alloc(ERTS_ALC_T_LOGGER_DSBUF,
					     sizeof(erts_dsprintf_buf_t));
    sys_memcpy((void *) dsbufp, (void *) &init, sizeof(erts_dsprintf_buf_t));
    dsbufp->str = (char *) erts_alloc(ERTS_ALC_T_LOGGER_DSBUF,
				      LOGGER_DSBUF_INC_SZ);
    dsbufp->str[0] = '\0';
    dsbufp->size = LOGGER_DSBUF_INC_SZ;
    return dsbufp;
}

static ERTS_INLINE void
destroy_logger_dsbuf(erts_dsprintf_buf_t *dsbufp)
{
    ASSERT(dsbufp && dsbufp->str);
    erts_free(ERTS_ALC_T_LOGGER_DSBUF, (void *) dsbufp->str);
    erts_free(ERTS_ALC_T_LOGGER_DSBUF, (void *) dsbufp);
}

int
erts_send_info_to_logger(Eterm gleader, erts_dsprintf_buf_t *dsbufp)
{
    int res;
    res = send_info_to_logger(gleader, dsbufp->str, dsbufp->str_len);
    destroy_logger_dsbuf(dsbufp);
    return res;
}

int
erts_send_warning_to_logger(Eterm gleader, erts_dsprintf_buf_t *dsbufp)
{
    int res;
    res = send_warning_to_logger(gleader, dsbufp->str, dsbufp->str_len);
    destroy_logger_dsbuf(dsbufp);
    return res;
}

int
erts_send_error_to_logger(Eterm gleader, erts_dsprintf_buf_t *dsbufp)
{
    int res;
    res = send_error_to_logger(gleader, dsbufp->str, dsbufp->str_len);
    destroy_logger_dsbuf(dsbufp);
    return res;
}

int
erts_send_info_to_logger_str(Eterm gleader, char *str)
{
    return send_info_to_logger(gleader, str, sys_strlen(str));
}

int
erts_send_warning_to_logger_str(Eterm gleader, char *str)
{
    return send_warning_to_logger(gleader, str, sys_strlen(str));
}

int
erts_send_error_to_logger_str(Eterm gleader, char *str)
{
    return send_error_to_logger(gleader, str, sys_strlen(str));
}

int
erts_send_info_to_logger_nogl(erts_dsprintf_buf_t *dsbuf)
{
    return erts_send_info_to_logger(NIL, dsbuf);
}

int
erts_send_warning_to_logger_nogl(erts_dsprintf_buf_t *dsbuf)
{
    return erts_send_warning_to_logger(NIL, dsbuf);
}

int
erts_send_error_to_logger_nogl(erts_dsprintf_buf_t *dsbuf)
{
    return erts_send_error_to_logger(NIL, dsbuf);
}

int
erts_send_info_to_logger_str_nogl(char *str)
{
    return erts_send_info_to_logger_str(NIL, str);
}

int
erts_send_warning_to_logger_str_nogl(char *str)
{
    return erts_send_warning_to_logger_str(NIL, str);
}

int
erts_send_error_to_logger_str_nogl(char *str)
{
    return erts_send_error_to_logger_str(NIL, str);
}


#define TMP_DSBUF_INC_SZ 256

static erts_dsprintf_buf_t *
grow_tmp_dsbuf(erts_dsprintf_buf_t *dsbufp, size_t need)
{
    size_t size;
    size_t free_size = dsbufp->size - dsbufp->str_len;

    ASSERT(dsbufp);

    if (need <= free_size)
	return dsbufp;
    size = need - free_size + TMP_DSBUF_INC_SZ;
    size = ((size + TMP_DSBUF_INC_SZ - 1)/TMP_DSBUF_INC_SZ)*TMP_DSBUF_INC_SZ;
    size += dsbufp->size;
    ASSERT(dsbufp->str_len + need <= size);
    dsbufp->str = (char *) erts_realloc(ERTS_ALC_T_TMP_DSBUF,
					(void *) dsbufp->str,
					size);
    dsbufp->size = size;
    return dsbufp;
}

erts_dsprintf_buf_t *
erts_create_tmp_dsbuf(Uint size)
{
    Uint init_size = size ? size : TMP_DSBUF_INC_SZ;
    erts_dsprintf_buf_t init = ERTS_DSPRINTF_BUF_INITER(grow_tmp_dsbuf);
    erts_dsprintf_buf_t *dsbufp = erts_alloc(ERTS_ALC_T_TMP_DSBUF,
					     sizeof(erts_dsprintf_buf_t));
    sys_memcpy((void *) dsbufp, (void *) &init, sizeof(erts_dsprintf_buf_t));
    dsbufp->str = (char *) erts_alloc(ERTS_ALC_T_TMP_DSBUF, init_size);
    dsbufp->str[0] = '\0';
    dsbufp->size = init_size;
    return dsbufp;
}

void
erts_destroy_tmp_dsbuf(erts_dsprintf_buf_t *dsbufp)
{
    if (dsbufp->str)
	erts_free(ERTS_ALC_T_TMP_DSBUF, (void *) dsbufp->str);
    erts_free(ERTS_ALC_T_TMP_DSBUF, (void *) dsbufp);
}

/* eq and cmp are written as separate functions a eq is a little faster */

/*
 * Test for equality of two terms.
 * Returns 0 if not equal, or a non-zero value otherwise.
 */
#if HALFWORD_HEAP
int eq_rel(Eterm a, Eterm* a_base, Eterm b, Eterm* b_base)
#else
int eq(Eterm a, Eterm b)
#endif
{
    DECLARE_WSTACK(stack);
    Sint sz;
    Eterm* aa;
    Eterm* bb;

tailrecur:
    if (is_same(a, a_base, b, b_base)) goto pop_next;
tailrecur_ne:

    switch (primary_tag(a)) {
    case TAG_PRIMARY_LIST:
	if (is_list(b)) {
	    Eterm* aval = list_val_rel(a, a_base);
	    Eterm* bval = list_val_rel(b, b_base);
	    while (1) {
		Eterm atmp = CAR(aval);
		Eterm btmp = CAR(bval);
		if (!is_same(atmp,a_base,btmp,b_base)) {
		    WSTACK_PUSH2(stack,(UWord) CDR(bval),(UWord) CDR(aval));
		    a = atmp;
		    b = btmp;
		    goto tailrecur_ne;
		}
		atmp = CDR(aval);
		btmp = CDR(bval);
		if (is_same(atmp,a_base,btmp,b_base)) {
		    goto pop_next;
		}
		if (is_not_list(atmp) || is_not_list(btmp)) {
		    a = atmp;
		    b = btmp;
		    goto tailrecur_ne;
		}
		aval = list_val_rel(atmp, a_base);
		bval = list_val_rel(btmp, b_base);
	    }
	}
	break; /* not equal */

    case TAG_PRIMARY_BOXED:
	{	
	    Eterm hdr = *boxed_val_rel(a,a_base);
	    switch (hdr & _TAG_HEADER_MASK) {
	    case ARITYVAL_SUBTAG:
		{
		    aa = tuple_val_rel(a, a_base);
		    if (!is_boxed(b) || *boxed_val_rel(b,b_base) != *aa)
			goto not_equal;
		    bb = tuple_val_rel(b,b_base);
		    if ((sz = arityval(*aa)) == 0) goto pop_next;
		    ++aa;
		    ++bb;
		    goto term_array;
		}
	    case MAP_SUBTAG:
		{
		    aa = map_val_rel(a, a_base);
		    if (!is_boxed(b) || *boxed_val_rel(b,b_base) != *aa)
			goto not_equal;
		    bb = map_val_rel(b,b_base);
		    sz = map_get_size((map_t*)aa);

		    if (sz != map_get_size((map_t*)bb)) goto not_equal;
		    if (sz == 0) goto pop_next;

		    aa += 2;
		    bb += 2;
		    sz += 1; /* increment for tuple-keys */
		    goto term_array;
		}
	    case REFC_BINARY_SUBTAG:
	    case HEAP_BINARY_SUBTAG:
	    case SUB_BINARY_SUBTAG:
		{
		    byte* a_ptr;
		    byte* b_ptr;
		    size_t a_size;
		    size_t b_size;
		    Uint a_bitsize;
		    Uint b_bitsize;
		    Uint a_bitoffs;
		    Uint b_bitoffs;
		    
		    if (!is_binary_rel(b,b_base)) {
			goto not_equal;
		    }
		    a_size = binary_size_rel(a,a_base);
		    b_size = binary_size_rel(b,b_base);
		    if (a_size != b_size) {
			goto not_equal;
		    }
		    ERTS_GET_BINARY_BYTES_REL(a, a_ptr, a_bitoffs, a_bitsize, a_base);
		    ERTS_GET_BINARY_BYTES_REL(b, b_ptr, b_bitoffs, b_bitsize, b_base);
		    if ((a_bitsize | b_bitsize | a_bitoffs | b_bitoffs) == 0) {
			if (sys_memcmp(a_ptr, b_ptr, a_size) == 0) goto pop_next;
		    } else if (a_bitsize == b_bitsize) {
			if (erts_cmp_bits(a_ptr, a_bitoffs, b_ptr, b_bitoffs,
					  (a_size << 3) + a_bitsize) == 0) goto pop_next;
		    }
		    break; /* not equal */
		}
	    case EXPORT_SUBTAG:
		{
		    if (is_export_rel(b,b_base)) {
			Export* a_exp = *((Export **) (export_val_rel(a,a_base) + 1));
			Export* b_exp = *((Export **) (export_val_rel(b,b_base) + 1));
			if (a_exp == b_exp) goto pop_next;
		    }
		    break; /* not equal */
		}
	    case FUN_SUBTAG:
		{
		    ErlFunThing* f1;
		    ErlFunThing* f2;
  
		    if (!is_fun_rel(b,b_base))
			goto not_equal;
		    f1 = (ErlFunThing *) fun_val_rel(a,a_base);
		    f2 = (ErlFunThing *) fun_val_rel(b,b_base);
		    if (f1->fe->module != f2->fe->module ||
			f1->fe->old_index != f2->fe->old_index ||
			f1->fe->old_uniq != f2->fe->old_uniq ||
			f1->num_free != f2->num_free) {
			goto not_equal;
		    }
		    if ((sz = f1->num_free) == 0) goto pop_next;
		    aa = f1->env;
		    bb = f2->env;
		    goto term_array;
		}

	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG: {
		ExternalThing *ap;
		ExternalThing *bp;

		if(!is_external_rel(b,b_base))
		    goto not_equal;

		ap = external_thing_ptr_rel(a,a_base);
		bp = external_thing_ptr_rel(b,b_base);

		if(ap->header == bp->header && ap->node == bp->node) {
		    ASSERT(1 == external_data_words_rel(a,a_base));
		    ASSERT(1 == external_data_words_rel(b,b_base));
		    
		    if (ap->data.ui[0] == bp->data.ui[0]) goto pop_next;
		}
		break; /* not equal */
	    }
	    case EXTERNAL_REF_SUBTAG: {
		/*
		 * Observe!
		 *  When comparing refs we need to compare ref numbers
		 * (32-bit words) *not* ref data words.
		 */
		Uint32 *anum;
		Uint32 *bnum;
		Uint common_len;
		Uint alen;
		Uint blen;
		Uint i;
		ExternalThing* athing;
		ExternalThing* bthing;

		if(!is_external_ref_rel(b,b_base))
		    goto not_equal;

		athing = external_thing_ptr_rel(a,a_base);
		bthing = external_thing_ptr_rel(b,b_base);

		if(athing->node != bthing->node)
		    goto not_equal;

		anum = external_thing_ref_numbers(athing);
		bnum = external_thing_ref_numbers(bthing);
		alen = external_thing_ref_no_of_numbers(athing);
		blen = external_thing_ref_no_of_numbers(bthing);

		goto ref_common;
	    case REF_SUBTAG:
		    if (!is_internal_ref_rel(b,b_base))
			goto not_equal;

		    {
			RefThing* athing = ref_thing_ptr_rel(a,a_base);
			RefThing* bthing = ref_thing_ptr_rel(b,b_base);
			alen = internal_thing_ref_no_of_numbers(athing);
			blen = internal_thing_ref_no_of_numbers(bthing);
			anum = internal_thing_ref_numbers(athing);
			bnum = internal_thing_ref_numbers(bthing);
		    }

	    ref_common:
		    ASSERT(alen > 0 && blen > 0);

		    if (anum[0] != bnum[0])
			goto not_equal;

		    if (alen == 3 && blen == 3) {
			/* Most refs are of length 3 */
			if (anum[1] == bnum[1] && anum[2] == bnum[2]) {
			    goto pop_next; 
			} else {
			    goto not_equal;
			}
		    }

		    common_len = alen;
		    if (blen < alen)
			common_len = blen;

		    for (i = 1; i < common_len; i++)
			if (anum[i] != bnum[i])
			    goto not_equal;

		    if(alen != blen) {

			if (alen > blen) {
			    for (i = common_len; i < alen; i++)
				if (anum[i] != 0)
				    goto not_equal;
			}
			else {
			    for (i = common_len; i < blen; i++)
				if (bnum[i] != 0)
				    goto not_equal;
			}			
		    }
		    goto pop_next;
	    }
	    case POS_BIG_SUBTAG:
	    case NEG_BIG_SUBTAG:
		{
		    int i;
  
		    if (!is_big_rel(b,b_base))
			goto not_equal;
		    aa = big_val_rel(a,a_base);
		    bb = big_val_rel(b,b_base);
		    if (*aa != *bb)
			goto not_equal;
		    i = BIG_ARITY(aa);
		    while(i--) {
			if (*++aa != *++bb)
			    goto not_equal;
		    }
		    goto pop_next;
		}
	    case FLOAT_SUBTAG:
		{
		    FloatDef af;
		    FloatDef bf;
  
		    if (is_float_rel(b,b_base)) {
			GET_DOUBLE_REL(a, af, a_base);
			GET_DOUBLE_REL(b, bf, b_base);
			if (af.fd == bf.fd) goto pop_next;
		    }
		    break; /* not equal */
		}
	    }
	    break;
	}
    }
    goto not_equal;


term_array: /* arrays in 'aa' and 'bb', length in 'sz' */
    ASSERT(sz != 0);
    {
	Eterm* ap = aa;
	Eterm* bp = bb;
	Sint i = sz;
	for (;;) {
	    if (!is_same(*ap,a_base,*bp,b_base)) break;
	    if (--i == 0) goto pop_next;
	    ++ap;
	    ++bp;
	}
	a = *ap;
	b = *bp;
	if (is_both_immed(a,b)) {
	    goto not_equal;
	}
	if (i > 1) { /* push the rest */
	    WSTACK_PUSH3(stack, i-1, (UWord)(bp+1),
			 ((UWord)(ap+1)) | TAG_PRIMARY_HEADER);
	    /* We (ab)use TAG_PRIMARY_HEADER to recognize a term_array */
	}
	goto tailrecur_ne;
    }
   
pop_next:
    if (!WSTACK_ISEMPTY(stack)) {
	UWord something  = WSTACK_POP(stack);
	if (primary_tag((Eterm) something) == TAG_PRIMARY_HEADER) { /* a term_array */
	    aa = (Eterm*) something;
	    bb = (Eterm*) WSTACK_POP(stack);
	    sz = WSTACK_POP(stack);
	    goto term_array;
	}
	a = something;
	b = WSTACK_POP(stack);
	goto tailrecur;
    }

    DESTROY_WSTACK(stack);
    return 1;

not_equal:
    DESTROY_WSTACK(stack);
    return 0;
}

/*
 * Compare objects.
 * Returns 0 if equal, a negative value if a < b, or a positive number a > b.
 *
 * According to the Erlang Standard, types are orderered as follows:
 *   numbers < (characters) < atoms < refs < funs < ports < pids <
 *   tuples < maps < [] < conses < binaries.
 *
 * Note that characters are currently not implemented.
 *
 */


#define float_comp(x,y)    (((x)<(y)) ? -1 : (((x)==(y)) ? 0 : 1))

static int cmp_atoms(Eterm a, Eterm b)
{
    EPIPHANY_STUB_FUN();
}

#if !HALFWORD_HEAP
/* cmp(Eterm a, Eterm b)
 *  For compatibility with HiPE - arith-based compare.
 */
Sint cmp(Eterm a, Eterm b)
{
    return erts_cmp(a, b, 0);
}
#endif

/* erts_cmp(Eterm a, Eterm b, int exact)
 * exact = 1 -> term-based compare
 * exact = 0 -> arith-based compare
 */
#if HALFWORD_HEAP
Sint erts_cmp_rel_opt(Eterm a, Eterm* a_base, Eterm b, Eterm* b_base, int exact)
#else
Sint erts_cmp(Eterm a, Eterm b, int exact)
#endif
{
    DECLARE_WSTACK(stack);
    Eterm* aa;
    Eterm* bb;
    int i;
    Sint j;
    int a_tag;
    int b_tag;
    // Uint adata;
    // Uint bdata;
    // Uint alen;
    // Uint blen;
    // Uint32 *anum;
    // Uint32 *bnum;

#define RETURN_NEQ(cmp) { j=(cmp); ASSERT(j != 0); goto not_equal; }
#define ON_CMP_GOTO(cmp) if ((j=(cmp)) == 0) goto pop_next; else goto not_equal

tailrecur:
    if (is_same(a,a_base,b,b_base)) {	/* Equal values or pointers. */
	goto pop_next;
    }
tailrecur_ne:

    /* deal with majority (?) cases by brute-force */
    if (is_atom(a)) {
	if (is_atom(b)) {
	    ON_CMP_GOTO(cmp_atoms(a, b));
	}
    } else if (is_both_small(a, b)) {
	ON_CMP_GOTO(signed_val(a) - signed_val(b));
    }

    /*
     * Take care of cases where the types are the same.
     */

    a_tag = 42;			/* Suppress warning */
    switch (primary_tag(a)) {
    case TAG_PRIMARY_IMMED1:
	switch ((a & _TAG_IMMED1_MASK) >> _TAG_PRIMARY_SIZE) {
	case (_TAG_IMMED1_PORT >> _TAG_PRIMARY_SIZE):
            // port_common:
            EPIPHANY_STUB(erts_cmp);
		
	case (_TAG_IMMED1_PID >> _TAG_PRIMARY_SIZE):
            // pid_common:
            EPIPHANY_STUB(erts_cmp);
	    
	case (_TAG_IMMED1_SMALL >> _TAG_PRIMARY_SIZE):
	    a_tag = SMALL_DEF;
	    goto mixed_types;
	case (_TAG_IMMED1_IMMED2 >> _TAG_PRIMARY_SIZE): {
	    switch ((a & _TAG_IMMED2_MASK) >> _TAG_IMMED1_SIZE) {
	    case (_TAG_IMMED2_ATOM >> _TAG_IMMED1_SIZE):
		a_tag = ATOM_DEF;
		goto mixed_types;
	    case (_TAG_IMMED2_NIL >> _TAG_IMMED1_SIZE):
		a_tag = NIL_DEF;
		goto mixed_types;
	    }
	}
	}
    case TAG_PRIMARY_LIST:
	if (is_not_list(b)) {
	    a_tag = LIST_DEF;
	    goto mixed_types;
	}
	aa = list_val_rel(a,a_base);
	bb = list_val_rel(b,b_base);
	while (1) {
	    Eterm atmp = CAR(aa);
	    Eterm btmp = CAR(bb);
	    if (!is_same(atmp,a_base,btmp,b_base)) {
		WSTACK_PUSH2(stack,(UWord) CDR(bb),(UWord) CDR(aa));
		a = atmp;
		b = btmp;
		goto tailrecur_ne;
	    }
	    atmp = CDR(aa);
	    btmp = CDR(bb);
	    if (is_same(atmp,a_base,btmp,b_base)) {
		goto pop_next;
	    }
	    if (is_not_list(atmp) || is_not_list(btmp)) {
		a = atmp;
		b = btmp;
		goto tailrecur_ne;
	    }
	    aa = list_val_rel(atmp,a_base);
	    bb = list_val_rel(btmp,b_base);
	}
    case TAG_PRIMARY_BOXED:
	{
	    Eterm ahdr = *boxed_val_rel(a,a_base);
	    switch ((ahdr & _TAG_HEADER_MASK) >> _TAG_PRIMARY_SIZE) {
	    case (_TAG_HEADER_ARITYVAL >> _TAG_PRIMARY_SIZE):
		if (!is_tuple_rel(b,b_base)) {
		    a_tag = TUPLE_DEF;
		    goto mixed_types;
		}
		aa = tuple_val_rel(a,a_base);
		bb = tuple_val_rel(b,b_base);
		/* compare the arities */
		i = arityval(ahdr);	/* get the arity*/
		if (i != arityval(*bb)) {
		    RETURN_NEQ((int)(i - arityval(*bb)));
		}
		if (i == 0) {
		    goto pop_next;
		}
		++aa;
		++bb;
		goto term_array;
	    case (_TAG_HEADER_MAP >> _TAG_PRIMARY_SIZE) :
		if (!is_map_rel(b,b_base)) {
		    a_tag = MAP_DEF;
		    goto mixed_types;
		}
		aa = (Eterm *)map_val_rel(a,a_base);
		bb = (Eterm *)map_val_rel(b,b_base);

		i = map_get_size((map_t*)aa);
		if (i != map_get_size((map_t*)bb)) {
		    RETURN_NEQ((int)(i - map_get_size((map_t*)bb)));
		}
		if (i == 0) {
		    goto pop_next;
		}
		aa += 2;
		bb += 2;
		i  += 1; /* increment for tuple-keys */
		goto term_array;
	    case (_TAG_HEADER_FLOAT >> _TAG_PRIMARY_SIZE):
		if (!is_float_rel(b,b_base)) {
		    a_tag = FLOAT_DEF;
		    goto mixed_types;
		} else {
		    FloatDef af;
		    FloatDef bf; 

		    GET_DOUBLE_REL(a, af, a_base);
		    GET_DOUBLE_REL(b, bf, b_base);
		    ON_CMP_GOTO(float_comp(af.fd, bf.fd));
		}
	    case (_TAG_HEADER_POS_BIG >> _TAG_PRIMARY_SIZE):
	    case (_TAG_HEADER_NEG_BIG >> _TAG_PRIMARY_SIZE):
		if (!is_big_rel(b,b_base)) {
		    a_tag = BIG_DEF;
		    goto mixed_types;
		}
		ON_CMP_GOTO(big_comp(rterm2wterm(a,a_base), rterm2wterm(b,b_base)));
	    case (_TAG_HEADER_EXPORT >> _TAG_PRIMARY_SIZE):
		if (!is_export_rel(b,b_base)) {
		    a_tag = EXPORT_DEF;
		    goto mixed_types;
		} else {
		    Export* a_exp = *((Export **) (export_val_rel(a,a_base) + 1));
		    Export* b_exp = *((Export **) (export_val_rel(b,b_base) + 1));

		    if ((j = cmp_atoms(a_exp->code[0], b_exp->code[0])) != 0) {
			RETURN_NEQ(j);
		    }
		    if ((j = cmp_atoms(a_exp->code[1], b_exp->code[1])) != 0) {
			RETURN_NEQ(j);
		    }
		    ON_CMP_GOTO((Sint) a_exp->code[2] - (Sint) b_exp->code[2]);
		}
		break;
	    case (_TAG_HEADER_FUN >> _TAG_PRIMARY_SIZE):
		if (!is_fun_rel(b,b_base)) {
		    a_tag = FUN_DEF;
		    goto mixed_types;
		} else {
                    EPIPHANY_STUB(erts_cmp);
		}
	    case (_TAG_HEADER_EXTERNAL_PID >> _TAG_PRIMARY_SIZE):
                EPIPHANY_STUB(erts_cmp);

	    case (_TAG_HEADER_EXTERNAL_PORT >> _TAG_PRIMARY_SIZE):
                EPIPHANY_STUB(erts_cmp);

	    case (_TAG_HEADER_REF >> _TAG_PRIMARY_SIZE):
                // ref_common:
                EPIPHANY_STUB(erts_cmp);

	    case (_TAG_HEADER_EXTERNAL_REF >> _TAG_PRIMARY_SIZE):
                EPIPHANY_STUB(erts_cmp);

            default:
		/* Must be a binary */
		ASSERT(is_binary_rel(a,a_base));
		if (!is_binary_rel(b,b_base)) {
		    a_tag = BINARY_DEF;
		    goto mixed_types;
		} else {
		    Uint a_size = binary_size_rel(a,a_base);
		    Uint b_size = binary_size_rel(b,b_base);
		    Uint a_bitsize;
		    Uint b_bitsize;
		    Uint a_bitoffs;
		    Uint b_bitoffs;
		    Uint min_size;
		    int cmp;
		    byte* a_ptr;
		    byte* b_ptr;
		    ERTS_GET_BINARY_BYTES_REL(a, a_ptr, a_bitoffs, a_bitsize, a_base);
		    ERTS_GET_BINARY_BYTES_REL(b, b_ptr, b_bitoffs, b_bitsize, b_base);
		    if ((a_bitsize | b_bitsize | a_bitoffs | b_bitoffs) == 0) {
			min_size = (a_size < b_size) ? a_size : b_size;
			if ((cmp = sys_memcmp(a_ptr, b_ptr, min_size)) != 0) {
			    RETURN_NEQ(cmp);
			}
		    }
		    else {
			a_size = (a_size << 3) + a_bitsize;
			b_size = (b_size << 3) + b_bitsize;
			min_size = (a_size < b_size) ? a_size : b_size;
			if ((cmp = erts_cmp_bits(a_ptr,a_bitoffs,
						 b_ptr,b_bitoffs,min_size)) != 0) {
			    RETURN_NEQ(cmp);
			}
		    }
		    ON_CMP_GOTO((Sint)(a_size - b_size));
		}
	    }
	}
    }

    /*
     * Take care of the case that the tags are different.
     */

 mixed_types:

    {
	FloatDef f1, f2;
	Eterm big;
#if HALFWORD_HEAP
	Wterm aw = is_immed(a) ? a : rterm2wterm(a,a_base);
	Wterm bw = is_immed(b) ? b : rterm2wterm(b,b_base);
#else
	Eterm aw = a;
	Eterm bw = b;
#endif
#define MAX_LOSSLESS_FLOAT ((double)((1LL << 53) - 2))
#define MIN_LOSSLESS_FLOAT ((double)(((1LL << 53) - 2)*-1))
#define BIG_ARITY_FLOAT_MAX (1024 / D_EXP) /* arity of max float as a bignum */
	Eterm big_buf[BIG_NEED_SIZE(BIG_ARITY_FLOAT_MAX)];

	b_tag = tag_val_def(bw);

	switch(_NUMBER_CODE(a_tag, b_tag)) {
	case SMALL_BIG:
	    j = big_sign(bw) ? 1 : -1;
	    break;
	case BIG_SMALL:
	    j = big_sign(aw) ? -1 : 1;
	    break;
	case SMALL_FLOAT:
	    if (exact) goto exact_fall_through;
	    GET_DOUBLE(bw, f2);
	    if (f2.fd < MAX_LOSSLESS_FLOAT && f2.fd > MIN_LOSSLESS_FLOAT) {
		/* Float is within the no loss limit */
		f1.fd = signed_val(aw);
		j = float_comp(f1.fd, f2.fd);
	    }
#if ERTS_SIZEOF_ETERM == 8
	    else if (f2.fd > (double) (MAX_SMALL + 1)) {
		/* Float is a positive bignum, i.e. bigger */
		j = -1;
	    } else if (f2.fd < (double) (MIN_SMALL - 1)) {
		/* Float is a negative bignum, i.e. smaller */
		j = 1;
	    } else {
		/* Float is a Sint but less precise */
		j = signed_val(aw) - (Sint) f2.fd;
	    }
#else
	    else {
		/* If float is positive it is bigger than small */
		j = (f2.fd > 0.0) ? -1 : 1;
	    }
#endif /* ERTS_SIZEOF_ETERM == 8 */
	    break;
        case FLOAT_BIG:
	    if (exact) goto exact_fall_through;
	{
	    Wterm tmp = aw;
	    aw = bw;
	    bw = tmp;
	}/* fall through */
	case BIG_FLOAT:
	    if (exact) goto exact_fall_through;
	    GET_DOUBLE(bw, f2);
	    if ((f2.fd < (double) (MAX_SMALL + 1))
		    && (f2.fd > (double) (MIN_SMALL - 1))) {
		/* Float is a Sint */
		j = big_sign(aw) ? -1 : 1;
	    } else if (big_arity(aw) > BIG_ARITY_FLOAT_MAX
		       || pow(2.0,(big_arity(aw)-1)*D_EXP) > fabs(f2.fd)) {
		/* If bignum size shows that it is bigger than the abs float */
		j = big_sign(aw) ? -1 : 1;
	    } else if (big_arity(aw) < BIG_ARITY_FLOAT_MAX
		       && (pow(2.0,(big_arity(aw))*D_EXP)-1.0) < fabs(f2.fd)) {
		/* If bignum size shows that it is smaller than the abs float */
		j = f2.fd < 0 ? 1 : -1;
	    } else if (f2.fd < MAX_LOSSLESS_FLOAT && f2.fd > MIN_LOSSLESS_FLOAT) {
		/* Float is within the no loss limit */
		if (big_to_double(aw, &f1.fd) < 0) {
		    j = big_sign(aw) ? -1 : 1;
		} else {
		    j = float_comp(f1.fd, f2.fd);
		}
	    } else {
		big = double_to_big(f2.fd, big_buf, sizeof(big_buf)/sizeof(Eterm));
		j = big_comp(aw, rterm2wterm(big,big_buf));
	    }
	    if (_NUMBER_CODE(a_tag, b_tag) == FLOAT_BIG) {
		j = -j;
	    }
	    break;
	case FLOAT_SMALL:
	    if (exact) goto exact_fall_through;
	    GET_DOUBLE(aw, f1);
	    if (f1.fd < MAX_LOSSLESS_FLOAT && f1.fd > MIN_LOSSLESS_FLOAT) {
		/* Float is within the no loss limit */
		f2.fd = signed_val(bw);
		j = float_comp(f1.fd, f2.fd);
	    }
#if ERTS_SIZEOF_ETERM == 8
	    else if (f1.fd > (double) (MAX_SMALL + 1)) {
		/* Float is a positive bignum, i.e. bigger */
		j = 1;
	    } else if (f1.fd < (double) (MIN_SMALL - 1)) {
		/* Float is a negative bignum, i.e. smaller */
		j = -1;
	    } else {
		/* Float is a Sint but less precise it */
		j = (Sint) f1.fd - signed_val(bw);
	    }
#else
	    else {
		/* If float is positive it is bigger than small */
		j = (f1.fd > 0.0) ? 1 : -1;
	    }
#endif /* ERTS_SIZEOF_ETERM == 8 */
	    break;
exact_fall_through:
	default:
	    j = b_tag - a_tag;
	}
    }
    if (j == 0) {
	goto pop_next; 
    } else {
	goto not_equal;
    }

term_array: /* arrays in 'aa' and 'bb', length in 'i' */
    ASSERT(i>0);
    while (--i) {
	a = *aa++;
	b = *bb++;
	if (!is_same(a,a_base, b,b_base)) {
	    if (is_atom(a) && is_atom(b)) {
		if ((j = cmp_atoms(a, b)) != 0) {
		    goto not_equal;
		}
	    } else if (is_both_small(a, b)) {
		if ((j = signed_val(a)-signed_val(b)) != 0) {
		    goto not_equal;
		}
	    } else {
		/* (ab)Use TAG_PRIMARY_HEADER to recognize a term_array */
		WSTACK_PUSH3(stack, i, (UWord)bb, (UWord)aa | TAG_PRIMARY_HEADER);
		goto tailrecur_ne;
	    }
	}
    }
    a = *aa;
    b = *bb;
    goto tailrecur;    
   
pop_next:
    if (!WSTACK_ISEMPTY(stack)) {
	UWord something = WSTACK_POP(stack);
	if (primary_tag((Eterm) something) == TAG_PRIMARY_HEADER) { /* a term_array */
	    aa = (Eterm*) something;
	    bb = (Eterm*) WSTACK_POP(stack);
	    i = WSTACK_POP(stack);
	    goto term_array;
	}
	a = (Eterm) something;
	b = (Eterm) WSTACK_POP(stack);
	goto tailrecur;
    }

    DESTROY_WSTACK(stack);
    return 0;

not_equal:
    DESTROY_WSTACK(stack);
    return j;
}


Eterm
store_external_or_ref_(Uint **hpp, ErlOffHeap* oh, Eterm ns)
{
    Uint i;
    Uint size;
    Uint *from_hp;
    Uint *to_hp = *hpp;

    ASSERT(is_external(ns) || is_internal_ref(ns));

    if(is_external(ns)) {
	from_hp = external_val(ns);
	size = thing_arityval(*from_hp) + 1;
	*hpp += size;

	for(i = 0; i < size; i++)
	    to_hp[i] = from_hp[i];

	erts_refc_inc(&((ExternalThing *) to_hp)->node->refc, 2);

	((struct erl_off_heap_header*) to_hp)->next = oh->first;
	oh->first = (struct erl_off_heap_header*) to_hp;

	return make_external(to_hp);
    }

    /* Internal ref */
    from_hp = internal_ref_val(ns);

    size = thing_arityval(*from_hp) + 1;

    *hpp += size;

    for(i = 0; i < size; i++)
	to_hp[i] = from_hp[i];

    return make_internal_ref(to_hp);
}

Eterm
store_external_or_ref_in_proc_(Process *proc, Eterm ns)
{
    Uint sz;
    Uint *hp;

    ASSERT(is_external(ns) || is_internal_ref(ns));

    sz = NC_HEAP_SIZE(ns);
    ASSERT(sz > 0);
    hp = HAlloc(proc, sz);
    return store_external_or_ref_(&hp, &MSO(proc), ns);
}

void bin_write(int to, void *to_arg, byte* buf, size_t sz)
{
    size_t i;

    for (i=0;i<sz;i++) {
	if (IS_DIGIT(buf[i]))
	    erts_print(to, to_arg, "%d,", buf[i]);
	else if (IS_PRINT(buf[i])) {
	    erts_print(to, to_arg, "%c,", buf[i]);
	}
	else
	    erts_print(to, to_arg, "%d,", buf[i]);
    }
    erts_putc(to, to_arg, '\n');
}

/* Fill buf with the contents of bytelist list 
   return number of chars in list or -1 for error */

int
intlist_to_buf(Eterm list, char *buf, int len)
{
    Eterm* listptr;
    int sz = 0;

    if (is_nil(list)) 
	return 0;
    if (is_not_list(list))
	return -1;
    listptr = list_val(list);

    while (sz < len) {
	if (!is_byte(*listptr)) 
	    return -1;
	buf[sz++] = unsigned_val(*listptr);
	if (is_nil(*(listptr + 1)))
	    return(sz);
	if (is_not_list(*(listptr + 1))) 
	    return -1;
	listptr = list_val(*(listptr + 1));
    }
    return -1;			/* not enough space */
}

/*
** Convert an integer to a byte list
** return pointer to converted stuff (need not to be at start of buf!)
*/
char* Sint_to_buf(Sint n, struct Sint_buf *buf)
{
    char* p = &buf->s[sizeof(buf->s)-1];
    int sign = 0;

    *p-- = '\0'; /* null terminate */
    if (n == 0)
	*p-- = '0';
    else if (n < 0) {
	sign = 1;
	n = -n;
    }

    while (n != 0) {
	*p-- = (n % 10) + '0';
	n /= 10;
    }
    if (sign)
	*p-- = '-';
    return p+1;
}

/* Build a list of integers in some safe memory area
** Memory must be pre allocated prio call 2*len in size
** hp is a pointer to the "heap" pointer on return
** this pointer is updated to point after the list
*/

Eterm
buf_to_intlist(Eterm** hpp, const char *buf, size_t len, Eterm tail)
{
    Eterm* hp = *hpp;
    size_t i = len;

    while(i != 0) {
	--i;
	tail = CONS(hp, make_small((Uint)(byte)buf[i]), tail);
	hp += 2;
    }

    *hpp = hp;
    return tail;
}

/*
 * Return 0 if successful, and non-zero if unsuccessful.
 *
 * It is vital that if erts_iolist_to_buf would return an error for
 * any type of term data, this function should do so as well.
 * Any input term error detected in erts_iolist_to_buf should also
 * be detected in this function!
 */

static ERTS_INLINE int
iolist_size(const int yield_support, ErtsIOListState *state, Eterm obj, ErlDrvSizeT* sizep)
{
    int res, init_yield_count, yield_count;
    Eterm* objp;
    Uint size = (Uint) *sizep; /* Intentionally Uint due to halfword heap */
    DECLARE_ESTACK(s);

    if (!yield_support)
	yield_count = init_yield_count = 0; /* Shut up faulty warning... >:-( */
    else {
	if (state->reds_left <= 0)
	    return ERTS_IOLIST_YIELD;
	ESTACK_CHANGE_ALLOCATOR(s, ERTS_ALC_T_SAVED_ESTACK);
	init_yield_count = ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED;
	init_yield_count *= state->reds_left;
	yield_count = init_yield_count;
	if (state->estack.start) {
	    /* Restart; restore state... */
	    ESTACK_RESTORE(s, &state->estack);
	    size = (Uint) state->size;
	    obj = state->obj;
	}
    }

    goto L_again;

#define SAFE_ADD(Var, Val)			\
    do {					\
        Uint valvar = (Val);			\
	Var += valvar;				\
	if (Var < valvar) {			\
	    goto L_overflow_error;		\
	}					\
    } while (0)

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	    while (1) { /* Tail loop */
		while (1) { /* Head loop */
		    if (yield_support && --yield_count <= 0)
			goto L_yield;
		    objp = list_val(obj);
		    /* Head */
		    obj = CAR(objp);
		    if (is_byte(obj)) {
			size++;
			if (size == 0) {
			    goto L_overflow_error;
			}
		    } else if (is_binary(obj) && binary_bitsize(obj) == 0) {
			SAFE_ADD(size, binary_size(obj));
		    } else if (is_list(obj)) {
			ESTACK_PUSH(s, CDR(objp));
			continue; /* Head loop */
		    } else if (is_not_nil(obj)) {
			goto L_type_error;
		    }
		    break;
		}
		/* Tail */
		obj = CDR(objp);
		if (is_list(obj))
		    continue; /* Tail loop */
		else if (is_binary(obj) && binary_bitsize(obj) == 0) {
		    SAFE_ADD(size, binary_size(obj));
		} else if (is_not_nil(obj)) {
		    goto L_type_error;
		}
		break;
	    }
	} else {
	    if (yield_support && --yield_count <= 0)
		goto L_yield;
	    if (is_binary(obj) && binary_bitsize(obj) == 0) { /* Tail was binary */
		SAFE_ADD(size, binary_size(obj));
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	}
    }
#undef SAFE_ADD

    *sizep = (ErlDrvSizeT) size;

    res = ERTS_IOLIST_OK;

 L_return:

    DESTROY_ESTACK(s);

    if (yield_support) {
	int yc, reds;
	CLEAR_SAVED_ESTACK(&state->estack);
	yc = init_yield_count - yield_count;
	reds = ((yc - 1) / ERTS_IOLIST_SIZE_YIELDS_COUNT_PER_RED) + 1;
	BUMP_REDS(state->c_p, reds);
	state->reds_left -= reds;
	state->size = (ErlDrvSizeT) size;
	state->have_size = 1;
    }

    return res;

 L_overflow_error:
    res = ERTS_IOLIST_OVERFLOW;
    size = 0;
    goto L_return;

 L_type_error:
    res = ERTS_IOLIST_TYPE;
    size = 0;
    goto L_return;

 L_yield:
    BUMP_ALL_REDS(state->c_p);
    state->reds_left = 0;
    state->size = size;
    state->obj = obj;
    ESTACK_SAVE(s, &state->estack);
    return ERTS_IOLIST_YIELD;
}

int erts_iolist_size_yielding(ErtsIOListState *state)
{
    ErlDrvSizeT size = state->size;
    return iolist_size(1, state, state->obj, &size);
}

int erts_iolist_size(Eterm obj, ErlDrvSizeT* sizep)
{
    *sizep = 0;
    return iolist_size(0, NULL, obj, sizep);
}

/* return 0 if item is not a non-empty flat list of bytes */
int
is_string(Eterm list)
{
    int len = 0;

    while(is_list(list)) {
	Eterm* consp = list_val(list);
	Eterm hd = CAR(consp);

	if (!is_byte(hd))
	    return 0;
	len++;
	list = CDR(consp);
    }
    if (is_nil(list))
	return len;
    return 0;
}

#ifdef ERTS_SMP

/*
 * Process and Port timers in smp case
 */

#define ERTS_PTMR_FLGS_ALLCD_SIZE \
  2
#define ERTS_PTMR_FLGS_ALLCD_MASK \
  ((((Uint32) 1) << ERTS_PTMR_FLGS_ALLCD_SIZE) - 1)

#define ERTS_PTMR_FLGS_PREALLCD	((Uint32) 1)
#define ERTS_PTMR_FLGS_SLALLCD	((Uint32) 2)
#define ERTS_PTMR_FLGS_LLALLCD	((Uint32) 3)
#define ERTS_PTMR_FLG_CANCELLED	(((Uint32) 1) << (ERTS_PTMR_FLGS_ALLCD_SIZE+0))

static void
init_ptimers(void)
{
    // ESTUB
}

void
erts_create_smp_ptimer(ErtsSmpPTimer **timer_ref,
		       Eterm id,
		       ErlTimeoutProc timeout_func,
		       Uint timeout)
{
    EPIPHANY_STUB_FUN();
}

void
erts_cancel_smp_ptimer(ErtsSmpPTimer *ptimer)
{
    EPIPHANY_STUB_FUN();
}

#endif

static int trim_threshold;
static int top_pad;
static int mmap_threshold;
static int mmap_max;

Uint tot_bin_allocated;

void erts_init_utils(void)
{
#ifdef ERTS_SMP
    init_ptimers();
#endif
}

void erts_init_utils_mem(void) 
{
    trim_threshold = -1;
    top_pad = -1;
    mmap_threshold = -1;
    mmap_max = -1;
}

int
sys_alloc_opt(int opt, int value)
{
#if HAVE_MALLOPT
  int m_opt;
  int *curr_val;

  switch(opt) {
  case SYS_ALLOC_OPT_TRIM_THRESHOLD:
#ifdef M_TRIM_THRESHOLD
    m_opt = M_TRIM_THRESHOLD;
    curr_val = &trim_threshold;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_TOP_PAD:
#ifdef M_TOP_PAD
    m_opt = M_TOP_PAD;
    curr_val = &top_pad;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_MMAP_THRESHOLD:
#ifdef M_MMAP_THRESHOLD
    m_opt = M_MMAP_THRESHOLD;
    curr_val = &mmap_threshold;
    break;
#else
    return 0;
#endif
  case SYS_ALLOC_OPT_MMAP_MAX:
#ifdef M_MMAP_MAX
    m_opt = M_MMAP_MAX;
    curr_val = &mmap_max;
    break;
#else
    return 0;
#endif
  default:
    return 0;
  }

  if(mallopt(m_opt, value)) {
    *curr_val = value;
    return 1;
  }

#endif /* #if HAVE_MALLOPT */

  return 0;
}

void
sys_alloc_stat(SysAllocStat *sasp)
{
   sasp->trim_threshold = trim_threshold;
   sasp->top_pad        = top_pad;
   sasp->mmap_threshold = mmap_threshold;
   sasp->mmap_max       = mmap_max;

}

typedef struct {
    size_t sz;
    char *ptr;
} ErtsEmuArg;

typedef struct {
    int argc;
    ErtsEmuArg *arg;
    size_t no_bytes;
} ErtsEmuArgs;

ErtsEmuArgs saved_emu_args = {0};

void
erts_save_emu_args(int argc, char **argv)
{
#ifdef DEBUG
    char *end_ptr;
#endif
    char *ptr;
    int i;
    size_t arg_sz[100];
    size_t size;

    ASSERT(!saved_emu_args.argc);

    size = sizeof(ErtsEmuArg)*argc;
    for (i = 0; i < argc; i++) {
	size_t sz = sys_strlen(argv[i]);
	if (i < sizeof(arg_sz)/sizeof(arg_sz[0]))
	    arg_sz[i] = sz;
	size += sz+1;
    } 
    ptr = (char *) malloc(size);
    if (!ptr) {
        ERTS_INTERNAL_ERROR("malloc failed to allocate memory!");
    }
#ifdef DEBUG
    end_ptr = ptr + size;
#endif
    saved_emu_args.arg = (ErtsEmuArg *) ptr;
    ptr += sizeof(ErtsEmuArg)*argc;
    saved_emu_args.argc = argc;
    saved_emu_args.no_bytes = 0;
    for (i = 0; i < argc; i++) {
	size_t sz;
	if (i < sizeof(arg_sz)/sizeof(arg_sz[0]))
	    sz = arg_sz[i];
	else
	    sz = sys_strlen(argv[i]);
	saved_emu_args.arg[i].ptr = ptr;
	saved_emu_args.arg[i].sz = sz;
	saved_emu_args.no_bytes += sz;
	ptr += sz+1;
	sys_strcpy(saved_emu_args.arg[i].ptr, argv[i]);
    }
    ASSERT(ptr == end_ptr);
}

Eterm
erts_get_emu_args(Process *c_p)
{
#ifdef DEBUG
    Eterm *end_hp;
#endif
    int i;
    Uint hsz;
    Eterm *hp, res;

    hsz = saved_emu_args.no_bytes*2;
    hsz += saved_emu_args.argc*2;

    hp = HAlloc(c_p, hsz);
#ifdef DEBUG
    end_hp = hp + hsz;
#endif
    res = NIL;

    for (i = saved_emu_args.argc-1; i >= 0; i--) {
    Eterm arg = buf_to_intlist(&hp,
				   saved_emu_args.arg[i].ptr,
				   saved_emu_args.arg[i].sz,
				   NIL);
	res = CONS(hp, arg, res);
	hp += 2;
    }

    ASSERT(hp == end_hp);

    return res;
}


Eterm
erts_get_ethread_info(Process *c_p)
{
    Uint sz, *szp;
    Eterm res, *hp, **hpp, *end_hp = NULL;

    sz = 0;
    szp = &sz;
    hpp = NULL;

    while (1) {
	Eterm tup, list, name;
#if defined(ETHR_NATIVE_ATOMIC32_IMPL)	  \
    || defined(ETHR_NATIVE_ATOMIC64_IMPL)	\
    || defined(ETHR_NATIVE_DW_ATOMIC_IMPL)
	char buf[1024];
	int i;
	char **str;
#endif

	res = NIL;

#ifdef ETHR_X86_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "sse2"),
#ifdef ETHR_X86_RUNTIME_CONF_HAVE_SSE2__
			     erts_bld_string(hpp, szp,
					     (ETHR_X86_RUNTIME_CONF_HAVE_SSE2__
					      ? "yes" : "no"))
#else
			     erts_bld_string(hpp, szp, "yes")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp,
					     "x86"
#ifdef ARCH_64
					     "_64"
#endif
					     " OOO"),
			     erts_bld_string(hpp, szp,
#ifdef ETHR_X86_OUT_OF_ORDER
					     "yes"
#else
					     "no"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);
#endif

#ifdef ETHR_SPARC_V9_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Sparc V9"),
			     erts_bld_string(hpp, szp,
#if defined(ETHR_SPARC_TSO)
					     "TSO"
#elif defined(ETHR_SPARC_PSO)
					     "PSO"
#elif defined(ETHR_SPARC_RMO)
					     "RMO"
#else
					     "undefined"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);

#endif

#ifdef ETHR_PPC_MEMBAR_H__

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "lwsync"),
			     erts_bld_string(hpp, szp,
#if defined(ETHR_PPC_HAVE_LWSYNC)
					     "yes"
#elif defined(ETHR_PPC_HAVE_NO_LWSYNC)
					     "no"
#elif defined(ETHR_PPC_RUNTIME_CONF_HAVE_LWSYNC__)
					     ETHR_PPC_RUNTIME_CONF_HAVE_LWSYNC__ ? "yes" : "no"
#else
					     "undefined"
#endif
				 ));

	res = erts_bld_cons(hpp, szp, tup, res);

#endif

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Native rw-spinlocks"),
#ifdef ETHR_NATIVE_RWSPINLOCK_IMPL
			     erts_bld_string(hpp, szp, ETHR_NATIVE_RWSPINLOCK_IMPL)
#else
			     erts_bld_string(hpp, szp, "no")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);

	tup = erts_bld_tuple(hpp, szp, 2,
			     erts_bld_string(hpp, szp, "Native spinlocks"),
#ifdef ETHR_NATIVE_SPINLOCK_IMPL
			     erts_bld_string(hpp, szp, ETHR_NATIVE_SPINLOCK_IMPL)
#else
			     erts_bld_string(hpp, szp, "no")
#endif
	    );
	res = erts_bld_cons(hpp, szp, tup, res);


	list = NIL;
#ifdef ETHR_NATIVE_DW_ATOMIC_IMPL
	if (ethr_have_native_dw_atomic()) {
	    name = erts_bld_string(hpp, szp, ETHR_NATIVE_DW_ATOMIC_IMPL);
	    str = ethr_native_dw_atomic_ops();
	    for (i = 0; str[i]; i++) {
		erts_snprintf(buf, sizeof(buf), "ethr_native_dw_atomic_%s()", str[i]);
		list = erts_bld_cons(hpp, szp,
				     erts_bld_string(hpp, szp, buf),
				     list);
	    }
	    str = ethr_native_su_dw_atomic_ops();
	    for (i = 0; str[i]; i++) {
		erts_snprintf(buf, sizeof(buf), "ethr_native_su_dw_atomic_%s()", str[i]);
		list = erts_bld_cons(hpp, szp,
				     erts_bld_string(hpp, szp, buf),
				     list);
	    }
	}
	else 
#endif
	    name = erts_bld_string(hpp, szp, "no");

	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "Double word native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	list = NIL;
#ifdef ETHR_NATIVE_ATOMIC64_IMPL
	name = erts_bld_string(hpp, szp, ETHR_NATIVE_ATOMIC64_IMPL);
	str = ethr_native_atomic64_ops();
	for (i = 0; str[i]; i++) {
	    erts_snprintf(buf, sizeof(buf), "ethr_native_atomic64_%s()", str[i]);
	    list = erts_bld_cons(hpp, szp,
				 erts_bld_string(hpp, szp, buf),
				 list);
	}
#else
	name = erts_bld_string(hpp, szp, "no");
#endif
	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "64-bit native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	list = NIL;
#ifdef ETHR_NATIVE_ATOMIC32_IMPL
	name = erts_bld_string(hpp, szp, ETHR_NATIVE_ATOMIC32_IMPL);
	str = ethr_native_atomic32_ops();
	for (i = 0; str[i]; i++) {
	    erts_snprintf(buf, sizeof(buf), "ethr_native_atomic32_%s()", str[i]);
	    list = erts_bld_cons(hpp, szp,
				erts_bld_string(hpp, szp, buf),
				list);
	}
#else
	name = erts_bld_string(hpp, szp, "no");
#endif
	tup = erts_bld_tuple(hpp, szp, 3,
			     erts_bld_string(hpp, szp, "32-bit native atomics"),
			     name,
			     list);
	res = erts_bld_cons(hpp, szp, tup, res);

	if (hpp) {
	    HRelease(c_p, end_hp, *hpp)
	    return res;
	}

	hp = HAlloc(c_p, sz);
	end_hp = hp + sz;
	hpp = &hp;
	szp = NULL;
    }
}

/*
 * To be used to silence unused result warnings, but do not abuse it.
 */
void erts_silence_warn_unused_result(long unused)
{

}

/*
 * Interval counts
 */
void
erts_interval_init(erts_interval_t *icp)
{
#ifdef ARCH_64
    erts_atomic_init_nob(&icp->counter.atomic, 0);
#else
    erts_dw_aint_t dw;
#ifdef ETHR_SU_DW_NAINT_T__
    dw.dw_sint = 0;
#else
    dw.sint[ERTS_DW_AINT_HIGH_WORD] = 0;
    dw.sint[ERTS_DW_AINT_LOW_WORD] = 0;
#endif
    erts_dw_atomic_init_nob(&icp->counter.atomic, &dw);

#endif
#ifdef DEBUG
    icp->smp_api = 0;
#endif
}

void
erts_smp_interval_init(erts_interval_t *icp)
{
#ifdef ERTS_SMP
    erts_interval_init(icp);
#else
    icp->counter.not_atomic = 0;
#endif
#ifdef DEBUG
    icp->smp_api = 1;
#endif
}

static ERTS_INLINE Uint64
step_interval_nob(erts_interval_t *icp)
{
#ifdef ARCH_64
    return (Uint64) erts_atomic_inc_read_nob(&icp->counter.atomic);
#else
    erts_dw_aint_t exp;

    erts_dw_atomic_read_nob(&icp->counter.atomic, &exp);
    while (1) {
	erts_dw_aint_t new = exp;

#ifdef ETHR_SU_DW_NAINT_T__
	new.dw_sint++;
#else
	new.sint[ERTS_DW_AINT_LOW_WORD]++;
	if (new.sint[ERTS_DW_AINT_LOW_WORD] == 0)
	    new.sint[ERTS_DW_AINT_HIGH_WORD]++;
#endif

	if (erts_dw_atomic_cmpxchg_nob(&icp->counter.atomic, &new, &exp))
	    return erts_interval_dw_aint_to_val__(&new);

    }
#endif
}

static ERTS_INLINE Uint64
step_interval_relb(erts_interval_t *icp)
{
#ifdef ARCH_64
    return (Uint64) erts_atomic_inc_read_relb(&icp->counter.atomic);
#else
    erts_dw_aint_t exp;

    erts_dw_atomic_read_nob(&icp->counter.atomic, &exp);
    while (1) {
	erts_dw_aint_t new = exp;

#ifdef ETHR_SU_DW_NAINT_T__
	new.dw_sint++;
#else
	new.sint[ERTS_DW_AINT_LOW_WORD]++;
	if (new.sint[ERTS_DW_AINT_LOW_WORD] == 0)
	    new.sint[ERTS_DW_AINT_HIGH_WORD]++;
#endif

	if (erts_dw_atomic_cmpxchg_relb(&icp->counter.atomic, &new, &exp))
	    return erts_interval_dw_aint_to_val__(&new);

    }
#endif
}


static ERTS_INLINE Uint64
ensure_later_interval_nob(erts_interval_t *icp, Uint64 ic)
{
    Uint64 curr_ic;
#ifdef ARCH_64
    curr_ic = (Uint64) erts_atomic_read_nob(&icp->counter.atomic);
    if (curr_ic > ic)
	return curr_ic;
    return (Uint64) erts_atomic_inc_read_nob(&icp->counter.atomic);
#else
    erts_dw_aint_t exp;

    erts_dw_atomic_read_nob(&icp->counter.atomic, &exp);
    curr_ic = erts_interval_dw_aint_to_val__(&exp);
    if (curr_ic > ic)
	return curr_ic;

    while (1) {
	erts_dw_aint_t new = exp;

#ifdef ETHR_SU_DW_NAINT_T__
	new.dw_sint++;
#else
	new.sint[ERTS_DW_AINT_LOW_WORD]++;
	if (new.sint[ERTS_DW_AINT_LOW_WORD] == 0)
	    new.sint[ERTS_DW_AINT_HIGH_WORD]++;
#endif

	if (erts_dw_atomic_cmpxchg_nob(&icp->counter.atomic, &new, &exp))
	    return erts_interval_dw_aint_to_val__(&new);

	curr_ic = erts_interval_dw_aint_to_val__(&exp);
	if (curr_ic > ic)
	    return curr_ic;
    }
#endif
}


static ERTS_INLINE Uint64
ensure_later_interval_acqb(erts_interval_t *icp, Uint64 ic)
{
    Uint64 curr_ic;
#ifdef ARCH_64
    curr_ic = (Uint64) erts_atomic_read_acqb(&icp->counter.atomic);
    if (curr_ic > ic)
	return curr_ic;
    return (Uint64) erts_atomic_inc_read_acqb(&icp->counter.atomic);
#else
    erts_dw_aint_t exp;

    erts_dw_atomic_read_acqb(&icp->counter.atomic, &exp);
    curr_ic = erts_interval_dw_aint_to_val__(&exp);
    if (curr_ic > ic)
	return curr_ic;

    while (1) {
	erts_dw_aint_t new = exp;

#ifdef ETHR_SU_DW_NAINT_T__
	new.dw_sint++;
#else
	new.sint[ERTS_DW_AINT_LOW_WORD]++;
	if (new.sint[ERTS_DW_AINT_LOW_WORD] == 0)
	    new.sint[ERTS_DW_AINT_HIGH_WORD]++;
#endif

	if (erts_dw_atomic_cmpxchg_acqb(&icp->counter.atomic, &new, &exp))
	    return erts_interval_dw_aint_to_val__(&new);

	curr_ic = erts_interval_dw_aint_to_val__(&exp);
	if (curr_ic > ic)
	    return curr_ic;
    }
#endif
}

Uint64
erts_step_interval_nob(erts_interval_t *icp)
{
    ASSERT(!icp->smp_api);
    return step_interval_nob(icp);
}

Uint64
erts_step_interval_relb(erts_interval_t *icp)
{
    ASSERT(!icp->smp_api);
    return step_interval_relb(icp);
}

Uint64
erts_smp_step_interval_nob(erts_interval_t *icp)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return step_interval_nob(icp);
#else
    return ++icp->counter.not_atomic;
#endif
}

Uint64
erts_smp_step_interval_relb(erts_interval_t *icp)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return step_interval_relb(icp);
#else
    return ++icp->counter.not_atomic;
#endif
}

Uint64
erts_ensure_later_interval_nob(erts_interval_t *icp, Uint64 ic)
{
    ASSERT(!icp->smp_api);
    return ensure_later_interval_nob(icp, ic);
}

Uint64
erts_ensure_later_interval_acqb(erts_interval_t *icp, Uint64 ic)
{
    ASSERT(!icp->smp_api);
    return ensure_later_interval_acqb(icp, ic);
}

Uint64
erts_smp_ensure_later_interval_nob(erts_interval_t *icp, Uint64 ic)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return ensure_later_interval_nob(icp, ic);
#else
    if (icp->counter.not_atomic > ic)
	return icp->counter.not_atomic;
    else
	return ++icp->counter.not_atomic;
#endif
}

Uint64
erts_smp_ensure_later_interval_acqb(erts_interval_t *icp, Uint64 ic)
{
    ASSERT(icp->smp_api);
#ifdef ERTS_SMP
    return ensure_later_interval_acqb(icp, ic);
#else
    if (icp->counter.not_atomic > ic)
	return icp->counter.not_atomic;
    else
	return ++icp->counter.not_atomic;
#endif
}

/*
 * A millisecond timestamp without time correction where there's no hrtime
 * - for tracing on "long" things...
 */
Uint64 erts_timestamp_millis(void)
{
#ifdef HAVE_GETHRTIME
    return (Uint64) (sys_gethrtime() / 1000000);
#else
    Uint64 res;
    SysTimeval tv;
    sys_gettimeofday(&tv);
    res = (Uint64) tv.tv_sec*1000000;
    res += (Uint64) tv.tv_usec;
    return (res / 1000);
#endif
}

#ifdef DEBUG
/*
 * Handy functions when using a debugger - don't use in the code!
 */

void upp(byte *buf, size_t sz)
{
    bin_write(ERTS_PRINT_STDERR, NULL, buf, sz);
}

void pat(Eterm atom)
{
    EPIPHANY_STUB_FUN();
}


void pinfo()
{
    //ESTUB
    erts_printf("pinfo: not printing, is stubbed out\n");
}


void pp(p)
Process *p;
{
    if(p)
        erts_fprintf(stderr, "pp: not printing, is stubbed out\n");
}

void td(Eterm x)
{
    erts_fprintf(stderr, "%T\n", x);
}

void
ps(Process* p, Eterm* stop)
{
    Eterm* sp = STACK_START(p) - 1;

    if (stop <= STACK_END(p)) {
        stop = STACK_END(p) + 1;
    }

    while(sp >= stop) {
	erts_printf("%p: %.75T\n", sp, *sp);
	sp--;
    }
}
#endif


