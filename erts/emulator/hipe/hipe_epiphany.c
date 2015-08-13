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
 */


#include <stddef.h>	/* offsetof() */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "global.h"
#include "erl_binary.h"

#include "hipe_native_bif.h"	/* nbif_callemu() */
#include "hipe_bif0.h"

#ifndef HIPE_EPIPHANY_C_SLAVE_MODE
#  include "hipe_arch.h"
#  ifndef ERTS_SLAVE
#    error This module only supports slave mode
#  endif
#  define GLOBAL_HIPE_FUN(Name) hipe_ ## Name
#else
#  include "hipe_slave.h"
#  define GLOBAL_HIPE_FUN(Name) hipe_slave_ ## Name
#  define MODE am_slave
#endif

/*
 * In case we want to load code from a big-endian host (will probably never
 * happen)
 */
#ifdef WORDS_BIGENDIAN
#  include <byteswap.h>
#  define LITTLE32(X) (__bswap_32(X))
#  define LITTLE16(X) (__bswap_16(X))
#else
#  define LITTLE32(X) (X)
#  define LITTLE16(X) (X)
#endif

#ifndef ERTS_SLAVE
static int check_callees(Eterm callees)
{
    Eterm *tuple;
    Uint arity;
    Uint i;

    if (is_not_tuple(callees))
	return -1;
    tuple = tuple_val(callees);
    arity = arityval(tuple[0]);
    for (i = 1; i <= arity; ++i) {
	Eterm mfa = tuple[i];
	if (is_atom(mfa))
	    continue;
	if (is_not_tuple(mfa) ||
	    tuple_val(mfa)[0] != make_arityval(3) ||
	    is_not_atom(tuple_val(mfa)[1]) ||
	    is_not_atom(tuple_val(mfa)[2]) ||
	    is_not_small(tuple_val(mfa)[3]) ||
	    unsigned_val(tuple_val(mfa)[3]) > 255)
	    return -1;
    }
    return arity;
}

static int
reachable_pcrel(void *ptr, void *from_start, Uint from_len)
{
    const Uint farthest_distance = (1<<(23+1)) - 1;
    return from_len < farthest_distance
	&& llabs((long long)(Uint)ptr - (Uint)from_start)
	    < farthest_distance
	&& llabs((long long)(Uint)ptr - (Uint)(from_start + from_len))
	    < farthest_distance;
}

static Uint32 *
try_alloc(Uint nrbytes, int nrcallees, Eterm callees, Uint32 **trampvec)
{
    Uint32 *address;
    int trampnr;

    /*
     * There's no memory protection to deal with on Epiphany -- just use the
     * shared memory allocator!
     */
    address = erts_alloc(ERTS_ALC_T_HIPE_SLAVE, nrbytes);
    if (!address) return NULL;

    for (trampnr = 1; trampnr <= nrcallees; ++trampnr) {
	Eterm m, f, mfa = tuple_val(callees)[trampnr];
	unsigned int a;
	Uint32 *trampoline;
	if (is_atom(mfa))
	    trampoline = hipe_primop_get_trampoline(MODE, mfa);
	else {
	    m = tuple_val(mfa)[1];
	    f = tuple_val(mfa)[2];
	    a = unsigned_val(tuple_val(mfa)[3]);
	    trampoline = hipe_mfa_get_trampoline(MODE, m, f, a);
	}
	if (!trampoline || !reachable_pcrel(trampoline, address, nrbytes)) {
	    /* We expect this case to be fairly rare */
	    trampoline = erts_alloc(ERTS_ALC_T_HIPE_SLAVE,
				    3 * sizeof(Uint32));
	    trampoline[0] = LITTLE32(0x2002800b); /* mov  r12, 0 */
	    trampoline[1] = LITTLE32(0x3002800b); /* movt r12, 0 */
	    trampoline[2] = LITTLE32(0x0402114f); /* jr   r12 */
	    /* No icache and coherent DRAM access on the Epiphany; no flushing
	     * required. */
	    if (is_atom(mfa))
		hipe_primop_set_trampoline(MODE, mfa, trampoline);
	    else
		hipe_mfa_set_trampoline(MODE, m, f, a, trampoline);
	}
	trampvec[trampnr-1] = trampoline;
    }
    return address;
}

void *
GLOBAL_HIPE_FUN(alloc_code)(Uint nrbytes, Eterm callees, Eterm *trampolines,
			    Process *p)
{
    int nrcallees;
    Eterm trampvecbin;
    Uint32 **trampvec, *address;

    nrcallees = check_callees(callees);
    if (nrcallees < 0)
	return NULL;
    trampvecbin = new_binary(p, NULL, nrcallees*sizeof(Uint32*));
    trampvec = (Uint32**)binary_bytes(trampvecbin);

    address = try_alloc(nrbytes, nrcallees, callees, trampvec);
    if (address) {
	*trampolines = trampvecbin;
    }
    return address;
}

static Uint32 *alloc_stub(Uint nrbytes)
{
    return try_alloc(nrbytes, 0, NIL, NULL);
}

#define EMIT16(P,C) ({				    \
	    char *__p = (P);			    \
	    Uint16 __c = (C);			    \
	    *(__p++) = __c & 0xff;		    \
	    *(__p++) = __c >> 8;		    \
	    (P) = __p;				    \
	})

/* This macro must deal with a misaligned P, we can't simply cast it to Uint32*
 * and write to it. */
#define EMIT32(P,C) ({				    \
	    char *__p = (P);			    \
	    Uint32 __c = (C);			    \
	    *(__p++) =  __c        & 0xff;	    \
	    *(__p++) = (__c >> 8)  & 0xff;	    \
	    *(__p++) = (__c >> 16) & 0xff;	    \
	    *(__p++) =  __c >> 24;		    \
	    (P) = __p;				    \
	})

static Uint16 uimm8(char immediate) {
    return (Uint16)immediate << 5;
}

static Uint32 uimm16(Uint16 immediate) {
    return uimm8(immediate & 0xff)
	| (((Uint32)immediate >> 8) << 20);
}

static Uint32 simm24(Sint32 immediate) {
    return immediate << 8;
}

static void patch_simm24(Uint16* address, Sint32 immediate) {
    Uint32 value = address[0] | address[1] << 16;
    value &= ~simm24(-1); /* Clear immediate field */
    value |= simm24(immediate);
    address[0] = value;
    address[1] = value >> 16;
}

static Uint32 patch_li(Uint32 *address, Uint32 value)
{
    erl_exit(1, "Immediate patching for epiphany needs some love");
}

void GLOBAL_HIPE_FUN(patch_load_fe)(Uint32 *address, Uint32 value)
{
    patch_li(address, value);
}

int GLOBAL_HIPE_FUN(patch_insn)(void *address, Uint32 value, Eterm type)
{
    switch (type) {
      case am_closure:
      case am_constant:
      case am_atom:
      case am_c_const:
	break;
      default:
	return -1;
    }
    patch_li((Uint32*)address, value);
    return 0;
}

void *
GLOBAL_HIPE_FUN(make_native_stub)(void *beamAddress, unsigned int beamArity)
{
    char *ret, *code;
    ret = code = (char*)alloc_stub(3 * 4 + 2);

    /*
     * Native code calls BEAM via a stub looking as follows:
     *
     * mov  r8, %low(beamAddress)
     * mov  r0, #beamArity
     * movt r8, %high(beamAddress)
     * b    nbif_callemu
     *
     * I'm using r0 and r8 since they aren't used for parameter passing in
     * native code. Trampolines (for b/bl to distant targets) may modify r12.
     */

    /*
     * It is not a given that we will be able to reach nbif_callemu with a
     * branch.
     */
    if (!reachable_pcrel(&nbif_callemu, ret + 2 * 4 + 2, 0))
	abort();

    /* mov r8, %low(beamAddress) */
    EMIT32(code, 0x2002000b | uimm16((Uint)beamAddress & 0xffff));
    /* mov r0, #beamArity*/
    EMIT16(code, 0x0003 | uimm8(beamArity));
    /* movt r8, %high(beamAddress) */
    EMIT32(code, 0x3002000b | uimm16((Uint)beamAddress >> 16));
    /* b nbif_callemu */
    EMIT32(code, 0x000000e8 | simm24(((char*)&nbif_callemu - code) >> 1));

    return ret;
}

int
GLOBAL_HIPE_FUN(patch_call)(void *callAddress, void *destAddress, void *trampoline)
{
    if (reachable_pcrel(destAddress, callAddress, 0)) {
	/* The destination can be reached with a b/bl instruction.
	   This is typical for nearby Erlang code. */
	patch_simm24(callAddress, (destAddress - callAddress) >> 1);
    } else {
	/* The destination is too distant for b/bl.
	   Must do a b/bl to the trampoline. */
	if (reachable_pcrel(trampoline, callAddress, 0)) {
	    /* Update the trampoline's address computation.
	       (May be redundant, but we can't tell.) */
	    patch_li(trampoline, (Uint32)destAddress);
	    /* Update this call site. */
	    patch_simm24(callAddress, (trampoline - callAddress) >> 1);
	} else {
	    return -1;
	}
    }
    return 0;
}

#endif /* !ERTS_SLAVE */

#ifndef HIPE_EPIPHANY_C_SLAVE_MODE
void
hipe_arch_print_pcb(struct hipe_process_state *p)
{
#define U(n,x) \
    printf(" % 4d | %s | 0x%0*lx | %*s |\r\n", \
	   (int)offsetof(struct hipe_process_state,x), n, 2*(int)sizeof(long), \
	   (unsigned long)p->x, 2+2*(int)sizeof(long), "")
    U("nra        ", nra);
    U("narity     ", narity);
#undef U
}
#endif
