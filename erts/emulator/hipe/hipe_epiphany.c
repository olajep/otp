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
#include "hipe_slave_cache.h"

#ifndef HIPE_EPIPHANY_C_SLAVE_MODE
#  include "hipe_arch.h"
#  ifndef ERTS_SLAVE
#    error This module only supports slave mode
#  endif
#  define GLOBAL_HIPE_FUN(Name) hipe_ ## Name
#else
/* Only for SLAVE_SYM_nbif_callemu; can we get rid of this? */
#  include "slave_syms.h"

#  include "slave_io.h" /* for slave_workgroup.num_cores */
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
reachable_pcrel(const void *ptr, const void *from_start, Uint from_len)
{
    const Uint farthest_distance = (1<<(23+1)) - 1;
    return from_len < farthest_distance
	&& llabs((long long)(Uint)ptr - (Uint)from_start)
	    < farthest_distance
	&& llabs((long long)(Uint)ptr - (Uint)(from_start + from_len))
	    < farthest_distance;
}

static Uint32 *
try_alloc(Uint codebytes, int nrcallees, Uint32 **trampvec)
{
    Uint32 *address, *trampoline;
    int trampnr;

    /*
     * To facilitate caching, trampolines are always emitted and are not reused.
     */
    Uint codewords = (codebytes + 3) / 4;
    Uint nrbytes = 4 * (3 * nrcallees + codewords);

    /*
     * There's no memory protection to deal with on Epiphany -- just use the
     * shared memory allocator!
     */
    address = erts_alloc(ERTS_ALC_T_HIPE_SLAVE, nrbytes);
    /* erts_alloc cannot return NULL; it aborts when out of memory */

    trampoline = address + codewords;
    for (trampnr = 1; trampnr <= nrcallees; ++trampnr) {

	trampoline[0] = LITTLE32(0x2002800b); /* mov  r12, 0 */
	trampoline[1] = LITTLE32(0x3002800b); /* movt r12, 0 */
	trampoline[2] = LITTLE32(0x0402114f); /* jr   r12 */
	/* No icache and coherent DRAM access on the Epiphany; no flushing
	 * required. */

	trampvec[trampnr-1] = trampoline;
	trampoline += 3;
	ASSERT((void*)trampoline <= (((void*)address)+nrbytes));
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

    address = try_alloc(nrbytes, nrcallees, trampvec);
    if (address) {
	*trampolines = trampvecbin;
    }
    return address;
}

static Uint32 *alloc_stub(Uint nrbytes)
{
    return try_alloc(nrbytes, 0, NULL);
}

static void free_stub(void *stub)
{
    erts_free(ERTS_ALC_T_HIPE_SLAVE, stub);
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

#define LOAD32(P) load_unaligned_32(P)
static Uint32 load_unaligned_32(const Uint32 *address) {
    const char *p = (const char*)address;
    return p[0] | p[1] << 8 | p[2] << 16 | p[3] << 24;
}

static void patch_simm24(void* address, Sint32 immediate) {
    Uint32 value = LOAD32(address);
    value &= ~simm24(-1); /* Clear immediate field */
    value |= simm24(immediate);
    EMIT32(address, value);
}

static void patch_uimm16(void* address, Uint16 immediate) {
    Uint32 value = LOAD32(address);
    value &= ~uimm16(~0); /* Clear immediate field */
    value |= uimm16(immediate);
    EMIT32(address, value);
}

void GLOBAL_HIPE_FUN(patch_load_fe)(Uint32 *address, Uint32 value) {
    ASSERT(!(value >> 16));
    patch_uimm16(address, value);
}

int GLOBAL_HIPE_FUN(patch_insn)(void *address, Uint32 value, Eterm type)
{
    /* Maximum immediate size is 16 bits */
    if (value >> 16) return -1;

    switch (type) {
      case am_closure:
      case am_constant:
      case am_atom:
      case am_c_const:
	break;
      default:
	return -1;
    }
    patch_uimm16(address, value);
    return 0;
}

void *
GLOBAL_HIPE_FUN(make_native_stub)(void *beamAddress, unsigned int beamArity)
{
    char *ret, *code;
#ifdef HIPE_EPIPHANY_C_SLAVE_MODE
    const void *nbif_callemu_addr = (const void*) SLAVE_SYM_nbif_callemu;
#else
    const void *nbif_callemu_addr = &nbif_callemu;
#endif
    int long_jump = 0;

    while(1) {
	ret = code = (char*)alloc_stub((long_jump ? 5 : 3) * 4 + 2);

	/*
	 * Native code calls BEAM via a stub looking as follows:
	 *
	 * mov  r8, %low(beamAddress)
	 * mov  r0, #beamArity
	 * movt r8, %high(beamAddress)
	 * b    nbif_callemu
	 *
	 * Sometimes, nbif_callemu is not reachable by a "b" instruction. In that
	 * case, a slightly longer stub is used:
	 *
	 * mov  r12, %low(nbif_callemu)
	 * mov  r8,  %low(beamAddress)
	 * movt r12, %high(nbif_callemu)
	 * mov  r0,  #beamArity
	 * movt r8,  %high(beamAddress)
	 * jr   r12
	 *
	 * I'm using r0 and r8 since they aren't used for parameter passing in
	 * native code. r12 is the scratch that trampolines use to construct the
	 * callee address; we're thus free to use it for that purpose since any
	 * callers of this stub must consider that they might need trampolines
	 * to reach here.
	 */

	if (!long_jump
	    && !reachable_pcrel(nbif_callemu_addr, ret + 2 * 4 + 2, 0)) {
	    /* Retry with a longjump */
	    free_stub(ret);
	    long_jump = 1;
	    continue;
	}

	/* mov  r12, %low(nbif_callemu) */
	if (long_jump)
	    EMIT32(code, 0x2002800b | uimm16((Uint)nbif_callemu_addr & 0xffff));
	/* mov r8, %low(beamAddress) */
	EMIT32(code, 0x2002000b | uimm16((Uint)beamAddress & 0xffff));
	/* movt r12, %high(nbif_callemu) */
	if (long_jump)
	    EMIT32(code, 0x3002800b | uimm16((Uint)nbif_callemu_addr >> 16));
	/* mov r0, #beamArity*/
	EMIT16(code, 0x0003 | uimm8(beamArity));
	/* movt r8, %high(beamAddress) */
	EMIT32(code, 0x3002000b | uimm16((Uint)beamAddress >> 16));
	if (long_jump) {
	    /* jr   r12 */
	    EMIT32(code, 0x0402114f);
	} else {
	    /* b nbif_callemu */
	    EMIT32(code, 0x000000e8
		   | simm24(((char*)nbif_callemu_addr - code) >> 1));
	}

	return ret;
    }
}

int
GLOBAL_HIPE_FUN(patch_call)(void *callAddress, void *destAddress, void *trampoline)
{
    /* We unconditionally use the trampolines to facilitate caching. */
    if (reachable_pcrel(trampoline, callAddress, 0)) {
	/* Update the trampoline's address computation.
	   (May be redundant, but we can't tell.) */
	patch_uimm16(trampoline,   (Uint32)destAddress & 0xffff);
	patch_uimm16(trampoline+4, (Uint32)destAddress >> 16);
	/* Update this call site. */
	patch_simm24(callAddress, (trampoline - callAddress) >> 1);
    } else {
	return -1;
    }
    return 0;
}

void *
GLOBAL_HIPE_FUN(cache_insert)(void *address, Uint codebytes, Uint nrcallees,
			      int sdesc_count, struct sdesc **sdescs, Eterm mfa)
{
#ifdef HIPE_EPIPHANY_C_SLAVE_MODE
    void *hipe_cold_call_addr = (void*) SLAVE_SYM_hipe_cold_call;
    const int corecount = slave_workgroup.num_cores;
#else
    void *hipe_cold_call_addr = &hipe_cold_call;
    const int corecount = epiphany_workgroup_size();
#endif

    /*
     * To facilitate caching, trampolines are always emitted and are not reused.
     */
    Uint codewords = (codebytes + 3) / 4;
    Uint nrbytes = 4 * (3 * nrcallees + codewords);

    int coreix;
    struct fun_entrypoint *entry
	= erts_alloc(ERTS_ALC_T_HIPE_SLAVE, sizeof(*entry)
		     + sizeof(*entry->table) * corecount);
    ASSERT(corecount > 0 && corecount <= 1024);

    /* mov  r12, %low(table) */
    entry->code[0] = LITTLE32(0x2002800b | uimm16((Uint)(&entry->table[0].address) & 0xffff));
    /* movt r12, %high(table) */
    entry->code[1] = LITTLE32(0x3002800b | uimm16((Uint)(&entry->table[0].address) >> 16));
    entry->code[2] = LITTLE32(0x06001149); /* ldr r0, [r12,r34] (r34 = CORIX) */
    entry->code[3] = LITTLE32(0x0002014f); /* jr.l r0 */

    entry->cold_address = address;
    entry->size = nrbytes;
    entry->sdescs = sdescs;
    entry->sdesc_count = sdesc_count;

#ifdef DEBUG
    if (is_tuple_arity(mfa, 3) && is_atom(tuple_val(mfa)[1])
	&& is_atom(tuple_val(mfa)[2]) && is_byte(tuple_val(mfa)[3])) {
	entry->dbg_M = tuple_val(mfa)[1];
	entry->dbg_F = tuple_val(mfa)[2];
	entry->dbg_A = unsigned_val(tuple_val(mfa)[3]);
    } else {
	entry->dbg_M = entry->dbg_F = am_undefined;
	entry->dbg_A = 0;
    }
#endif

    for (coreix = 0; coreix < corecount; coreix++) {
	entry->table[coreix].address = hipe_cold_call_addr;
	entry->table[coreix].count = 0;
    }

    return entry->code;
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
