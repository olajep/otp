/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2003-2011. All Rights Reserved.
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


#ifndef HIPE_SLAVE_H
#define HIPE_SLAVE_H

extern const void *hipe_slave_arch_primop_address(Eterm key);

/* used by beam_load.c:patch(). patchtype == am_load_fe, Value is an ErlFunEntry* */
extern void hipe_slave_patch_address(Uint *address, Eterm patchtype, Uint value);
extern void hipe_slave_patch_load_fe(Uint *address, Uint value);
extern int hipe_slave_patch_insn(void *address, Uint value, Eterm type);
extern int hipe_slave_patch_call(void *callAddress, void *destAddress, void *trampoline);

extern void *hipe_slave_make_native_stub(void *beamAddress, unsigned int beamArity);

/* Return the address of a stub switching a native closure call to BEAM. */
extern const void *hipe_slave_closure_stub_address(unsigned int arity);

/* #ifdef ERTS_SLAVE */
#include "hipe_epiphany_slave.h"
/* #endif */

#ifdef HIPE_SLAVE_USE_CACHE
struct sdesc;
extern void *hipe_slave_cache_insert(void *address, Uint nrbytes,
				     Uint num_tramp, int sdesc_count,
				     struct sdesc **sdescs, Eterm mfa);
#endif

#if !defined(AEXTERN)
#define AEXTERN(RET,NAME,PROTO)	extern RET NAME PROTO
#endif

#endif /* HIPE_SLAVE_H */
