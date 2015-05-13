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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_bits.h"

#if defined(ERTS_SMP)
void
erts_bits_init_state(ERL_BITS_PROTO_0)
{
    // Do nothing
}
#endif

int
erts_cmp_bits(byte* a_ptr, size_t a_offs, byte* b_ptr, size_t b_offs, size_t size)
{
    EPIPHANY_STUB_BT();
    return 0;
}

void
erts_copy_bits(byte* src,	/* Base pointer to source. */
	       size_t soffs,	/* Bit offset for source relative to src. */
	       int sdir,	/* Direction: 1 (forward) or -1 (backward). */
	       byte* dst,	/* Base pointer to destination. */
	       size_t doffs,	/* Bit offset for destination relative to dst. */
	       int ddir,	/* Direction: 1 (forward) or -1 (backward). */
	       size_t n)	/* Number of bits to copy. */
{
    EPIPHANY_STUB_BT();
}
