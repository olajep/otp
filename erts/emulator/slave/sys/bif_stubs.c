/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2001-2015. All Rights Reserved.
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
 * Weakly stubs all BIFs
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "global.h"
#include "error.h"
#include "bif.h"

#define STUB_BIF_0(N) BIF_RETTYPE __attribute__((weak)) N(BIF_ALIST_0) { EPIPHANY_STUB_BT(); return THE_NON_VALUE; }
#define STUB_BIF_1(N) BIF_RETTYPE __attribute__((weak)) N(BIF_ALIST_1) { EPIPHANY_STUB_BT(); return THE_NON_VALUE; }
#define STUB_BIF_2(N) BIF_RETTYPE __attribute__((weak)) N(BIF_ALIST_2) { EPIPHANY_STUB_BT(); return THE_NON_VALUE; }
#define STUB_BIF_3(N) BIF_RETTYPE __attribute__((weak)) N(BIF_ALIST_3) { EPIPHANY_STUB_BT(); return THE_NON_VALUE; }

#define BIF_LIST(M,F,A,C,I) STUB_BIF_##A(C)
#include "erl_bif_list.h"
#undef BIF_LIST

Eterm
erts_bif_trace(int bif_index, Process* p, Eterm* args, BeamInstr* I)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_mixed_plus(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_mixed_minus(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_mixed_times(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_mixed_div(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_int_div(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_int_rem(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_band(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_bor(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_bxor(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_bnot(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}


Eterm __attribute__((weak))
erts_gc_length_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_size_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_bit_size_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_byte_size_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_map_size_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_abs_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_float_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_round_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_trunc_1(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_binary_part_3(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

Eterm __attribute__((weak))
erts_gc_binary_part_2(Process* p, Eterm* reg, Uint live)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}
