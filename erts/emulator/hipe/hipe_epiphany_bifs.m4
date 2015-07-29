changecom(`/*', `*/')dnl
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


include(`hipe/hipe_epiphany_asm.m4')
#`include' "config.h"
#`include' "hipe_literals.h"

	.text
        .p2align 1

`#if defined(ERTS_ENABLE_LOCK_CHECK) && defined(ERTS_SMP)
#  define CALL_BIF(F)	ldr r?, =F; str r?, [r0, #P_BIF_CALLEE]; bl hipe_debug_bif_wrapper
#else
#  define CALL_BIF(F)	bl	F
#endif'

