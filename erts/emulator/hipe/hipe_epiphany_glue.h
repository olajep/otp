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


#ifndef HIPE_EPIPHANY_GLUE_H
#define HIPE_EPIPHANY_GLUE_H

#include "hipe_epiphany_asm.h"		/* for NR_ARG_REGS, EPIPHANY_LEAF_WORDS */
#define NR_LEAF_WORDS			EPIPHANY_LEAF_WORDS
#define HIPE_ARCH_CALL_TO_NATIVE	hipe_epiphany_call_to_native
#define HIPE_ARCH_RETURN_TO_NATIVE	hipe_epiphany_return_to_native
#define HIPE_ARCH_TAILCALL_TO_NATIVE	hipe_epiphany_tailcall_to_native
#define HIPE_ARCH_THROW_TO_NATIVE	hipe_epiphany_throw_to_native
#include "hipe_risc_glue.h"

#endif /* HIPE_EPIPHANY_GLUE_H */
