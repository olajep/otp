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
/*
 * Stack walking helpers for slave native stack GC procedures.
 * Epiphany version.
 */
#ifndef HIPE_SLAVE_EPIPHANY_GC_H
#define HIPE_SLAVE_EPIPHANY_GC_H

#undef F_TIMO
#undef THE_NON_VALUE
#undef ERL_FUN_SIZE
#include "hipe_literals.h" /* For EPIPHANY_NR_ARG_REGS */

#define SLAVE_NR_ARG_REGS EPIPHANY_NR_ARG_REGS
#include "hipe_slave_risc_gc.h"

#endif /* HIPE_SLAVE_EPIPHANY_GC_H */
