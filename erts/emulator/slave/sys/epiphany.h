/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2015. All Rights Reserved.
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

#ifndef _SYS_EPIPHANY_H_
#define _SYS_EPIPHANY_H_

#define EPIPHANY_SRAM_DATA __attribute__((section(".data_bank0")))
#define EPIPHANY_SRAM_FUNC __attribute__((section(".text_bank0")))
#define EPIPHANY_SRAM_HEAP __attribute__((section(".local_heap")))

int epiphany_in_emulator(void);
int epiphany_coreno(void);
int epiphany_workgroup_size(void);
void epiphany_workgroup_origin(unsigned *row, unsigned *col);
void epiphany_workgroup_dimens(unsigned *rows, unsigned *cols);

void epiphany_backtrace(void);
void returning_abort(void);

int epiphany_in_dram(void *);

#ifdef DEBUG
int epiphany_sane_address(void *);
#endif

void epiphany_dram_fence(void);

#endif /* defined(_SYS_EPIPHANY_H) */
