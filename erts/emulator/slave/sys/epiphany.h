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
#define EPIPHANY_SRAM_FUNC __attribute__((section(".data_bank1")))

struct workgroup_coords {
    char row, col;
};

struct workgroup_dimens {
    char rows, cols;
};

int epiphany_in_emulator(void);
int epiphany_coreno(void);
int epiphany_workgroup_size(void);
struct workgroup_coords epiphany_workgroup_origin(void);
struct workgroup_dimens epiphany_workgroup_dimens(void);

void epiphany_backtrace(void);

#ifdef DEBUG
int epiphany_in_dram(void *);
int epiphany_sane_address(void *);
#endif

#endif /* defined(_SYS_EPIPHANY_H) */
