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
#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "epiphany.h"
#include <e-lib.h>

int
epiphany_in_emulator(void)
{
    return e_group_config.group_rows == 0
        && e_group_config.group_cols == 0;
}

int
epiphany_coreno(void)
{
    e_coreid_t id = e_get_coreid();
    unsigned row, col;
    struct workgroup_coords origin = epiphany_workgroup_origin();
    struct workgroup_dimens dimens = epiphany_workgroup_dimens();
    e_coords_from_coreid(id, &row, &col);

    return (row - origin.row) * dimens.cols + (col - origin.col);
}

struct workgroup_dimens
epiphany_workgroup_dimens(void)
{
    if (!epiphany_in_emulator()) {
	struct workgroup_dimens dimens = {
	    .rows = e_group_config.group_rows,
	    .cols = e_group_config.group_cols,
	};
	return dimens;
    } else {
	// The emulator does not set e_group_config. Assume a P16 4x4 workgroup.
	struct workgroup_dimens dimens = { .rows = 4, .cols = 4, };
	return dimens;
    }
}

int
epiphany_workgroup_size(void)
{
    struct workgroup_dimens dimens = epiphany_workgroup_dimens();
    return dimens.rows * dimens.cols;
}


struct workgroup_coords
epiphany_workgroup_origin(void)
{
    if (!epiphany_in_emulator()) {
	struct workgroup_coords origin = { .row = 0, .col = 0 };
	return origin;
    } else {
	// The emulator does not set e_group_config. Assume a P16 4x4 workgroup.
	struct workgroup_coords origin = { .row = 32, .col = 8 };
#ifdef DEBUG
	e_coreid_t id = e_get_coreid();
	unsigned row, col;
	e_coords_from_coreid(id, &row, &col);
	ASSERT(32 <= row && row < 36);
	ASSERT(8 <= col && col < 12);
#endif
	return origin;
    }
}

int
epiphany_in_dram(void *addr)
{
    return 0x8e000000 <= (unsigned)addr && (unsigned)addr < 0x90000000;
}

int
epiphany_sane_address(void *addrp)
{
    e_coreid_t owning;
    unsigned row, col, min_row, min_col, max_row, max_col;
    unsigned addr = (unsigned)addrp;

    // Shared DRAM
    if (epiphany_in_dram(addrp)) return 1;

    // In memory space of core in workgroup
    owning = addr >> (32 - 12);
    e_coords_from_coreid(owning, &row, &col);
    min_row = e_group_config.group_row;
    max_row = min_row + e_group_config.group_rows;
    min_col = e_group_config.group_col;
    max_col = min_col + e_group_config.group_cols;
    if (owning == 0
	|| ((min_row <= row && row < max_row)
	    && (min_col <= col && col < max_col))) {
	unsigned corespc = addr & (1024 * 1024 - 1);
	// Local SRAM
	if (corespc < 32 * 1024) return 1;

	// Memory-mapped registers
	if (corespc >= 0x000F0000) return 1;
    }
    return 0;
}

#define LDR_ANY_MASK  0b00011110000000000001110001111111
#define LDR_LR_MASK   0b11111110000000001111110001111111
#define LDR_ANY_MATCH 0b00000100000000000001010001001100
#define LDR_LR_MATCH  0b00100100000000001101010001001100

#define ADD_SP_MASK  0b11111100000000001111110001111111
#define ADD_SP_MATCH 0b00100100000000001011010000011011

#define RTS_MASK  0b00011100000011110001111111111111
#define RTS_MATCH 0b00000100000000100001100101001111


#define IMM11(INSTR) ((((INSTR) >> 13) & (0xff << 3)) | (((INSTR) >> 7) & 0x7))
#define IMM11_SIGN(INSTR) ((INSTR) & (1 << 24))

// Sign-extend IMM11
#define SIMM11(INSTR) ((((signed)IMM11(INSTR)) << (32-11)) >> (32-11))

#define HARDDEBUG 0

#if HARDDEBUG
#  define HDEBUG_PRINTF(X ...) erts_printf(X)
#else
#  define HDEBUG_PRINTF(X ...)
#endif

/*
 * Scans for the instruction sequence
 *  ldr lr,[sp,(*lr_offset)>>2]
 *   Any number of ldr ?, [sp, ?]
 *  add sp,sp,*framesize
 *   Any number of ldr ?, [sp, ?]
 *  rts
 * starting at address ptr. Returns zero if found and nonzero if not.
 */
static inline int
scan_epilogue(unsigned *__attribute__((packed)) ptr, int *framesize, int *lr_offset)
{
    extern char end;
    unsigned state = 0;

    while (1) {
	unsigned instr;
	if ((unsigned)ptr > (unsigned)&end) {
	    erts_printf("Scan reached end of text segment\n");
	    return -1;
	}
	if ((unsigned)ptr & 0x3) {
	    short unsigned *hptr = (short unsigned *)ptr;
	    // Epiphany is little-endian
	    instr = hptr[1] << 16 | hptr[0];
	} else {
	    instr = *ptr;
	}
	switch (state) {
	case 0:
	    if ((instr & LDR_LR_MASK) == LDR_LR_MATCH) {
		ASSERT((instr & LDR_ANY_MASK) == LDR_ANY_MATCH);
		*lr_offset = IMM11(instr) << 2;
		if (IMM11_SIGN(instr)) *lr_offset = -*lr_offset;
		HDEBUG_PRINTF("%x:\t%x\tldr lr,[sp,%d]\n",
			      (unsigned)ptr, instr, *lr_offset);
		state = 1;
	    }
	    break;
	case 1:
	    if ((instr & ADD_SP_MASK) == ADD_SP_MATCH) {
		*framesize = SIMM11(instr);
		HDEBUG_PRINTF("%x:\t%x\tldr add sp,sp,%d\n",
			      (unsigned)ptr, instr, *framesize);
		state = 2;
	    } else if ((instr & LDR_ANY_MASK) != LDR_ANY_MATCH) {
		state = 0;
		HDEBUG_PRINTF("Partial match: only link load (ptr=0x%x, instr=0x%x)\n",
			      (unsigned)ptr, instr);
	    } else {
		HDEBUG_PRINTF("%x:\t%x\tldr ??,[sp,%d]\n",
			      (unsigned)ptr, instr,
			      IMM11_SIGN(instr) ? -(IMM11(instr) << 2) : IMM11(instr) << 2);
	    }
	    break;
	case 2:
	    if ((instr & RTS_MASK) == RTS_MATCH) {
		HDEBUG_PRINTF("%x:\t%x\trts\n", (unsigned)ptr, instr);
		return 0;
	    } else if ((instr & LDR_ANY_MASK) != LDR_ANY_MATCH) {
		state = 0;
		HDEBUG_PRINTF("Partial match: no rts after sp add (ptr=0x%x, instr=0x%x)\n",
			      (unsigned)ptr, instr);
	    } else {
		HDEBUG_PRINTF("%x:\t%x\tldr ??,[sp,%d]\n",
			      (unsigned)ptr, instr,
			      IMM11_SIGN(instr) ? -(IMM11(instr) << 2) : IMM11(instr) << 2);
	    }
	    break;
	}
	if (state == 0)
	    ptr = (unsigned*)((unsigned)ptr + 2);
	else
	    ptr++;
    }
}

/*
 * Since epiphany_backtrace can't trace through functions that do not return
 * (they lack epilogue), this abort function is used instead so the compiler
 * won't know there will be no return.
 */
void returning_abort(void)
{
    abort();
}

void
epiphany_backtrace(void)
{
    void **frame, **link;
    int count = 0;
    asm("mov %0,fp" : "=r"(frame) : : );
    asm("movfs %0,pc" : "=r"(link) : : );
    erts_printf("Backtrace:");
    while (16*1024 <= (unsigned)frame && (unsigned)frame < 32*1024
	   && count++ < 100) {
	int frame_off, link_off;
	erts_printf(" 0x%x", (unsigned)link - 4);
	if (scan_epilogue((unsigned*)link, &frame_off, &link_off)) {
	    break;
	}
	link = frame[link_off >> 2];
	frame = frame[frame_off >> 2];
    }
    erts_printf(" 0x%x\n", (unsigned)link - 4);
}
