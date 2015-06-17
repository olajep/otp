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
    unsigned row, col, origin_row, origin_col, rows, cols;
    epiphany_workgroup_origin(&origin_row, &origin_col);
    epiphany_workgroup_dimens(&rows, &cols);
    e_coords_from_coreid(id, &row, &col);
    return (row - origin_row) * cols + (col - origin_col);
}

void
epiphany_workgroup_dimens(unsigned *rows, unsigned *cols)
{
    if (!epiphany_in_emulator()) {
	*rows = e_group_config.group_rows;
	*cols = e_group_config.group_cols;
    } else {
	// The emulator does not set e_group_config. Assume a P16 4x4 workgroup.
	*rows = 4;
	*cols = 4;
    }
}

int
epiphany_workgroup_size(void)
{
    unsigned rows, cols;
    epiphany_workgroup_dimens(&rows, &cols);
    return rows * cols;
}


void
epiphany_workgroup_origin(unsigned *row, unsigned *col)
{
    if (!epiphany_in_emulator()) {
	*row = 0;
	*col = 0;
    } else {
	// The emulator does not set e_group_config. Assume a P16 4x4 workgroup.
#ifdef DEBUG
	e_coreid_t id = e_get_coreid();
	unsigned myrow, mycol;
	e_coords_from_coreid(id, &myrow, &mycol);
	ASSERT(32 <= myrow && myrow < 36);
	ASSERT(8 <= mycol && mycol < 12);
#endif
	*row = 32;
	*col = 8;
    }
}

int
epiphany_in_dram(void *addr)
{
    return 0x8e000000 <= (unsigned)addr && (unsigned)addr < 0x90000000;
}

#ifdef DEBUG
int
epiphany_sane_address(void *addrp)
{
    e_coreid_t owning;
    unsigned row, col, min_row, min_col, max_row, max_col, rows, cols;
    unsigned addr = (unsigned)addrp;

    // Shared DRAM
    if (epiphany_in_dram(addrp)) return 1;

    // In memory space of core in workgroup
    owning = addr >> (32 - 12);
    e_coords_from_coreid(owning, &row, &col);
    epiphany_workgroup_origin(&min_row, &min_col);
    epiphany_workgroup_dimens(&rows, &cols);
    max_row = min_row + rows;
    max_col = min_col + cols;
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
#endif

// Word or Doubleword
#define LDR_ANY_MASK   0b00011110000000000001110001011111
#define LDR_LR_MASK    0b11111110000000001111110001011111
#define LDRD_R10_MASK  0b11111110000000001111110001111111
#define LDR_ANY_MATCH  0b00000100000000000001010001001100
#define LDR_LR_MATCH   0b00100100000000001101010001001100
#define LDRD_R10_MATCH 0b00100100000000000101010001101100

#define ADD_SP_MASK  0b11111100000000001111110001111111
#define ADD_SP_MATCH 0b00100100000000001011010000011011

#define RTS_MASK  0b00011100000011110001111111111111
#define RTS_MATCH 0b00000100000000100001100101001111


#define IMM11(INSTR) ((((INSTR) >> 13) & (0xff << 3)) | (((INSTR) >> 7) & 0x7))
#define IMM11_SIGN(INSTR) ((INSTR) & (1 << 24))

#define MEM_SHIFT(INSTR) (((INSTR) >> 5) & 0b11)

// Sign-extend IMM11
#define SIMM11(INSTR) ((((signed)IMM11(INSTR)) << (32-11)) >> (32-11))

#define LDREG(INSTR) (((INSTR >> 13) & 0b111) | ((INSTR >> 26) & 0b111000))

#define HARDDEBUG 0

#if HARDDEBUG
#  define HDEBUG_PRINTF(X ...) erts_printf(X)
#  define IFMT(I,A) ("%#08x: %08x    " I " " A "\n")
#  define IFMTARGS  (unsigned)ptr, instr
static const char * const load_instrs[] = { "ldrb", "ldrh", "ldr", "ldrd" };
#  define HDEBUG_PRINT_GEN_LOAD					\
    HDEBUG_PRINTF(IFMT("%s", "r%d,[sp,%d]"),			\
		  IFMTARGS, load_instrs[MEM_SHIFT(instr)],	\
		  LDREG(instr),					\
		  IMM11_SIGN(instr)					\
		  ? -(IMM11(instr) << MEM_SHIFT(instr))			\
		  : IMM11(instr) << MEM_SHIFT(instr))
#else
#  define HDEBUG_PRINTF(X ...)
#  define HDEBUG_PRINT_GEN_LOAD
#endif

/*
 * Scans for the instruction sequence
 *  ldrd r10,[sp, ?] (optionally)
 *   Any number of ldr ?, [sp, ?]
 *  ldr lr,[sp,(*lr_offset)>>2]
 *   Any number of ldr ?, [sp, ?]
 *  add sp,sp,*framesize
 *   Any number of ldr ?, [sp, ?]
 *  rts
 * starting at address ptr. Returns zero if found and nonzero if not.
 */
static int
scan_epilogue(unsigned *__attribute__((packed)) ptr, int *framesize, int *lr_offset)
{
    extern char end;
    unsigned state = 0;
    unsigned frame_off = 0;

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
	case 1:
	    if ((instr & LDR_LR_MASK) == LDR_LR_MATCH) {
		ASSERT((instr & LDR_ANY_MASK) == LDR_ANY_MATCH);
		*lr_offset = IMM11(instr) << MEM_SHIFT(instr);
		if (IMM11_SIGN(instr)) *lr_offset = -*lr_offset;
		HDEBUG_PRINTF(IFMT("%s", "lr,[sp,%d]"), IFMTARGS,
			      load_instrs[MEM_SHIFT(instr)], *lr_offset);
		state = 2;
	    } else if ((instr & LDRD_R10_MASK) == LDRD_R10_MATCH) {
		HDEBUG_PRINT_GEN_LOAD;
		frame_off = 4;
		state = 1;
	    }
	    break;
	case 2:
	    if ((instr & ADD_SP_MASK) == ADD_SP_MATCH) {
		*framesize = SIMM11(instr) + frame_off;
		HDEBUG_PRINTF(IFMT("add", "sp,sp,%d"), IFMTARGS, SIMM11(instr));
		state = 3;
	    } else if ((instr & LDR_ANY_MASK) != LDR_ANY_MATCH) {
		state = 0;
		HDEBUG_PRINTF("Partial match: only link load "
			      "(ptr=0x%x, instr=0x%x)\n", (unsigned)ptr, instr);
	    } else {
		HDEBUG_PRINT_GEN_LOAD;
	    }
	    break;
	case 3:
	    if ((instr & RTS_MASK) == RTS_MATCH) {
		HDEBUG_PRINTF(IFMT("rts", ""), IFMTARGS);
		return 0;
	    } else if ((instr & LDR_ANY_MASK) != LDR_ANY_MATCH) {
		state = 0;
		HDEBUG_PRINTF("Partial match: no rts after sp add "
			      "(ptr=0x%x, instr=0x%x)\n", (unsigned)ptr, instr);
	    } else {
		HDEBUG_PRINT_GEN_LOAD;
	    }
	    break;
	}
	if (state == 0) {
	    /* Reset state that relies on a default value */
	    frame_off = 0;

	    ptr = (unsigned*)((unsigned)ptr + 2);
	} else
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

#define BT_MAX_FRAMES 32
void
epiphany_backtrace(void)
{
    void **frame, **link;
#if !HARDDEBUG
    char links[9 * BT_MAX_FRAMES + 1];
    int links_pos = 0;
#endif
    int count = 0;
    asm("mov %0,fp" : "=r"(frame) : : );
    asm("movfs %0,pc" : "=r"(link) : : );
#if HARDDEBUG
    erts_printf("Backtrace:");
#endif
    while (16*1024 <= (unsigned)frame && (unsigned)frame < 32*1024
	   && count++ < BT_MAX_FRAMES) {
	int frame_off = 0, link_off = 0;
#if HARDDEBUG
	erts_printf("[%#x] frame=%#x\n", (unsigned)link - 4, (unsigned)frame);
#else
	links_pos += snprintf(links + links_pos, sizeof(links) - links_pos,
			      " %#x", (unsigned)link - 4);
#endif
	if (scan_epilogue((unsigned*)link, &frame_off, &link_off)) {
	    break;
	}
	link = frame[link_off >> 2];
	frame = frame[frame_off >> 2];
    }
#if HARDDEBUG
    erts_printf("[%#x] frame=%#x\n", (unsigned)link - 4, (unsigned)frame);
#else
    erts_printf("Backtrace:%s %#x\n", links, (unsigned)link - 4);
#endif
}
