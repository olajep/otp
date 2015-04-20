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
