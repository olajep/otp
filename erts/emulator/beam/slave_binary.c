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
#include "slave_command.h"
#include "erl_binary.h"

#ifdef ERTS_SLAVE
#  include "epiphany.h"
#endif

/*
 * Binary allocations are proxied to the master, because they need to be
 * freeable by the master.
 */

#define HARDDEBUG 0

#ifdef ERTS_SLAVE
Binary *
erts_bin_nrml_alloc(Uint size)
{
    struct slave_syscall_bin *cmd =
	erts_alloc(ERTS_ALC_T_TMP, sizeof(*cmd));
    Binary *bp;
#if HARDDEBUG
    erts_printf("Allocating refc binary, size %d\n", size);
    /* epiphany_backtrace(); */
#endif
    cmd->size = size;
    cmd->bp = NULL;
    erts_master_syscall(SLAVE_SYSCALL_BIN, cmd);
    bp = cmd->bp;
    erts_free(ERTS_ALC_T_TMP, cmd);
    return bp;
}

Binary *
erts_bin_realloc(Binary *bp, Uint size)
{
    struct slave_syscall_bin *cmd =
	erts_alloc(ERTS_ALC_T_TMP, sizeof(*cmd));
#if HARDDEBUG
    erts_printf("Reallocing refc binary, new size %d\n", size);
    /* epiphany_backtrace(); */
#endif
    cmd->size = size;
    cmd->bp = bp;
    erts_master_syscall(SLAVE_SYSCALL_BIN, cmd);
    bp = cmd->bp;
    erts_free(ERTS_ALC_T_TMP, cmd);
    return bp;
}

ProcBin *
erts_normalise_proc_bin(void *vptr)
{
    ProcBin *ptr = vptr;
    ASSERT(*(Eterm *)ptr == HEADER_PROC_BIN);
    if (!epiphany_in_dram(ptr->bytes)) {
	struct slave_syscall_bin *cmd =
	    erts_alloc(ERTS_ALC_T_TMP, sizeof(*cmd));
	cmd->size = 0;
	cmd->pbp = ptr;
	erts_master_syscall(SLAVE_SYSCALL_BIN, cmd);
	ptr = cmd->pbp;
	erts_free(ERTS_ALC_T_TMP, cmd);
    }
    return ptr;
}

#else /* ERTS_SLAVE */

void
erts_slave_serve_bin(struct slave *slave, struct slave_syscall_bin *arg)
{
    if (!arg->size) {
	Binary *slave_bin;
	Binary *bin = arg->pbp->val;
	/* We can't migrate magic binaries without breaking them. */
	ASSERT(!(bin->flags & (BIN_FLAG_SLAVE|BIN_FLAG_MAGIC)));
	if (!(slave_bin = (Binary*)erts_atomic_read_nob(&bin->otherp))) {
#if HARDDEBUG
	    erts_fprintf(stderr, "Migrating binary (%d bytes, %d byte slice) "
			 "for slave %d\n", bin->orig_size, arg->pbp->size,
			 slave->no);
#endif
	    slave_bin = erts_bin_slave_alloc(bin->orig_size);
	    slave_bin->orig_size = bin->orig_size;
	    erts_refc_init(&slave_bin->refc, 1); /* bin->otherp */
	    memcpy(slave_bin->orig_bytes, bin->orig_bytes, bin->orig_size);
	    if (erts_atomic_cmpxchg_nob(&bin->otherp, (erts_aint_t)slave_bin, 0)) {
		/* Someone beat us to the punch */
		erts_bin_free(slave_bin);
		slave_bin = (Binary*)erts_atomic_read_nob(&bin->otherp);
	    }
	}

	erts_refc_inc(&slave_bin->refc, 2); /* arg->pbp */

	arg->pbp->val = slave_bin;
	arg->pbp->bytes += slave_bin->orig_bytes - bin->orig_bytes;

	erts_refc_decfree(&bin->refc, 0, ERTS_DECFREE_BIN, bin); /* arg->pbp */
    } else if (arg->bp) {
	arg->bp = erts_bin_realloc(arg->bp, arg->size);
    } else {
	arg->bp = erts_bin_slave_alloc(arg->size);
    }
}
#endif
