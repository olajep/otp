/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2014. All Rights Reserved.
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
#include "erl_vm.h"
#include "global.h"
#include "bif.h"

#include "slave_command.h"

#include "epiphany.h"

/*
 * Garbage collect a process.
 *
 * p: Pointer to the process structure.
 * need: Number of Eterm words needed on the heap.
 * objv: Array of terms to add to rootset; that is to preserve.
 * nobj: Number of objects in objv.
 */
int
erts_garbage_collect(Process* p, int need, Eterm* objv, int nobj)
{
    int ret;
    struct slave_syscall_gc *cmd = erts_alloc(ERTS_ALC_T_TMP, sizeof(*cmd));
    Eterm *dram_objv;
    int copy_objv = nobj != 0 && !epiphany_in_dram(objv);

    if (copy_objv) {
	dram_objv = erts_alloc(ERTS_ALC_T_TMP, nobj*sizeof(Eterm));
	memcpy(dram_objv, objv, nobj*sizeof(Eterm));
    } else {
	dram_objv = objv;
    }

    cmd->need = need;
    cmd->objv = dram_objv;
    cmd->nobj = nobj;
    slave_state_swapout(p, &cmd->state);

    erts_master_syscall(SLAVE_SYSCALL_GC, cmd);

    if (copy_objv) {
	memcpy(objv, dram_objv, nobj*sizeof(Eterm));
	erts_free(ERTS_ALC_T_TMP, dram_objv);
    }

    slave_state_swapin(p, &cmd->state);
    ret = cmd->ret;
    erts_free(ERTS_ALC_T_TMP, cmd);
    return ret;
}

Eterm
erts_gc_after_bif_call(Process* p, Eterm result, Eterm* regs, Uint arity)
{
    EPIPHANY_STUB_FUN();
}
