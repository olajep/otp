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
 *
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "erl_thr_progress.h"
#include "slave_bif.h"

#define HARDDEBUG 0

void
erts_slave_serve_bif(struct slave *slave, struct slave_syscall_bif *arg)
{
    Process *p = slave->c_p;
    erts_aint32_t old_state = 0;
    BifEntry *bif = bif_table + arg->bif_no;
    Eterm (*f)(Process*, Eterm*);
    ErtsThrPrgrDelayHandle delay;
    ASSERT(p);
    ASSERT(arg->bif_no < BIF_SIZE);
    old_state = erts_smp_atomic32_read_acqb(&p->state);

#if HARDDEBUG
    switch (bif->arity) {
    case 0: erts_printf("Proxying bif %T:%T()\n", bif->module, bif->name); break;
    case 1: erts_printf("Proxying bif %T:%T(%T)\n", bif->module, bif->name, arg->args[0]);break;
    case 2: erts_printf("Proxying bif %T:%T(%T,%T)\n", bif->module, bif->name, arg->args[0], arg->args[1]);break;
    case 3: erts_printf("Proxying bif %T:%T(%T,%T,%T)\n", bif->module, bif->name, arg->args[0], arg->args[1], arg->args[2]);break;
    }
#endif

    slave_state_swapin(p, &arg->state);

    delay = erts_thr_progress_unmanaged_delay();

    f = bif->f;
    arg->result = f(p, arg->args);

    erts_thr_progress_unmanaged_continue(delay);

    if (old_state != erts_smp_atomic32_read_acqb(&p->state)) {
	erl_exit(1, "Process state changed by bif %T:%T/%d!\nWas: %#x, Now: %#x",
		 bif->module, bif->name, bif->arity, old_state,
		 erts_smp_atomic32_read_acqb(&p->state));
    }

    slave_state_swapout(p, &arg->state);

#if HARDDEBUG
    erts_printf("Replying with result %T\n", arg->result);
#endif
}

void
erts_slave_serve_gc(struct slave *slave, struct slave_syscall_gc *arg)
{
    Process *p = slave->c_p;

#if HARDDEBUG
    erts_printf("Garbage collecting %T (heap size %d, %d additional roots)\n",
		p->common.id, p->hend - p->heap, arg->nobj);
#endif

    slave_state_swapin(p, &arg->state);
    arg->ret = erts_garbage_collect(p, arg->need, arg->objv, arg->nobj);
    slave_state_swapout(p, &arg->state);

#if HARDDEBUG
    erts_printf("[gc %T] ret=%d, heap size %d\n", p->common.id, arg->ret,
		p->hend - p->heap);
#endif
}
