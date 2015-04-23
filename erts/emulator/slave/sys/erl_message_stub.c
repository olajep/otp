/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1997-2012. All Rights Reserved.
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
 * Message passing primitives.
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_message.h"
#include "erl_process.h"
#include "erl_binary.h"
#include "dtrace-wrapper.h"

/* ERTS_SCHED_PREF_QUICK_ALLOC_IMPL(message, */
/* 				 ErlMessage, */
/* 				 ERL_MESSAGE_BUF_SZ, */
/* 				 ERTS_ALC_T_MSG_REF) */

#if defined(DEBUG) && 0
#define HARD_DEBUG
#else
#undef HARD_DEBUG
#endif

void init_message(void)
{
    EPIPHANY_STUB_BT();
}

void free_message(ErlMessage *m)
{
    EPIPHANY_STUB_BT();
}

ErlHeapFragment* new_message_buffer(Uint u)
{
    EPIPHANY_STUB_BT();
    return NULL;
}

ErlHeapFragment* erts_resize_message_buffer(ErlHeapFragment *, Uint,
					    Eterm *, Uint);
void free_message_buffer(ErlHeapFragment *f)
{
    EPIPHANY_STUB_BT();
}

void erts_queue_dist_message(Process *p, ErtsProcLocks *l, ErtsDistExternal *d, Eterm t)
{
    EPIPHANY_STUB_BT();
}

void erts_queue_message(Process *p, ErtsProcLocks *l, ErlHeapFragment *f, Eterm t, Eterm t2
#ifdef USE_VM_PROBES
		   , Eterm dt_utag
#endif
)
{
    EPIPHANY_STUB_BT();
}

void erts_deliver_exit_message(Eterm t, Process* p, ErtsProcLocks *l, Eterm t2, Eterm t3)
{
    EPIPHANY_STUB_BT();
}

Sint erts_send_message(Process *p, Process *p2, ErtsProcLocks *l, Eterm t, unsigned u)
{
    EPIPHANY_STUB_BT();
    return 0;
}

void erts_link_mbuf_to_proc(Process *proc, ErlHeapFragment *bp)
{
    EPIPHANY_STUB_BT();
}

/*
 * Moves content of message buffer attached to a message into a heap.
 * The message buffer is deallocated.
 */
void erts_move_msg_mbuf_to_heap(Eterm** hpp, ErlOffHeap* off_heap, ErlMessage *msg)
{
    EPIPHANY_STUB_BT();
}

Uint erts_msg_attached_data_size_aux(ErlMessage *msg)
{
    EPIPHANY_STUB_BT();
    return 0;
}

void erts_move_msg_attached_data_to_heap(Eterm **t, ErlOffHeap *o, ErlMessage *m)
{
    EPIPHANY_STUB_BT();
}


Eterm erts_msg_distext2heap(Process *p, ErtsProcLocks *l, ErlHeapFragment **f,
			    Eterm *t, ErtsDistExternal *d)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

void erts_cleanup_offheap(ErlOffHeap *offheap)
{
    EPIPHANY_STUB_BT();
}
