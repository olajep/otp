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

#include "slave_command.h"

#ifdef DEBUG
#  include "epiphany.h"
#endif

#if defined(DEBUG) && 0
#define HARD_DEBUG
#else
#undef HARD_DEBUG
#endif

void init_message(void)
{
}

void free_message(ErlMessage *m)
{
    struct master_command_free_message cmd = { m };
    erts_master_send_command(MASTER_COMMAND_FREE_MESSAGE, &cmd, sizeof(cmd));
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

void
slave_serve_message(Process *c_p, struct slave_command_message *cmd)
{
    ErlMessage *mp = cmd->m;
    erts_aint_t state;

    ASSERT(epiphany_in_dram(mp));
    ASSERT(!mp->data.attached || epiphany_in_dram(mp->data.attached));

    if (c_p == NULL || c_p->common.id != cmd->receiver) goto drop;

    state = erts_smp_atomic32_read_acqb(&c_p->state);
    if (state & (ERTS_PSFLG_EXITING|ERTS_PSFLG_PENDING_EXIT)) goto drop;

    LINK_MESSAGE(c_p, mp);

    /* ETODO: This */
#ifdef USE_VM_PROBES
    if (DTRACE_ENABLED(message_queued)) {
        DTRACE_CHARBUF(receiver_name, DTRACE_TERM_BUF_SIZE);
        Sint tok_label = 0;
        Sint tok_lastcnt = 0;
        Sint tok_serial = 0;

	dtrace_proc_str(c_p, receiver_name);
        if (seq_trace_token != NIL && is_tuple(seq_trace_token)) {
            tok_label = signed_val(SEQ_TRACE_T_LABEL(seq_trace_token));
            tok_lastcnt = signed_val(SEQ_TRACE_T_LASTCNT(seq_trace_token));
            tok_serial = signed_val(SEQ_TRACE_T_SERIAL(seq_trace_token));
        }
        DTRACE6(message_queued,
		receiver_name, size_object(message), c_p->msg.len,
                tok_label, tok_lastcnt, tok_serial);
    }
#endif

    if (IS_TRACED_FL(c_p, F_TRACE_RECEIVE))
	EPIPHANY_STUB_BT(); /* Tracing */

#ifndef ERTS_SMP
    ERTS_HOLE_CHECK(c_p);
#endif
    return;

    /* Drop the message */
 drop: {
	struct master_command_free_message rep = { cmd->m };
	erts_printf("Dropping message %T\n", mp->m[0]);
	erts_master_send_command(MASTER_COMMAND_FREE_MESSAGE, &rep, sizeof(rep));
	return;
    }
}
