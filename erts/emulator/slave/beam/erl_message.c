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

#ifdef DEBUG
static ERTS_INLINE int in_heapfrag(const Eterm* ptr, const ErlHeapFragment *bp)
{
    return ((unsigned)(ptr - bp->mem) < bp->used_size);
}
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
void free_message_buffer(ErlHeapFragment *bp)
{
    if (!sys_in_slave_heap(bp)) {
	free_master_message_buffer(bp);
	return;
    }

    ASSERT(bp != NULL);
    do {
	ErlHeapFragment* next_bp = bp->next;

	erts_cleanup_offheap(&bp->off_heap);
	ERTS_HEAP_FREE(ERTS_ALC_T_HEAP_FRAG, (void *) bp,
		       ERTS_HEAP_FRAG_SIZE(bp->size));
	bp = next_bp;
    } while (bp != NULL);
}

void free_master_message_buffer(ErlHeapFragment *bp)
{
    struct master_command_free_hfrag cmd = { bp };
    erts_master_send_command(MASTER_COMMAND_FREE_HFRAG, &cmd, sizeof(cmd));
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
    if (bp) {
	/* Link the message buffer */
	bp->next = MBUF(proc);
	MBUF(proc) = bp;
	MBUF_SIZE(proc) += bp->used_size;
	/* ETODO: Is this needed on master too? Do we sync flags? */
	FLAGS(proc) |= F_FORCE_GC;

	/* Move any off_heap's into the process */
	if (bp->off_heap.first != NULL) {
	    struct erl_off_heap_header** next_p = &bp->off_heap.first;
	    while (*next_p != NULL) {
		next_p = &((*next_p)->next);
	    }
	    *next_p = MSO(proc).first;
	    MSO(proc).first = bp->off_heap.first;
	    bp->off_heap.first = NULL;
	    OH_OVERHEAD(&(MSO(proc)), bp->off_heap.overhead);
	}
    }
}

/*
 * Moves content of message buffer attached to a message into a heap.
 * The message buffer is deallocated.
 */
void
erts_move_msg_mbuf_to_heap(Eterm** hpp, ErlOffHeap* off_heap, ErlMessage *msg)
{
    struct erl_off_heap_header* oh;
    Eterm term, token, *fhp, *hp;
    Sint offs;
    Uint sz;
    ErlHeapFragment *bp;
#ifdef USE_VM_PROBES
    Eterm utag;
#endif

    bp = msg->data.heap_frag;
    term = ERL_MESSAGE_TERM(msg);
    token = ERL_MESSAGE_TOKEN(msg);
#ifdef USE_VM_PROBES
    utag = ERL_MESSAGE_DT_UTAG(msg);
#endif
    if (!bp) {
#ifdef USE_VM_PROBES
	ASSERT(is_immed(term) && is_immed(token) && is_immed(utag));
#else
	ASSERT(is_immed(term) && is_immed(token));
#endif
	return;
    }

    if (bp->next != NULL) {
	move_multi_frags(hpp, off_heap, bp, msg->m, 
#ifdef USE_VM_PROBES
			 3
#else
			 2
#endif
			 );
	goto copy_done;
    }

    OH_OVERHEAD(off_heap, bp->off_heap.overhead);
    sz = bp->used_size;

    ASSERT(is_immed(term) || in_heapfrag(ptr_val(term),bp));
    ASSERT(is_immed(token) || in_heapfrag(ptr_val(token),bp));

    fhp = bp->mem;
    hp = *hpp;
    offs = hp - fhp;

    oh = NULL;
    while (sz--) {
	Uint cpy_sz;
	Eterm val = *fhp++;

	switch (primary_tag(val)) {
	case TAG_PRIMARY_IMMED1:
	    *hp++ = val;
	    break;
	case TAG_PRIMARY_LIST:
	case TAG_PRIMARY_BOXED:
	    ASSERT(in_heapfrag(ptr_val(val), bp));
	    *hp++ = offset_ptr(val, offs);
	    break;
	case TAG_PRIMARY_HEADER:
	    *hp++ = val;
	    switch (val & _HEADER_SUBTAG_MASK) {
	    case ARITYVAL_SUBTAG:
		break;
	    case REFC_BINARY_SUBTAG:
	    case FUN_SUBTAG:
	    case EXTERNAL_PID_SUBTAG:
	    case EXTERNAL_PORT_SUBTAG:
	    case EXTERNAL_REF_SUBTAG:
		oh = (struct erl_off_heap_header*) (hp-1);
		cpy_sz = thing_arityval(val);
		goto cpy_words;
	    default:
		cpy_sz = header_arity(val);

	    cpy_words:
		ASSERT(sz >= cpy_sz);
		sz -= cpy_sz;
		while (cpy_sz >= 8) {
		    cpy_sz -= 8;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		    *hp++ = *fhp++;
		}
		switch (cpy_sz) {
		case 7: *hp++ = *fhp++;
		case 6: *hp++ = *fhp++;
		case 5: *hp++ = *fhp++;
		case 4: *hp++ = *fhp++;
		case 3: *hp++ = *fhp++;
		case 2: *hp++ = *fhp++;
		case 1: *hp++ = *fhp++;
		default: break;
		}
		if (oh) {
		    /* Add to offheap list */
		    oh->next = off_heap->first;
		    off_heap->first = oh;
		    ASSERT(*hpp <= (Eterm*)oh);
		    ASSERT(hp > (Eterm*)oh);
		    oh = NULL;
		}
		break;
	    }
	    break;
	}
    }

    ASSERT(bp->used_size == hp - *hpp);
    *hpp = hp;

    if (is_not_immed(token)) {
	ASSERT(in_heapfrag(ptr_val(token), bp));
	ERL_MESSAGE_TOKEN(msg) = offset_ptr(token, offs);
    }

    if (is_not_immed(term)) {
	ASSERT(in_heapfrag(ptr_val(term),bp));
	ERL_MESSAGE_TERM(msg) = offset_ptr(term, offs);
    }
#ifdef USE_VM_PROBES
    if (is_not_immed(utag)) {
	ASSERT(in_heapfrag(ptr_val(utag), bp));
	ERL_MESSAGE_DT_UTAG(msg) = offset_ptr(utag, offs);
    }
#endif

copy_done:

    bp->off_heap.first = NULL;
    free_master_message_buffer(bp);
    msg->data.heap_frag = NULL;
}

Uint erts_msg_attached_data_size_aux(ErlMessage *msg)
{
    EPIPHANY_STUB_BT();
    return 0;
}

void
erts_move_msg_attached_data_to_heap(Eterm **hpp, ErlOffHeap *ohp, ErlMessage *msg)
{
    if (is_value(ERL_MESSAGE_TERM(msg)))
	erts_move_msg_mbuf_to_heap(hpp, ohp, msg);
    else if (msg->data.dist_ext) {
	EPIPHANY_STUB_BT();
    }
    /* else: bad external detected when calculating size */
}


Eterm erts_msg_distext2heap(Process *p, ErtsProcLocks *l, ErlHeapFragment **f,
			    Eterm *t, ErtsDistExternal *d)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}

void erts_cleanup_offheap(ErlOffHeap *offheap)
{
    union erl_off_heap_ptr u;

    for (u.hdr = offheap->first; u.hdr; u.hdr = u.hdr->next) {
	switch (thing_subtag(u.hdr->thing_word)) {
	case REFC_BINARY_SUBTAG:
	case FUN_SUBTAG:
	    /* We can't decrease the reference counters yet. */
	    EPIPHANY_STUB_BT();
	default:
	    ASSERT(is_external_header(u.hdr->thing_word));
	    erts_deref_node_entry(u.ext->node);
	    break;
	}
    }
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
