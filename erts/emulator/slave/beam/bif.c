/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2014. All Rights Reserved.
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

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "erl_sys_driver.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "dist.h"
#include "erl_version.h"
#include "erl_binary.h"
#include "beam_bp.h"
#include "erl_db_util.h"
#include "register.h"
#include "erl_thr_progress.h"
#define ERTS_PTAB_WANT_BIF_IMPL__
#include "erl_ptab.h"
#include "erl_bits.h"

static Export* await_proc_exit_trap = NULL;
Export* erts_format_cpu_topology_trap = NULL;

#define DECL_AM(S) Eterm AM_ ## S = am_atom_put(#S, sizeof(#S) - 1)

/*
 * The BIF's now follow, see the Erlang Manual for a description of what
 * each individual BIF does.
 */

BIF_RETTYPE spawn_3(BIF_ALIST_3)
{
    EPIPHANY_STUB_FUN();
}

/**********************************************************************/

/* create a link to the process */
BIF_RETTYPE link_1(BIF_ALIST_1)
{
    EPIPHANY_STUB_FUN();
}

BIF_RETTYPE demonitor_1(BIF_ALIST_1)
{
    EPIPHANY_STUB_FUN();
}

BIF_RETTYPE demonitor_2(BIF_ALIST_2)
{
    EPIPHANY_STUB_FUN();
}

/* Type must be atomic object! */
void
erts_queue_monitor_message(Process *p,
			   ErtsProcLocks *p_locksp,
			   Eterm ref,
			   Eterm type,
			   Eterm item,
			   Eterm reason)
{
    Eterm tup;
    Eterm* hp;
    Eterm reason_copy, ref_copy, item_copy;
    Uint reason_size, ref_size, item_size, heap_size;
    ErlOffHeap *ohp;
    ErlHeapFragment *bp;

    reason_size = IS_CONST(reason) ? 0 : size_object(reason);
    item_size   = IS_CONST(item) ? 0 : size_object(item);
    ref_size    = size_object(ref);

    heap_size = 6+reason_size+ref_size+item_size;

    hp = erts_alloc_message_heap(heap_size,
				 &bp,
				 &ohp,
				 p,
				 p_locksp);

    reason_copy = (IS_CONST(reason)
		   ? reason
		   : copy_struct(reason, reason_size, &hp, ohp));
    item_copy   = (IS_CONST(item)
		   ? item
		   : copy_struct(item, item_size, &hp, ohp));
    ref_copy    = copy_struct(ref, ref_size, &hp, ohp);

    tup = TUPLE5(hp, am_DOWN, ref_copy, type, item_copy, reason_copy);
    erts_queue_message(p, p_locksp, bp, tup, NIL
#ifdef USE_VM_PROBES
		       , NIL
#endif
		       );
}

/**********************************************************************/
/* this is a combination of the spawn and link BIFs */

BIF_RETTYPE spawn_link_3(BIF_ALIST_3)
{
    EPIPHANY_STUB_FUN();
}

/**********************************************************************/

BIF_RETTYPE spawn_opt_1(BIF_ALIST_1)
{
    EPIPHANY_STUB_FUN();
}

  
/**********************************************************************/
/* remove a link from a process */
BIF_RETTYPE unlink_1(BIF_ALIST_1)
{
    EPIPHANY_STUB_FUN();
}

BIF_RETTYPE hibernate_3(BIF_ALIST_3)
{
    /*
     * hibernate/3 is usually translated to an instruction; therefore
     * this function is only called from HiPE or when the call could not
     * be translated.
     */
    Eterm reg[3];

    if (erts_hibernate(BIF_P, BIF_ARG_1, BIF_ARG_2, BIF_ARG_3, reg)) {
        /*
         * If hibernate succeeded, TRAP. The process will be wait in a
         * hibernated state if its state is inactive (!ERTS_PSFLG_ACTIVE);
         * otherwise, continue executing (if any message was in the queue).
         */
        BIF_TRAP_CODE_PTR_(BIF_P, BIF_P->i);
    }
    return THE_NON_VALUE;
}

/**********************************************************************/

BIF_RETTYPE get_stacktrace_0(BIF_ALIST_0)
{
    Eterm t = build_stacktrace(BIF_P, BIF_P->ftrace);
    BIF_RET(t);
}

/**********************************************************************/
/*
 * This is like exit/1, except that errors are logged if they terminate
 * the process, and the final error value will be {Term,StackTrace}.
 */

BIF_RETTYPE error_1(BIF_ALIST_1)
{
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_ERROR);
}

/**********************************************************************/
/*
 * This is like error/1, except that the given 'args' will be included
 * in the stacktrace.
 */

BIF_RETTYPE error_2(BIF_ALIST_2)
{
    Eterm* hp = HAlloc(BIF_P, 3);

    BIF_P->fvalue = TUPLE2(hp, BIF_ARG_1, BIF_ARG_2);
    BIF_ERROR(BIF_P, EXC_ERROR_2);
}

/**********************************************************************/
/*
 * This is like exactly like error/1. The only difference is
 * that Dialyzer thinks that it it will return an arbitrary term.
 * It is useful in stub functions for NIFs.
 */

BIF_RETTYPE nif_error_1(BIF_ALIST_1)
{
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_ERROR);
}

/**********************************************************************/
/*
 * This is like exactly like error/2. The only difference is
 * that Dialyzer thinks that it it will return an arbitrary term.
 * It is useful in stub functions for NIFs.
 */

BIF_RETTYPE nif_error_2(BIF_ALIST_2)
{
    Eterm* hp = HAlloc(BIF_P, 3);

    BIF_P->fvalue = TUPLE2(hp, BIF_ARG_1, BIF_ARG_2);
    BIF_ERROR(BIF_P, EXC_ERROR_2);
}

/**********************************************************************/
/* this is like throw/1 except that we set freason to EXC_EXIT */

BIF_RETTYPE exit_1(BIF_ALIST_1)
{
    BIF_P->fvalue = BIF_ARG_1;  /* exit value */
    BIF_ERROR(BIF_P, EXC_EXIT);
}


/**********************************************************************/
/* raise an exception of given class, value and stacktrace.
 *
 * If there is an error in the argument format, 
 * return the atom 'badarg' instead.
 */
BIF_RETTYPE raise_3(BIF_ALIST_3)
{
    Process *c_p = BIF_P;
    Eterm class = BIF_ARG_1;
    Eterm value = BIF_ARG_2;
    Eterm stacktrace = BIF_ARG_3;
    Eterm reason;
    Eterm l, *hp, *hp_end, *tp;
    int depth, cnt;
    size_t sz;
    int must_copy = 0;
    struct StackTrace *s;

    if (class == am_error) {
	c_p->fvalue = value;
	reason = EXC_ERROR;
    } else if (class == am_exit) {
	c_p->fvalue = value;
	reason = EXC_EXIT;
    } else if (class == am_throw) {
	c_p->fvalue = value;
	reason = EXC_THROWN;
    } else goto error;
    reason &= ~EXF_SAVETRACE;
    
    /* Check syntax of stacktrace, and count depth.
     * Accept anything that can be returned from erlang:get_stacktrace/0,
     * as well as a 2-tuple with a fun as first element that the
     * error_handler may need to give us. Also allow old-style
     * MFA three-tuples.
     */
    for (l = stacktrace, depth = 0;  
	 is_list(l);  
	 l = CDR(list_val(l)), depth++) {
	Eterm t = CAR(list_val(l));
	Eterm location = NIL;

	if (is_not_tuple(t)) goto error;
	tp = tuple_val(t);
	switch (arityval(tp[0])) {
	case 2:
	    /* {Fun,Args} */
	    if (is_fun(tp[1])) {
		must_copy = 1;
	    } else {
		goto error;
	    }
	    break;
	case 3:
	    /*
	     * One of:
	     * {Fun,Args,Location}
	     * {M,F,A}
	     */
	    if (is_fun(tp[1])) {
		location = tp[3];
	    } else if (is_atom(tp[1]) && is_atom(tp[2])) {
		must_copy = 1;
	    } else {
		goto error;
	    }
	    break;
	case 4:
	    if (!(is_atom(tp[1]) && is_atom(tp[2]))) {
		goto error;
	    }
	    location = tp[4];
	    break;
	default:
	    goto error;
	}
	if (is_not_list(location) && is_not_nil(location)) {
	    goto error;
	}
    }
    if (is_not_nil(l)) goto error;
    
    /* Create stacktrace and store */
    if (erts_backtrace_depth < depth) {
	depth = erts_backtrace_depth;
	must_copy = 1;
    }
    if (must_copy) {
	cnt = depth;
	c_p->ftrace = NIL;
    } else {
	/* No need to copy the stacktrace */
	cnt = 0;
	c_p->ftrace = stacktrace;
    }

    tp = &c_p->ftrace;
    sz = (offsetof(struct StackTrace, trace) + sizeof(Eterm) - 1) 
	/ sizeof(Eterm);
    hp = HAlloc(c_p, sz + (2+6)*(cnt + 1));
    hp_end = hp + sz + (2+6)*(cnt + 1);
    s = (struct StackTrace *) hp;
    s->header = make_neg_bignum_header(sz - 1);
    s->freason = reason;
    s->pc = NULL;
    s->current = NULL;
    s->depth = 0;
    hp += sz;
    if (must_copy) {
	int cnt;

	/* Copy list up to depth */
	for (cnt = 0, l = stacktrace;
	     cnt < depth;
	     cnt++, l = CDR(list_val(l))) {
	    Eterm t;
	    Eterm *tpp;
	    int arity;

	    ASSERT(*tp == NIL);
	    t = CAR(list_val(l));
	    tpp = tuple_val(t);
	    arity = arityval(tpp[0]);
	    if (arity == 2) {
		t = TUPLE3(hp, tpp[1], tpp[2], NIL);
		hp += 4;
	    } else if (arity == 3 && is_atom(tpp[1])) {
		t = TUPLE4(hp, tpp[1], tpp[2], tpp[3], NIL);
		hp += 5;
	    }
	    *tp = CONS(hp, t, *tp);
	    tp = &CDR(list_val(*tp));
	    hp += 2;
	}
    }
    c_p->ftrace = CONS(hp, c_p->ftrace, make_big((Eterm *) s));
    hp += 2;
    ASSERT(hp <= hp_end);
    HRelease(c_p, hp_end, hp);
    BIF_ERROR(c_p, reason);
    
 error:
    return am_badarg;
}

/**********************************************************************/
/* send an exit message to another process (if trapping exits) or
   exit the other process */

BIF_RETTYPE exit_2(BIF_ALIST_2)
{
    EPIPHANY_STUB_FUN();
}

BIF_RETTYPE process_flag_2(BIF_ALIST_2)
{
    EPIPHANY_STUB_FUN();
}

BIF_RETTYPE process_flag_3(BIF_ALIST_3)
{
    // We need locks on the process
    EPIPHANY_STUB_FUN();
}

/**********************************************************************/

/*
 * erlang:'!'/2
 */

BIF_RETTYPE
ebif_bang_2(BIF_ALIST_2)
{
    return erl_send(BIF_P, BIF_ARG_1, BIF_ARG_2);
}


/*
 * Send a message to Process, Port or Registered Process.
 * Returns non-negative reduction bump or negative result code.
 */
#define SEND_TRAP		(-1)
#define SEND_YIELD		(-2)
#define SEND_YIELD_RETURN	(-3)
#define SEND_BADARG		(-4)
#define SEND_USER_ERROR		(-5)
#define SEND_INTERNAL_ERROR	(-6)
#define SEND_AWAIT_RESULT	(-7)

Sint do_send(Process *p, Eterm to, Eterm msg, int suspend, Eterm *refp);

Sint
do_send(Process *p, Eterm to, Eterm msg, int suspend, Eterm *refp) {
    EPIPHANY_STUB_FUN();
}


BIF_RETTYPE send_3(BIF_ALIST_3)
{
    EPIPHANY_STUB_FUN();
}

Eterm erl_send(Process *p, Eterm to, Eterm msg)
{
    Eterm args[] = {to, msg};
    return send_2(p, args);
}

/**********************************************************************/
/*
 * apply/3 is implemented as an instruction and as erlang code in the
 * erlang module.
 *
 * There is only one reason that apply/3 is included in the BIF table:
 * The error handling code in the beam emulator passes the pointer to
 * this function to the error handling code if the apply instruction
 * fails.  The error handling use the function pointer to lookup
 * erlang:apply/3 in the BIF table.
 *
 * This function will never be called.  (It could be if init did something
 * like this:  apply(erlang, apply, [M, F, A]). Not recommended.)
 */

BIF_RETTYPE apply_3(BIF_ALIST_3)
{
    BIF_ERROR(BIF_P, BADARG);
}


/**********************************************************************/

/* integer to float */

/**********************************************************************/

/* returns the head of a list - this function is unecessary
   and is only here to keep Robert happy (Even more, since it's OP as well) */
BIF_RETTYPE hd_1(BIF_ALIST_1)
{
     if (is_not_list(BIF_ARG_1)) {
	 BIF_ERROR(BIF_P, BADARG);
     }
     BIF_RET(CAR(list_val(BIF_ARG_1)));
}

/**********************************************************************/

/* returns the tails of a list - same comment as above */

BIF_RETTYPE tl_1(BIF_ALIST_1)
{
    if (is_not_list(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    BIF_RET(CDR(list_val(BIF_ARG_1)));
}


/**********************************************************************/
/* return the size of an I/O list */

static Eterm
accumulate(Eterm acc, Uint size)
{
    if (is_non_value(acc)) {
	/*
	 * There is no pre-existing accumulator. Allocate a
	 * bignum buffer with one extra word to be used if
	 * the bignum grows in the future.
	 */
	Eterm* hp = (Eterm *) erts_alloc(ERTS_ALC_T_TEMP_TERM,
					 (BIG_UINT_HEAP_SIZE+1) *
					 sizeof(Eterm));
	return uint_to_big(size, hp);
    } else {
	Eterm* big;
	int need_heap;

	/*
	 * Add 'size' to 'acc' in place. There is always one
	 * extra word allocated in case the bignum grows by one word.
	 */
	big = big_val(acc);
	need_heap = BIG_NEED_SIZE(BIG_SIZE(big));
	acc = big_plus_small(acc, size, big);
	if (BIG_NEED_SIZE(big_size(acc)) > need_heap) {
	    /*
	     * The extra word has been consumed. Grow the
	     * allocation by one word.
	     */
	    big = (Eterm *) erts_realloc(ERTS_ALC_T_TEMP_TERM,
					 big_val(acc),
					 (need_heap+1) * sizeof(Eterm));
	    acc = make_big(big);
	}
	return acc;
    }
}

static Eterm
consolidate(Process* p, Eterm acc, Uint size)
{
    Eterm* hp;

    if (is_non_value(acc)) {
	return erts_make_integer(size, p);
    } else {
	Eterm* big;
	Uint sz;
	Eterm res;
	
	acc = accumulate(acc, size);
	big = big_val(acc);
	sz = BIG_NEED_SIZE(BIG_SIZE(big));
	hp = HAlloc(p, sz);
	res = make_big(hp);
	while (sz--) {
	    *hp++ = *big++;
	}
	erts_free(ERTS_ALC_T_TEMP_TERM, (void *) big_val(acc));
	return res;
    }
}

BIF_RETTYPE iolist_size_1(BIF_ALIST_1)
{
    Eterm obj, hd;
    Eterm* objp;
    Uint size = 0;
    Uint cur_size;
    Uint new_size;
    Eterm acc = THE_NON_VALUE;
    DECLARE_ESTACK(s);

    obj = BIF_ARG_1;
    goto L_again;

    while (!ESTACK_ISEMPTY(s)) {
	obj = ESTACK_POP(s);
    L_again:
	if (is_list(obj)) {
	L_iter_list:
	    objp = list_val(obj);
	    hd = CAR(objp);
	    obj = CDR(objp);
	    /* Head */
	    if (is_byte(hd)) {
		size++;
		if (size == 0) {
		    acc = accumulate(acc, (Uint) -1);
		    size = 1;
		}
	    } else if (is_binary(hd) && binary_bitsize(hd) == 0) {
		cur_size = binary_size(hd);
		if ((new_size = size + cur_size) >= size) {
		    size = new_size;
		} else {
		    acc = accumulate(acc, size);
		    size = cur_size;
		}
	    } else if (is_list(hd)) {
		ESTACK_PUSH(s, obj);
		obj = hd;
		goto L_iter_list;
	    } else if (is_not_nil(hd)) {
		goto L_type_error;
	    }
	    /* Tail */
	    if (is_list(obj)) {
		goto L_iter_list;
	    } else if (is_binary(obj) && binary_bitsize(obj) == 0) {
		cur_size = binary_size(obj);
		if ((new_size = size + cur_size) >= size) {
		    size = new_size;
		} else {
		    acc = accumulate(acc, size);
		    size = cur_size;
		}
	    } else if (is_not_nil(obj)) {
		goto L_type_error;
	    }
	} else if (is_binary(obj) && binary_bitsize(obj) == 0) {
	    cur_size = binary_size(obj);
	    if ((new_size = size + cur_size) >= size) {
		size = new_size;
	    } else {
		acc = accumulate(acc, size);
		size = cur_size;
	    }
	} else if (is_not_nil(obj)) {
	    goto L_type_error;
	}
    }

    DESTROY_ESTACK(s);
    BIF_RET(consolidate(BIF_P, acc, size));

 L_type_error:
    DESTROY_ESTACK(s);
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* return the N'th element of a tuple */

BIF_RETTYPE element_2(BIF_ALIST_2)
{
    if (is_not_small(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (is_tuple(BIF_ARG_2)) {
	Eterm* tuple_ptr = tuple_val(BIF_ARG_2);
	Sint ix = signed_val(BIF_ARG_1);

	if ((ix >= 1) && (ix <= arityval(*tuple_ptr)))
	    BIF_RET(tuple_ptr[ix]);
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* return the arity of a tuple */

BIF_RETTYPE tuple_size_1(BIF_ALIST_1)
{
    if (is_tuple(BIF_ARG_1)) {
	return make_small(arityval(*tuple_val(BIF_ARG_1)));
    }
    BIF_ERROR(BIF_P, BADARG);
}

/**********************************************************************/

/* set the n'th element in a tuple */

BIF_RETTYPE setelement_3(BIF_ALIST_3)
{
    Eterm* ptr;
    Eterm* hp;
    Eterm* resp;
    Uint ix;
    Uint size;

    if (is_not_small(BIF_ARG_1) || is_not_tuple(BIF_ARG_2)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    ptr = tuple_val(BIF_ARG_2);
    ix = signed_val(BIF_ARG_1);
    size = arityval(*ptr) + 1;   /* include arity */
    if ((ix < 1) || (ix >= size)) {
	goto error;
    }

    hp = HAlloc(BIF_P, size);

    /* copy the tuple */
    resp = hp;
    sys_memcpy(hp, ptr, sizeof(Eterm)*size);
    resp[ix] = BIF_ARG_3;
    BIF_RET(make_tuple(resp));
}

/**********************************************************************/

BIF_RETTYPE make_tuple_2(BIF_ALIST_2)
{
    Sint n;
    Eterm* hp;
    Eterm res;

    if (is_not_small(BIF_ARG_1) || (n = signed_val(BIF_ARG_1)) < 0 || n > ERTS_MAX_TUPLE_SIZE) {
	BIF_ERROR(BIF_P, BADARG);
    }
    hp = HAlloc(BIF_P, n+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(n);
    while (n--) {
	*hp++ = BIF_ARG_2;
    }
    BIF_RET(res);
}

BIF_RETTYPE make_tuple_3(BIF_ALIST_3)
{
    Sint n;
    Uint limit;
    Eterm* hp;
    Eterm res;
    Eterm list = BIF_ARG_3;
    Eterm* tup;

    if (is_not_small(BIF_ARG_1) || (n = signed_val(BIF_ARG_1)) < 0 || n > ERTS_MAX_TUPLE_SIZE) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    limit = (Uint) n;
    hp = HAlloc(BIF_P, n+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(n);
    tup = hp;
    while (n--) {
	*hp++ = BIF_ARG_2;
    }
    while(is_list(list)) {
	Eterm* cons;
	Eterm hd;
	Eterm* tp;
	Eterm index;
	Uint index_val;

	cons = list_val(list);
	hd = CAR(cons);
	list = CDR(cons);
	if (is_not_tuple_arity(hd, 2)) {
	    goto error;
	}
	tp = tuple_val(hd);
	if (is_not_small(index = tp[1])) {
	    goto error;
	}
	if ((index_val = unsigned_val(index) - 1) < limit) {
	    tup[index_val] = tp[2];
	} else {
	    goto error;
	}
    }
    if (is_not_nil(list)) {
	goto error;
    }
    BIF_RET(res);
}


/**********************************************************************/

BIF_RETTYPE append_element_2(BIF_ALIST_2)
{
    Eterm* ptr;
    Eterm* hp;
    Uint arity;
    Eterm res;

    if (is_not_tuple(BIF_ARG_1)) {
    error:
	BIF_ERROR(BIF_P, BADARG);
    }
    ptr   = tuple_val(BIF_ARG_1);
    arity = arityval(*ptr);

    if (arity + 1 > ERTS_MAX_TUPLE_SIZE)
	goto error;

    hp  = HAlloc(BIF_P, arity + 2);
    res = make_tuple(hp);
    *hp = make_arityval(arity+1);
    while (arity--) {
	*++hp = *++ptr;
    }
    *++hp = BIF_ARG_2;
    BIF_RET(res);
}

BIF_RETTYPE insert_element_3(BIF_ALIST_3)
{
    Eterm* ptr;
    Eterm* hp;
    Uint arity;
    Eterm res;
    Sint ix, c1, c2;

    if (is_not_tuple(BIF_ARG_2) || is_not_small(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    ptr   = tuple_val(BIF_ARG_2);
    arity = arityval(*ptr);
    ix    = signed_val(BIF_ARG_1);

    if ((ix < 1) || (ix > (arity + 1))) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp  = HAlloc(BIF_P, arity + 1 + 1);
    res = make_tuple(hp);
    *hp = make_arityval(arity + 1);

    c1 = ix - 1;
    c2 = arity - ix + 1;

    while (c1--) { *++hp = *++ptr; }
    *++hp = BIF_ARG_3;
    while (c2--) { *++hp = *++ptr; }

    BIF_RET(res);
}

BIF_RETTYPE delete_element_2(BIF_ALIST_3)
{
    Eterm* ptr;
    Eterm* hp;
    Uint arity;
    Eterm res;
    Sint ix, c1, c2;

    if (is_not_tuple(BIF_ARG_2) || is_not_small(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    ptr   = tuple_val(BIF_ARG_2);
    arity = arityval(*ptr);
    ix    = signed_val(BIF_ARG_1);

    if ((ix < 1) || (ix > arity) || (arity == 0)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp  = HAlloc(BIF_P, arity + 1 - 1);
    res = make_tuple(hp);
    *hp = make_arityval(arity - 1);

    c1  = ix - 1;
    c2  = arity - ix;

    while (c1--) { *++hp = *++ptr; }
    ++ptr;
    while (c2--) { *++hp = *++ptr; }

    BIF_RET(res);
}

/**********************************************************************/

/* convert an integer to a list of ascii integers */

BIF_RETTYPE integer_to_list_1(BIF_ALIST_1)
{
    Eterm* hp;
    Uint need;

    if (is_not_integer(BIF_ARG_1)) {
	BIF_ERROR(BIF_P, BADARG);
    }

    if (is_small(BIF_ARG_1)) {
	char *c;
	int n;
	struct Sint_buf ibuf;

	c = Sint_to_buf(signed_val(BIF_ARG_1), &ibuf);
	n = sys_strlen(c);
	need = 2*n;
	hp = HAlloc(BIF_P, need);
	BIF_RET(buf_to_intlist(&hp, c, n, NIL));
    }
    else {
	int n = big_decimal_estimate(BIF_ARG_1);
	Eterm res;
        Eterm* hp_end;

	need = 2*n;
	hp = HAlloc(BIF_P, need);
        hp_end = hp + need;
	res = erts_big_to_list(BIF_ARG_1, &hp);
        HRelease(BIF_P,hp_end,hp);
	BIF_RET(res);
    }
}

/**********************************************************************/

/* convert a list of ascii ascii integer value to an integer */


#define LTI_BAD_STRUCTURE 0
#define LTI_NO_INTEGER 1
#define LTI_SOME_INTEGER 2
#define LTI_ALL_INTEGER 3

static int do_list_to_integer(Process *p, Eterm orig_list, 
			      Eterm *integer, Eterm *rest)
{
     Sint i = 0;
     Uint ui = 0;
     int skip = 0;
     int neg = 0;
     int n = 0;
     int m;
     int lg2;
     Eterm res;
     Eterm* hp;
     Eterm *hp_end;
     Eterm lst = orig_list;
     Eterm tail = lst;
     int error_res = LTI_BAD_STRUCTURE;

     if (is_nil(lst)) {
       error_res = LTI_NO_INTEGER;
     error:
	 *rest = tail;
	 *integer = make_small(0);
	 return error_res;
     }       
     if (is_not_list(lst))
       goto error;

     /* if first char is a '-' then it is a negative integer */
     if (CAR(list_val(lst)) == make_small('-')) {
	  neg = 1;
	  skip = 1;
	  lst = CDR(list_val(lst));
	  if (is_not_list(lst)) {
	      tail = lst;
	      error_res = LTI_NO_INTEGER;
	      goto error;
	  }
     } else if (CAR(list_val(lst)) == make_small('+')) {
	 /* ignore plus */
	 skip = 1;
	 lst = CDR(list_val(lst));
	 if (is_not_list(lst)) {
	     tail = lst;
	     error_res = LTI_NO_INTEGER;
	     goto error;
	 }
     }

     /* Calculate size and do type check */

     while(1) {
	 if (is_not_small(CAR(list_val(lst)))) {
	     break;
	 }
	 if (unsigned_val(CAR(list_val(lst))) < '0' ||
	     unsigned_val(CAR(list_val(lst))) > '9') {
	     break;
	 }
	 ui = ui * 10;
	 ui = ui + unsigned_val(CAR(list_val(lst))) - '0';
	 n++;
	 lst = CDR(list_val(lst));
	 if (is_nil(lst)) {
	     break;
	 }
	 if (is_not_list(lst)) {
	     break;
	 }
     }

     tail = lst;
     if (!n) {
	 error_res = LTI_NO_INTEGER;
	 goto error;
     } 

	 
      /* If n <= 8 then we know it's a small int 
      ** since 2^27 = 134217728. If n > 8 then we must
      ** construct a bignum and let that routine do the checking
      */

     if (n <= SMALL_DIGITS) {  /* It must be small */
	 if (neg) i = -(Sint)ui;
         else i = (Sint)ui;
	 res = make_small(i);
     } else {
	 lg2 =  (n+1)*230/69+1;
	 m  = (lg2+D_EXP-1)/D_EXP; /* number of digits */
	 m  = BIG_NEED_SIZE(m);    /* number of words + thing */

	 hp = HAlloc(p, m);
	 hp_end = hp + m;
	 
	 lst = orig_list;
	 if (skip)
	     lst = CDR(list_val(lst));
	 
	 /* load first digits (at least one digit) */
	 if ((i = (n % D_DECIMAL_EXP)) == 0)
	     i = D_DECIMAL_EXP;
	 n -= i;
	 m = 0;
	 while(i--) {
	     m = 10*m + (unsigned_val(CAR(list_val(lst))) - '0');
	     lst = CDR(list_val(lst));
	 }
	 res = small_to_big(m, hp);  /* load first digits */
	 
	 while(n) {
	     i = D_DECIMAL_EXP;
	     n -= D_DECIMAL_EXP;
	     m = 0;
	     while(i--) {
		 m = 10*m + (unsigned_val(CAR(list_val(lst))) - '0');
		 lst = CDR(list_val(lst));
	     }
	     if (is_small(res))
		 res = small_to_big(signed_val(res), hp);
	     res = big_times_small(res, D_DECIMAL_BASE, hp);
	     if (is_small(res))
		 res = small_to_big(signed_val(res), hp);
	     res = big_plus_small(res, m, hp);
	 }

	 if (neg) {
	     if (is_small(res))
		 res = make_small(-signed_val(res));
	     else {
		 Uint *big = big_val(res); /* point to thing */
		 *big = bignum_header_neg(*big);
	     }
	 }

	 if (is_not_small(res)) {
	     res = big_plus_small(res, 0, hp); /* includes conversion to small */

	     if (is_not_small(res)) {
		 hp += (big_arity(res)+1);
	     }
	 }
	 HRelease(p,hp_end,hp);
     }
     *integer = res;
     *rest = tail;
     if (tail != NIL) {
	 return LTI_SOME_INTEGER;
     }
     return LTI_ALL_INTEGER;
}
BIF_RETTYPE string_to_integer_1(BIF_ALIST_1)
{
     Eterm res;
     Eterm tail;
     Eterm *hp;
     /* must be a list */
     switch (do_list_to_integer(BIF_P,BIF_ARG_1,&res,&tail)) {
	 /* HAlloc after do_list_to_integer as it 
	    might HAlloc itself (bignum) */
     case LTI_BAD_STRUCTURE:
	 hp = HAlloc(BIF_P,3);
	 BIF_RET(TUPLE2(hp, am_error, am_not_a_list));
     case LTI_NO_INTEGER:
	 hp = HAlloc(BIF_P,3);
	 BIF_RET(TUPLE2(hp, am_error, am_no_integer));
     default:
	 hp = HAlloc(BIF_P,3);
	 BIF_RET(TUPLE2(hp, res, tail));
     }
}

BIF_RETTYPE list_to_integer_1(BIF_ALIST_1)
 {
   /* Using do_list_to_integer is about twice as fast as using 
      erts_chars_to_integer because we do not have to copy the 
      entire list */
     Eterm res;
     Eterm dummy;
     /* must be a list */
     if (do_list_to_integer(BIF_P,BIF_ARG_1,&res,&dummy) != LTI_ALL_INTEGER) {
	 BIF_ERROR(BIF_P,BADARG);
     }
     BIF_RET(res);
 }

BIF_RETTYPE list_to_integer_2(BIF_ALIST_2)
{

  /* Bif implementation is about 50% faster than pure erlang,
     and since we have erts_chars_to_integer now it is simpler
     as well. This could be optmized further if we did not have to
     copy the list to buf. */
    int i;
    Eterm res;
    char *buf = NULL;
    int base;

    i = erts_list_length(BIF_ARG_1);
    if (i < 0)
      BIF_ERROR(BIF_P, BADARG);
    
    base = signed_val(BIF_ARG_2);
  
    if (base < 2 || base > 36) 
      BIF_ERROR(BIF_P, BADARG);

    /* Take fast path if base it 10 */
    if (base == 10)
      return list_to_integer_1(BIF_P,&BIF_ARG_1);
    
    buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
    
    if (intlist_to_buf(BIF_ARG_1, buf, i) < 0)
      goto list_to_integer_1_error;
    buf[i] = '\0';		/* null terminal */
    
    if ((res = erts_chars_to_integer(BIF_P,buf,i,base)) == THE_NON_VALUE)
      goto list_to_integer_1_error;
    
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(res);
    
 list_to_integer_1_error:
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_ERROR(BIF_P, BADARG);
     
 }

/**********************************************************************/

BIF_RETTYPE float_to_list_1(BIF_ALIST_1)
{
  EPIPHANY_STUB_FUN();
}

BIF_RETTYPE float_to_list_2(BIF_ALIST_2)
{
  EPIPHANY_STUB_FUN();
}

BIF_RETTYPE float_to_binary_1(BIF_ALIST_1)
{
  EPIPHANY_STUB_FUN();
}

BIF_RETTYPE float_to_binary_2(BIF_ALIST_2)
{
  EPIPHANY_STUB_FUN();
}

/**********************************************************************/

/* convert a list of ascii  integer values e's +'s and -'s to a float */


#define SIGN      0
#define INT       1
#define FRAC      2
#define EXP_SIGN  3
#define EXP0      4
#define EXP1      5
#define END       6

#define IS_DOT(x) (unsigned_val((x)) == '.' || unsigned_val((x)) == ',')
#define IS_E(x) (unsigned_val((x)) == 'e' || unsigned_val((x)) == 'E')
#define IS_DIGIT(x) (unsigned_val((x)) >= '0' && unsigned_val((x)) <= '9')
#define SAVE_E(xi,xim,xl,xlm) ((xim)=(xi), (xlm)=(xl))
#define LOAD_E(xi,xim,xl,xlm) ((xi)=(xim), (xl)=(xlm))

#define STRING_TO_FLOAT_BUF_INC_SZ (128)
BIF_RETTYPE string_to_float_1(BIF_ALIST_1)
{
    Eterm orig = BIF_ARG_1;
    Eterm list = orig;
    Eterm list_mem = list;
    int i = 0;
    int i_mem = 0;
    Eterm* hp;
    Eterm error_res = NIL;
    int part = SIGN;	/* expect a + or - (or a digit) first */
    FloatDef f;
    Eterm tup;
    byte *buf = NULL;
    Uint bufsz = STRING_TO_FLOAT_BUF_INC_SZ;

    /* check it's a valid list to start with */
    if (is_nil(list)) {
	error_res = am_no_float;
    error:
	if (buf)
	    erts_free(ERTS_ALC_T_TMP, (void *) buf);
	hp = HAlloc(BIF_P, 3);    
	BIF_RET(TUPLE2(hp, am_error, error_res));
    }
    if (is_not_list(list)) {
	error_res = am_not_a_list;
	goto error;
    }

    buf = (byte *) erts_alloc(ERTS_ALC_T_TMP, bufsz);

    /*
       The float might start with a SIGN (+ | -). It must contain an integer 
       part, INT, followed by a delimiter (. | ,) and a fractional, FRAC, 
       part. The float might also contain an exponent. If e or E indicates 
       this we will look for a possible EXP_SIGN (+ | -) followed by the
       exponential number, EXP. (EXP0 is the first digit and EXP1 the rest).

       When we encounter an expected e or E, we can't tell if it's part of
       the float or the rest of the string. We save the current position 
       with SAVE_E. If we later find out it was not part of the float, we
       restore the position (end of the float) with LOAD_E.
    */
    while(1) {
	if (is_not_small(CAR(list_val(list)))) 
	    goto back_to_e;
	if (CAR(list_val(list)) == make_small('-')) {
	    switch (part) {
	    case SIGN:		/* expect integer part next */
		part = INT;		
		break;
	    case EXP_SIGN:	/* expect first digit in exp */
		part = EXP0;
		break;
	    case EXP0:		/* example: "2.3e--" */
		LOAD_E(i, i_mem, list, list_mem);
	    default:		/* unexpected - done */
		part = END;
	    }
	} else if (CAR(list_val(list)) == make_small('+')) {
	    switch (part) {
	    case SIGN:		/* expect integer part next */
		part = INT;
		goto skip;
	    case EXP_SIGN:	/* expect first digit in exp */
		part = EXP0;
		break;
	    case EXP0:		/* example: "2.3e++" */
		LOAD_E(i, i_mem, list, list_mem);
	    default:		/* unexpected - done */
		part = END;
	    }
	} else if (IS_DOT(CAR(list_val(list)))) { /* . or , */
	    switch (part) {
	    case INT:		/* expect fractional part next */
		part = FRAC;
		break;
	    case EXP_SIGN:	/* example: "2.3e." */
		LOAD_E(i, i_mem, list, list_mem);
	    case EXP0:		/* example: "2.3e+." */
		LOAD_E(i, i_mem, list, list_mem);
	    default:		/* unexpected - done */
		part = END;
	    }
	} else if (IS_E(CAR(list_val(list)))) {	/* e or E */
	    switch (part) {
	    case FRAC:		/* expect a + or - (or a digit) next */
		/* 
		   remember the position of e in case we find out later
		   that it was not part of the float, e.g. "2.3eh?" 
		*/
		SAVE_E(i, i_mem, list, list_mem);
		part = EXP_SIGN;
		break;
	    case EXP0:		/* example: "2.3e+e" */
	    case EXP_SIGN:	/* example: "2.3ee" */
		LOAD_E(i, i_mem, list, list_mem);
	    case INT:		/* would like this to be ok, example "2e2",
				   but it's not compatible with list_to_float */
	    default:		/* unexpected - done */
		part = END;
	    }
	} else if (IS_DIGIT(CAR(list_val(list)))) { /* digit */
	    switch (part) {
	    case SIGN:		/* got initial digit in integer part */
		part = INT;	/* expect more digits to follow */
		break;
	    case EXP_SIGN:	/* expect exponential part */
	    case EXP0:		/* expect rest of exponential */
		part = EXP1;
		break;
	    }
	} else			/* character not part of float - done */
	    goto back_to_e;
	
	if (part == END) {
	    if (i < 3) {	/* we require a fractional part */
		error_res = am_no_float;
		goto error;
	    }
	    break;
	}

	buf[i++] = unsigned_val(CAR(list_val(list)));

	if (i == bufsz - 1)
	    buf = (byte *) erts_realloc(ERTS_ALC_T_TMP,
					(void *) buf,
					bufsz += STRING_TO_FLOAT_BUF_INC_SZ);
    skip:
	list = CDR(list_val(list)); /* next element */

	if (is_nil(list))
	    goto back_to_e;

	if (is_not_list(list)) {
	back_to_e:
	    if (part == EXP_SIGN || part == EXP0) {
		LOAD_E(i, i_mem, list, list_mem);
	    }
	    break;
	}
    }
      
    if (i == 0) {		/* no float first in list */
	error_res = am_no_float;
	goto error;
    }

    buf[i] = '\0';		/* null terminal */
    ASSERT(bufsz >= i + 1);
    if (sys_chars_to_double((char*) buf, &f.fd) != 0) {
	error_res = am_no_float;
	goto error;
    }
    hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT + 3);
    tup = TUPLE2(hp+FLOAT_SIZE_OBJECT, make_float(hp), list);
    PUT_DOUBLE(f, hp);
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(tup);
}

static BIF_RETTYPE do_charbuf_to_float(Process *BIF_P,char *buf) {
  FloatDef f;
  Eterm res;
  Eterm* hp;

  if (sys_chars_to_double(buf, &f.fd) != 0)
    BIF_ERROR(BIF_P, BADARG);

  hp = HAlloc(BIF_P, FLOAT_SIZE_OBJECT);
  res = make_float(hp);
  PUT_DOUBLE(f, hp);
  BIF_RET(res);

}

BIF_RETTYPE list_to_float_1(BIF_ALIST_1)
{
    int i;
    Eterm res;
    char *buf = NULL;

    i = erts_list_length(BIF_ARG_1);
    if (i < 0)
      BIF_ERROR(BIF_P, BADARG);
    
    buf = (char *) erts_alloc(ERTS_ALC_T_TMP, i + 1);
    
    if (intlist_to_buf(BIF_ARG_1, buf, i) < 0)
      goto list_to_float_1_error;
    buf[i] = '\0';		/* null terminal */
    
    if ((res = do_charbuf_to_float(BIF_P,buf)) == THE_NON_VALUE)
      goto list_to_float_1_error;
    
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_RET(res);
    
 list_to_float_1_error:
    erts_free(ERTS_ALC_T_TMP, (void *) buf);
    BIF_ERROR(BIF_P, BADARG);

}

/**********************************************************************/

/* convert a tuple to a list */

BIF_RETTYPE tuple_to_list_1(BIF_ALIST_1)
{
    Uint n;
    Eterm *tupleptr;
    Eterm list = NIL;
    Eterm* hp;

    if (is_not_tuple(BIF_ARG_1))  {
	BIF_ERROR(BIF_P, BADARG);
    }

    tupleptr = tuple_val(BIF_ARG_1);
    n = arityval(*tupleptr);
    hp = HAlloc(BIF_P, 2 * n);
    tupleptr++;

    while(n--) {
	list = CONS(hp, tupleptr[n], list);
	hp += 2;
    }
    BIF_RET(list);
}

/**********************************************************************/

/* convert a list to a tuple */

BIF_RETTYPE list_to_tuple_1(BIF_ALIST_1)
{
    Eterm list = BIF_ARG_1;
    Eterm* cons;
    Eterm res;
    Eterm* hp;
    int len;

    if ((len = erts_list_length(list)) < 0 || len > ERTS_MAX_TUPLE_SIZE) {
	BIF_ERROR(BIF_P, BADARG);
    }

    hp = HAlloc(BIF_P, len+1);
    res = make_tuple(hp);
    *hp++ = make_arityval(len);
    while(is_list(list)) {
	cons = list_val(list);
	*hp++ = CAR(cons);
	list = CDR(cons);
    }
    BIF_RET(res);
}

/**********************************************************************/

/* return the pid of our own process, in most cases this has been replaced by
   a machine instruction */

BIF_RETTYPE self_0(BIF_ALIST_0)
{
     BIF_RET(BIF_P->common.id);
}

/**********************************************************************/

/*
   New representation of refs in R9, see erl_term.h

   In the first data word, only the usual 18 bits are used. Ordinarily,
   in "long refs" all words are used (in other words, practically never
   wrap around), but for compatibility with older nodes, "short refs"
   exist. Short refs come into being by being converted from the old
   external format for refs (tag REFERENCE_EXT). Short refs are
   converted back to the old external format.

   When converting a long ref to the external format in the case of
   preparing for sending to an older node, the ref is truncated by only
   using the first word (with 18 significant bits), and using the old tag
   REFERENCE_EXT.

   When comparing refs or different size, only the parts up to the length
   of the shorter operand are used. This has the desirable effect that a
   long ref sent to an old node and back will be treated as equal to
   the original, although some of the bits have been lost.

   The hash value for a ref always considers only the first word, since
   in the above scenario, the original and the copy should have the same
   hash value.
*/

static Uint32 reference0; /* Initialized in erts_init_bif */
static Uint32 reference1;
static Uint32 reference2;

void
erts_make_ref_in_array(Uint32 ref[ERTS_MAX_REF_NUMBERS])
{
    EPIPHANY_STUB_FUN();
}

Eterm erts_make_ref_in_buffer(Eterm buffer[REF_THING_SIZE])
{
    Eterm* hp = buffer;
    Uint32 ref[ERTS_MAX_REF_NUMBERS];

    erts_make_ref_in_array(ref);
    write_ref_thing(hp, ref[0], ref[1], ref[2]);
    return make_internal_ref(hp);
}

Eterm erts_make_ref(Process *p)
{
    Eterm* hp;
    Uint32 ref[ERTS_MAX_REF_NUMBERS];

    ERTS_SMP_LC_ASSERT(ERTS_PROC_LOCK_MAIN & erts_proc_lc_my_proc_locks(p));

    hp = HAlloc(p, REF_THING_SIZE);

    erts_make_ref_in_array(ref);
    write_ref_thing(hp, ref[0], ref[1], ref[2]);

    return make_internal_ref(hp);
}

BIF_RETTYPE make_ref_0(BIF_ALIST_0)
{
    return erts_make_ref(BIF_P);
}

/**********************************************************************/

BIF_RETTYPE garbage_collect_0(BIF_ALIST_0)
{
    int reds;

    FLAGS(BIF_P) |= F_NEED_FULLSWEEP;
    reds = erts_garbage_collect(BIF_P, 0, NULL, 0);
    BIF_RET2(am_true, reds);
}

/**********************************************************************/

BIF_RETTYPE throw_1(BIF_ALIST_1)
{
    BIF_P->fvalue = BIF_ARG_1;
    BIF_ERROR(BIF_P, EXC_THROWN);
}

/**********************************************************************/


/* 
 * Non-standard, undocumented, dirty BIF, meant for debugging.
 *
 */
BIF_RETTYPE display_1(BIF_ALIST_1)
{
    erts_printf("%.*T\n", INT_MAX, BIF_ARG_1);
    BIF_RET(am_true);
}

BIF_RETTYPE display_string_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm string = BIF_ARG_1;
    int len = is_string(string);
    char *str;

    if (len <= 0) {
	BIF_ERROR(p, BADARG);
    }
    str = (char *) erts_alloc(ERTS_ALC_T_TMP, sizeof(char)*(len + 1));
    if (intlist_to_buf(string, str, len) != len)
	erl_exit(1, "%s:%d: Internal error\n", __FILE__, __LINE__);
    str[len] = '\0';
    erts_fprintf(stderr, "%s", str);
    erts_free(ERTS_ALC_T_TMP, (void *) str);
    BIF_RET(am_true);
}

BIF_RETTYPE display_nl_0(BIF_ALIST_0)
{
    erts_fprintf(stderr, "\n");
    BIF_RET(am_true);
}

/**********************************************************************/

BIF_RETTYPE function_exported_3(BIF_ALIST_3)
{
    if (is_not_atom(BIF_ARG_1) ||
	is_not_atom(BIF_ARG_2) || 
	is_not_small(BIF_ARG_3)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if (erts_find_function(BIF_ARG_1, BIF_ARG_2, signed_val(BIF_ARG_3),
			   erts_active_code_ix()) == NULL) {
	BIF_RET(am_false);
    }
    BIF_RET(am_true);
}

/**********************************************************************/    

BIF_RETTYPE is_builtin_3(BIF_ALIST_3)
{
    Process* p = BIF_P;
    Eterm Mod = BIF_ARG_1;
    Eterm Name = BIF_ARG_2;
    Eterm Arity = BIF_ARG_3;

    if (is_not_atom(Mod) || is_not_atom(Name) || is_not_small(Arity)) {
	BIF_ERROR(p, BADARG);
    }
    BIF_RET(erts_is_builtin(Mod, Name, signed_val(Arity)) ?
	    am_true : am_false);
}

/**********************************************************************/    

/* NOTE: Cannot be used in all *_to_list() bifs. erts_dsprintf() prints
 *       some terms on other formats than what is desired as results
 *       from *_to_list() bifs.
 */

static Eterm
term2list_dsprintf(Process *p, Eterm term)
{
    int pres;
    Eterm res;
    Eterm *hp;
    erts_dsprintf_buf_t *dsbufp = erts_create_tmp_dsbuf(64);       
    pres = erts_dsprintf(dsbufp, "%T", term);
    if (pres < 0)
	erl_exit(1, "Failed to convert term to list: %d\n", -pres);
    hp = HAlloc(p, 2*dsbufp->str_len); /* we need length * 2 heap words */
    res = buf_to_intlist(&hp, dsbufp->str, dsbufp->str_len, NIL);
    erts_destroy_tmp_dsbuf(dsbufp);
    return res;
}

BIF_RETTYPE ref_to_list_1(BIF_ALIST_1)
{
    if (is_not_ref(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(term2list_dsprintf(BIF_P, BIF_ARG_1));
}

BIF_RETTYPE fun_to_list_1(BIF_ALIST_1)
{
    Process* p = BIF_P;
    Eterm fun = BIF_ARG_1;

    if (is_not_any_fun(fun))
	BIF_ERROR(p, BADARG);
    BIF_RET(term2list_dsprintf(p, fun));
}

/**********************************************************************/    

/* convert a pid to an erlang list (for the linked cons cells) of the form
   <node.number.serial> to a PID
 */

BIF_RETTYPE pid_to_list_1(BIF_ALIST_1)
{
    if (is_not_pid(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(term2list_dsprintf(BIF_P, BIF_ARG_1));
}

BIF_RETTYPE port_to_list_1(BIF_ALIST_1)
{
    if (is_not_port(BIF_ARG_1))
	BIF_ERROR(BIF_P, BADARG);
    BIF_RET(term2list_dsprintf(BIF_P, BIF_ARG_1));
}

/**********************************************************************/

BIF_RETTYPE group_leader_0(BIF_ALIST_0)
{
    BIF_RET(BIF_P->group_leader);
}

/**********************************************************************/

BIF_RETTYPE hash_2(BIF_ALIST_2)
{
    Uint32 hash;
    Sint range;

    if (is_not_small(BIF_ARG_2)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    if ((range = signed_val(BIF_ARG_2)) <= 0) {  /* [1..MAX_SMALL] */
	BIF_ERROR(BIF_P, BADARG);
    }
#if defined(ARCH_64) && !HALFWORD_HEAP
    if (range > ((1L << 27) - 1))
	BIF_ERROR(BIF_P, BADARG);
#endif
    hash = make_broken_hash(BIF_ARG_1);
    BIF_RET(make_small(1 + (hash % range)));   /* [1..range] */
}

BIF_RETTYPE phash_2(BIF_ALIST_2)
{
    Uint32 hash;
    Uint32 final_hash;
    Uint32 range;

    /* Check for special case 2^32 */
    if (term_equals_2pow32(BIF_ARG_2)) {
	range = 0;
    } else {
	Uint u;
	if (!term_to_Uint(BIF_ARG_2, &u) || ((u >> 16) >> 16) != 0 || !u) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	range = (Uint32) u;
    }
    hash = make_hash(BIF_ARG_1);
    if (range) {
	final_hash = 1 + (hash % range); /* [1..range] */
    } else if ((final_hash = hash + 1) == 0) {
	/*
	 * XXX In this case, there will still be a ArithAlloc() in erts_mixed_plus().
	 */
	BIF_RET(erts_mixed_plus(BIF_P,
				erts_make_integer(hash, BIF_P),
				make_small(1)));
    }

    BIF_RET(erts_make_integer(final_hash, BIF_P));
}

BIF_RETTYPE phash2_1(BIF_ALIST_1)
{
    Uint32 hash;

    hash = make_hash2(BIF_ARG_1);
    BIF_RET(make_small(hash & ((1L << 27) - 1)));
}

BIF_RETTYPE phash2_2(BIF_ALIST_2)
{
    Uint32 hash;
    Uint32 final_hash;
    Uint32 range;

    /* Check for special case 2^32 */
    if (term_equals_2pow32(BIF_ARG_2)) {
	range = 0;
    } else {
	Uint u;
	if (!term_to_Uint(BIF_ARG_2, &u) || ((u >> 16) >> 16) != 0 || !u) {
	    BIF_ERROR(BIF_P, BADARG);
	}
	range = (Uint32) u;
    }
    hash = make_hash2(BIF_ARG_1);
    if (range) {
	final_hash = hash % range; /* [0..range-1] */
    } else {
	final_hash = hash;
    }
    /*
     * Return either a small or a big. Use the heap for bigs if there is room.
     */
#if defined(ARCH_64) && !HALFWORD_HEAP
    BIF_RET(make_small(final_hash));
#else
    if (IS_USMALL(0, final_hash)) {
	BIF_RET(make_small(final_hash));
    } else {
	Eterm* hp = HAlloc(BIF_P, BIG_UINT_HEAP_SIZE);
	BIF_RET(uint_to_big(final_hash, hp));
    }
#endif
}

BIF_RETTYPE bump_reductions_1(BIF_ALIST_1)
{
    Sint reds;
	
    if (is_not_small(BIF_ARG_1) || ((reds = signed_val(BIF_ARG_1)) < 0)) {
	BIF_ERROR(BIF_P, BADARG);
    }
    
    if (reds > CONTEXT_REDS) {
        reds = CONTEXT_REDS;
    }
    BIF_RET2(am_true, reds);
}

BIF_RETTYPE erts_internal_cmp_term_2(BIF_ALIST_2) {
    int res = CMP_TERM(BIF_ARG_1,BIF_ARG_2);

    /* ensure -1, 0, 1 result */
    if (res < 0) {
	BIF_RET(make_small(-1));
    } else if (res > 0) {
	BIF_RET(make_small(1));
    }
    BIF_RET(make_small(0));
}

/*
 * NOTE: The erts_bif_prep_await_proc_exit_*() functions are
 * tightly coupled with the implementation of erlang:await_proc_exit/3.
 * The erts_bif_prep_await_proc_exit_*() functions can safely call
 * skip_current_msgq() since they know that erlang:await_proc_exit/3
 * unconditionally will do a monitor and then unconditionally will
 * wait for the corresponding 'DOWN' message in a receive, and no other
 * receive is done before this receive. This optimization removes an
 * unnecessary scan of the currently existing message queue (which
 * can be large). If the erlang:await_proc_exit/3 implementation
 * is changed so that the above isn't true, nasty bugs in later
 * receives, etc, may appear.
 */

static ERTS_INLINE int
skip_current_msgq(Process *c_p)
{
    EPIPHANY_STUB_FUN();
}

void
erts_bif_prep_await_proc_exit_data_trap(Process *c_p, Eterm pid, Eterm ret)
{
    if (skip_current_msgq(c_p)) {
	ERTS_BIF_PREP_TRAP3_NO_RET(await_proc_exit_trap, c_p, pid, am_data, ret);
    }
}

void
erts_bif_prep_await_proc_exit_reason_trap(Process *c_p, Eterm pid)
{
    if (skip_current_msgq(c_p)) {
	ERTS_BIF_PREP_TRAP3_NO_RET(await_proc_exit_trap, c_p,
			    pid, am_reason, am_undefined);
    }
}

void
erts_bif_prep_await_proc_exit_apply_trap(Process *c_p,
					 Eterm pid,
					 Eterm module,
					 Eterm function,
					 Eterm args[],
					 int nargs)
{
    ASSERT(is_atom(module) && is_atom(function));
    if (skip_current_msgq(c_p)) {
	Eterm term;
	Eterm *hp;
	int i;

	hp = HAlloc(c_p, 4+2*nargs);
	term = NIL;
	for (i = nargs-1; i >= 0; i--) {
	    term = CONS(hp, args[i], term);
	    hp += 2;
	}
	term = TUPLE3(hp, module, function, term);
	ERTS_BIF_PREP_TRAP3_NO_RET(await_proc_exit_trap, c_p, pid, am_apply, term);
    }
}

Export bif_return_trap_export;

void erts_init_trap_export(Export* ep, Eterm m, Eterm f, Uint a,
			   Eterm (*bif)(BIF_ALIST_0))
{
    int i;
    sys_memset((void *) ep, 0, sizeof(Export));
    for (i=0; i<ERTS_NUM_CODE_IX; i++) {
	ep->addressv[i] = &ep->code[3];
    }
    ep->code[0] = m;
    ep->code[1] = f;
    ep->code[2] = a;
    ep->code[3] = (BeamInstr) em_apply_bif;
    ep->code[4] = (BeamInstr) bif;
}

void erts_init_bif(void)
{
    reference0 = 0;
    reference1 = 0;
    reference2 = 0;


    // ETODO: Init export fields
}

#ifdef HARDDEBUG
/*
You'll need this line in bif.tab to be able to use this debug bif

bif erlang:send_to_logger/2

*/
BIF_RETTYPE send_to_logger_2(BIF_ALIST_2)
{
    byte *buf;
    ErlDrvSizeT len;
    if (!is_atom(BIF_ARG_1) || !(is_list(BIF_ARG_2) ||
				 is_nil(BIF_ARG_1))) {
	BIF_ERROR(BIF_P,BADARG);
    }
    if (erts_iolist_size(BIF_ARG_2, &len) != 0)
	BIF_ERROR(BIF_P,BADARG);
    else if (len == 0)
	buf = "";
    else {
#ifdef DEBUG
	ErlDrvSizeT len2;
#endif
	buf = (byte *) erts_alloc(ERTS_ALC_T_TMP, len+1);
#ifdef DEBUG
	len2 =
#else
	(void)
#endif
	    erts_iolist_to_buf(BIF_ARG_2, buf, len);
	ASSERT(len2 == len);
	buf[len] = '\0';
	switch (BIF_ARG_1) {
	case am_info:
	    erts_send_info_to_logger(BIF_P->group_leader, buf, len);
	    break;
	case am_warning:
	    erts_send_warning_to_logger(BIF_P->group_leader, buf, len);
	    break;
	case am_error:
	    erts_send_error_to_logger(BIF_P->group_leader, buf, len);
	    break;
	default:
	{
	    BIF_ERROR(BIF_P,BADARG);
	}
	}
	erts_free(ERTS_ALC_T_TMP, (void *) buf);
    }
    BIF_RET(am_true);
}
#endif /* HARDDEBUG */

BIF_RETTYPE dt_put_tag_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm otag;
    if (BIF_ARG_1 == am_undefined) {
	otag = (DT_UTAG(BIF_P) == NIL) ? am_undefined : DT_UTAG(BIF_P);
	DT_UTAG(BIF_P) = NIL;
	DT_UTAG_FLAGS(BIF_P) = 0;
	if (SEQ_TRACE_TOKEN(BIF_P) == am_have_dt_utag) {
	    SEQ_TRACE_TOKEN(BIF_P) = NIL;
	}
	BIF_RET(otag);
    }
    if (!is_binary(BIF_ARG_1)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    otag = (DT_UTAG(BIF_P) == NIL) ? am_undefined : DT_UTAG(BIF_P);
    DT_UTAG(BIF_P) = BIF_ARG_1;
    DT_UTAG_FLAGS(BIF_P) |= DT_UTAG_PERMANENT;
    if (SEQ_TRACE_TOKEN(BIF_P) == NIL) {
	SEQ_TRACE_TOKEN(BIF_P) = am_have_dt_utag;
    }
    BIF_RET(otag);
#else
    BIF_RET(am_undefined);
#endif
}

BIF_RETTYPE dt_get_tag_0(BIF_ALIST_0)
{
#ifdef USE_VM_PROBES
    BIF_RET((DT_UTAG(BIF_P) == NIL || !(DT_UTAG_FLAGS(BIF_P) & DT_UTAG_PERMANENT)) ? am_undefined : DT_UTAG(BIF_P));
#else
    BIF_RET(am_undefined);
#endif
}
BIF_RETTYPE dt_get_tag_data_0(BIF_ALIST_0)
{
#ifdef USE_VM_PROBES
    BIF_RET((DT_UTAG(BIF_P) == NIL) ? am_undefined : DT_UTAG(BIF_P));
#else
    BIF_RET(am_undefined);
#endif
}
BIF_RETTYPE dt_prepend_vm_tag_data_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm b; 
    Eterm *hp;
    if (is_binary((DT_UTAG(BIF_P)))) {
	Uint sz = binary_size(DT_UTAG(BIF_P));
	int i;
	unsigned char *p,*q;
	byte *temp_alloc = NULL;
	b = new_binary(BIF_P,NULL,sz+1);
	q = binary_bytes(b);
	p = erts_get_aligned_binary_bytes(DT_UTAG(BIF_P),&temp_alloc);
	for(i=0;i<sz;++i) {
	    q[i] = p[i];
	} 
	erts_free_aligned_binary_bytes(temp_alloc);
	q[sz] = '\0';
    } else {
	b = new_binary(BIF_P,(byte *)"\0",1);
    }
    hp = HAlloc(BIF_P,2);
    BIF_RET(CONS(hp,b,BIF_ARG_1));
#else
    BIF_RET(BIF_ARG_1);
#endif
}
BIF_RETTYPE dt_append_vm_tag_data_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm b; 
    Eterm *hp;
    if (is_binary((DT_UTAG(BIF_P)))) {
	Uint sz = binary_size(DT_UTAG(BIF_P));
	int i;
	unsigned char *p,*q;
	byte *temp_alloc = NULL;
	b = new_binary(BIF_P,NULL,sz+1);
	q = binary_bytes(b);
	p = erts_get_aligned_binary_bytes(DT_UTAG(BIF_P),&temp_alloc);
	for(i=0;i<sz;++i) {
	    q[i] = p[i];
	} 
	erts_free_aligned_binary_bytes(temp_alloc);
	q[sz] = '\0';
    } else {
	b = new_binary(BIF_P,(byte *)"\0",1);
    }
    hp = HAlloc(BIF_P,2);
    BIF_RET(CONS(hp,BIF_ARG_1,b));
#else
    BIF_RET(BIF_ARG_1);
#endif
}
BIF_RETTYPE dt_spread_tag_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm ret;
    Eterm *hp;
#endif
    if (BIF_ARG_1 != am_true && BIF_ARG_1 != am_false) {
	BIF_ERROR(BIF_P,BADARG);
    }
#ifdef USE_VM_PROBES
    hp = HAlloc(BIF_P,3);
    ret = TUPLE2(hp,make_small(DT_UTAG_FLAGS(BIF_P)),DT_UTAG(BIF_P));
    if (DT_UTAG(BIF_P) != NIL) {
	if (BIF_ARG_1 == am_true) {
	    DT_UTAG_FLAGS(BIF_P) |= DT_UTAG_SPREADING;
#ifdef DTRACE_TAG_HARDDEBUG
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) start spreading tag %T\r\n",
			 BIF_P->common.id,DT_UTAG(BIF_P));
#endif
	} else {
	    DT_UTAG_FLAGS(BIF_P) &= ~DT_UTAG_SPREADING;
#ifdef DTRACE_TAG_HARDDEBUG
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) stop spreading tag %T\r\n",
			 BIF_P->common.id,DT_UTAG(BIF_P));
#endif
	}
    }
    BIF_RET(ret);
#else
    BIF_RET(am_true);
#endif
}
BIF_RETTYPE dt_restore_tag_1(BIF_ALIST_1)
{
#ifdef USE_VM_PROBES
    Eterm *tpl;
    Uint x;
    if (is_not_tuple(BIF_ARG_1)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    tpl = tuple_val(BIF_ARG_1);
    if(arityval(*tpl) != 2 || is_not_small(tpl[1]) || (is_not_binary(tpl[2]) && tpl[2] != NIL)) {
	BIF_ERROR(BIF_P,BADARG);
    }
    if (tpl[2] == NIL) {
	if (DT_UTAG(BIF_P) != NIL) {
#ifdef DTRACE_TAG_HARDDEBUG
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) restore Killing tag!\r\n",
			 BIF_P->common.id);
#endif
	}
	DT_UTAG(BIF_P) = NIL;
	if (SEQ_TRACE_TOKEN(BIF_P) == am_have_dt_utag) {
	    SEQ_TRACE_TOKEN(BIF_P) = NIL;
	}
	DT_UTAG_FLAGS(BIF_P) = 0;
    } else {
	x = unsigned_val(tpl[1]) & (DT_UTAG_SPREADING | DT_UTAG_PERMANENT);
#ifdef DTRACE_TAG_HARDDEBUG

	if (!(x & DT_UTAG_SPREADING) && (DT_UTAG_FLAGS(BIF_P) & 
					 DT_UTAG_SPREADING)) {
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) restore stop spreading "
			 "tag %T\r\n",
			 BIF_P->common.id, tpl[2]);
	} else if ((x & DT_UTAG_SPREADING) && 
		   !(DT_UTAG_FLAGS(BIF_P) & DT_UTAG_SPREADING)) {
	    erts_fprintf(stderr,
			 "Dtrace -> (%T) restore start spreading "
			 "tag %T\r\n",BIF_P->common.id,tpl[2]);
	}
#endif
	DT_UTAG_FLAGS(BIF_P) = x;
	DT_UTAG(BIF_P) = tpl[2];
	if (SEQ_TRACE_TOKEN(BIF_P) == NIL) {
	    SEQ_TRACE_TOKEN(BIF_P) = am_have_dt_utag;
	}
    }
#else    
    if (BIF_ARG_1 != am_true) {
	BIF_ERROR(BIF_P,BADARG);
    }
#endif
    BIF_RET(am_true);
}


