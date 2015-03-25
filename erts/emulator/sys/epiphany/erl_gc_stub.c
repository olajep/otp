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
    EPIPHANY_STUB(erts_garbage_collect);
}

/* /\* */
/*  * Place all living data on a the new heap; deallocate any old heap. */
/*  * Meant to be used by hibernate/3. */
/*  *\/ */
/* void */
/* erts_garbage_collect_hibernate(Process* p) */
/* { */
/*     /\* STUB *\/ */
/* } */


/* void */
/* erts_garbage_collect_literals(Process* p, Eterm* literals, */
/* 			      Uint lit_size, */
/* 			      struct erl_off_heap_header* oh) */
/* { */
/*     /\* STUB *\/ */
/* } */

/* Eterm */
/* erts_gc_info_request(Process *c_p) */
/* { */
/*     /\* STUB *\/ */
/* } */
