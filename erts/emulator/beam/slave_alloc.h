/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2013. All Rights Reserved.
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

#ifndef ERL_SLAVE_ALLOC_H__
#define ERL_SLAVE_ALLOC_H__
#include "erl_alloc.h"

void erl_slave_alloc_submit(void*, size_t);
void erl_slave_alloc_fallback(void);

void *erl_slave_alloc(ErtsAlcType_t, void *, Uint);
void *erl_slave_realloc(ErtsAlcType_t, void *, void *, Uint);
void erl_slave_free(ErtsAlcType_t, void *, void *);

#endif /* !defined(ERL_SLAVE_ALLOC_H__) */
