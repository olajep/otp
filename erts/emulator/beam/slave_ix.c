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

#include "slave_ix.h"
#include "slave_syms.h"

static ErtsCodeIndex *const slave_active_ix =
    (void*)SLAVE_SYM_the_active_code_index;
static ErtsCodeIndex *const slave_staging_ix =
    (void*)SLAVE_SYM_the_staging_code_index;

static int initialized = 0;

void
slave_code_ix_init(void)
{
    initialized = 1;
    slave_code_ix_update();
}

void
slave_code_ix_update(void)
{
    if (!initialized) return;
    *slave_active_ix = erts_active_code_ix();
    *slave_staging_ix = erts_staging_code_ix();
}
