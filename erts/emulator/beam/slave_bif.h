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

#ifndef SLAVE_BIF_H__
#define SLAVE_BIF_H__

#include "erl_term.h"
#include "erl_process.h"
#include "erl_bif_table.h"
#include "slave_command.h"

struct slave_syscall_bif {
    /* To master */
    Uint bif_no;
    Eterm args[3];
    /* Bidirectional */
    Eterm *heap, *htop, *hend, *stop;
    /* To slave */
    Eterm result;
} SLAVE_SHARED_DATA;

#ifndef ERTS_SLAVE
void erts_slave_serve_bif(struct slave *slave, struct slave_syscall_bif *arg);
#endif

#endif /* SLAVE_BIF_H__ */
