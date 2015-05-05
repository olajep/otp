/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1999-2013. All Rights Reserved.
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

#ifndef _ERL_SLAVE_LOAD_H
#  define _ERL_SLAVE_LOAD_H

#include "slave_command.h"

#ifdef NO_JUMP_TABLE
#define SlaveOp(Op) (Op)
#else
#define SlaveOp(Op) (loader_target_slave->beam_ops[(Op)])
#endif

extern const LoaderTarget *loader_target_slave;
extern const TargetExportTab export_table_slave;
extern BifEntry slave_bif_table[];
extern Export *slave_bif_export[];
extern int erts_slave_booted;

void erts_slave_init_load(struct master_command_setup*);

/*
 * Caller must hold the code write lock,
 * erts_slave_can_bootstrap() must have returned 'yes'
 */
void erts_slave_bootstrap(void);
Eterm erts_slave_can_bootstrap(void);

#endif /* !defined(_ERL_SLAVE_LOAD_H) */
