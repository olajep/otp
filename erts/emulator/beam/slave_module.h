/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2013. All Rights Reserved.
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

#ifndef __SLAVE_MODULE_H__
#define __SLAVE_MODULE_H__

#include "module.h"

Module* slave_get_module(Eterm mod, ErtsCodeIndex code_ix);
Module* slave_put_module(Eterm mod);

void slave_init_module_table(void);
void slave_module_start_staging(void);
void slave_module_end_staging(int commit);

Module *slave_module_code(int, ErtsCodeIndex);
int slave_module_code_size(ErtsCodeIndex);
int slave_module_table_sz(void);

Eterm slave_make_current_old(Process *c_p, ErtsProcLocks c_p_locks, Eterm module);

#endif /* !__SLAVE_MODULE_H__ */
