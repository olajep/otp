/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2015. All Rights Reserved.
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

#ifndef __ERL_SLAVE_PROCESS_H__
#define __ERL_SLAVE_PROCESS_H__

#include "sys.h"

#define ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__
#include "erl_process_lock.h" /* Only pull out important types... */
#undef ERTS_PROCESS_LOCK_ONLY_PROC_LOCK_TYPE__

#include "erl_vm.h"
#include "erl_smp.h"
#include "erl_message.h"
#include "erl_process_dict.h"
#include "erl_node_container_utils.h"
#include "erl_node_tables.h"
#include "erl_monitors.h"
#include "erl_bif_timer.h"
#include "erl_time.h"
#include "erl_atom_table.h"
#include "external.h"
#include "erl_mseg.h"
#include "erl_async.h"
#include "slave_command.h"

#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY
#define ERL_THR_PROGRESS_TSD_TYPE_ONLY
#include "erl_thr_progress.h"
#undef ERL_THR_PROGRESS_TSD_TYPE_ONLY

Eterm erl_create_slave_process(Process*, Eterm, Eterm, Eterm, ErlSpawnOpts*);
void slave_set_pending_exit(Process *p, Eterm reason);
int slave_do_exit_process(Process* p, struct slave_syscall_ready *arg);
Sint erts_slave_queue_message(Process* receiver, ErtsProcLocks *receiver_locks,
			      ErlHeapFragment* bp, Eterm message,
			      Eterm seq_trace_token
#ifdef USE_VM_PROBES
			      , Eterm dt_utag
#endif
			 );
struct slave;
void slave_free_message(struct slave *slave, ErlMessage *m);
void slave_free_message_buffer(struct slave *slave, ErlHeapFragment *bp);

#endif /* !defined(__ERL_SLAVE_PROCESS_H__) */
