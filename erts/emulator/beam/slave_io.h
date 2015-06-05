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

#ifndef ERL_SLAVE_IO_H__
#define ERL_SLAVE_IO_H__

#include <sys/types.h>

#ifndef ERTS_SLAVE
/* These cause warnings with -Wstrict-prototypes */
#  pragma GCC diagnostic push
#  pragma GCC diagnostic ignored "-Wstrict-prototypes"
#  include <e-hal.h>
#  include <e-loader.h>
#  pragma GCC diagnostic pop
#endif

#define SLAVE_IO_OUTBUF_SZ 1024

void erts_init_slave_io(void);

#ifndef ERTS_SLAVE
void erts_stop_slave_io(void);
void erts_restart_slave_io(void);
extern e_epiphany_t slave_workgroup;
extern int erts_slave_online;
#endif

#endif /* ERL_SLAVE_IO_H__ */
