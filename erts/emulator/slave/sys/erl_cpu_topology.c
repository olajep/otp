/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2010-2015. All Rights Reserved.
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
 * Description:	CPU topology and related functionality
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "global.h"
#include "erl_cpu_topology.h"
#include "epiphany.h"

void
erts_get_logical_processors(int *conf, int *onln, int *avail)
{
    int workgroup = epiphany_workgroup_size();
    int online;
#ifdef ERTS_SMP
    online = workgroup;
#else
    online = 1;
#endif
    if (conf)
	*conf = workgroup;
    if (onln)
	*onln = online;
    if (avail)
	*avail = online;
}
