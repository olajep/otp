/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2002-2015. All Rights Reserved.
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
#include "global.h"
#include "hipe_mode_switch.h"

int hipe_trap_count;

Process *hipe_mode_switch(Process *p, unsigned cmd, Eterm reg[])
{
    EPIPHANY_STUB_BT();
    return NULL;
}

Eterm hipe_build_stacktrace(Process *p, struct StackTrace *s)
{
    EPIPHANY_STUB_BT();
    return THE_NON_VALUE;
}
