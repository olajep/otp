/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2009-2015. All Rights Reserved.
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

#define ETHR_INLINE_FUNC_NAME_(X) X ## __
#define ETHR_EVENT_IMPL__

/* We need the prototypes to include sys.h properly */
#ifndef DEBUG
#  define ETHR_NEED_SPINLOCK_PROTOTYPES__
#  define ETHR_NEED_RWSPINLOCK_PROTOTYPES__
#endif

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif
#include "sys.h"

#include "ethread.h"
#include "epiphany.h"

int
ethr_event_init(ethr_event *e)
{
    // ESTUB
    ASSERT(epiphany_in_dram(e));
    return 0;
}

int
ethr_event_destroy(ethr_event *e)
{
    // ESTUB
    ASSERT(epiphany_in_dram(e));
    return 0;
}

static ETHR_INLINE int
wait__(ethr_event *e, int spincount)
{
    EPIPHANY_STUB_FUN();
}

void
ethr_event_reset(ethr_event *e)
{
    EPIPHANY_STUB_FUN();
}

void
ethr_event_set(ethr_event *e)
{
    EPIPHANY_STUB_FUN();
}

int
ethr_event_wait(ethr_event *e)
{
    return wait__(e, 0);
}

int
ethr_event_swait(ethr_event *e, int spincount)
{
    return wait__(e, spincount);
}
