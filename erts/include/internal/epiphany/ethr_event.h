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

typedef struct {} ethr_event;

#define ETHR_EVENT_OFF_WAITER__		-1L
#define ETHR_EVENT_OFF__		1L
#define ETHR_EVENT_ON__ 		0L

int ethr_event_init(ethr_event *e);
int ethr_event_destroy(ethr_event *e);
int ethr_event_wait(ethr_event *e);
int ethr_event_swait(ethr_event *e, int spincount);
void ethr_event_set(ethr_event *e);
void ethr_event_reset(ethr_event *e);
