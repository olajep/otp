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

#include "sys.h"
#include "global.h"
#include "bif.h"

HIPE_WRAPPER_BIF_DISABLE_GC(term_to_binary, 1)
HIPE_WRAPPER_BIF_DISABLE_GC(term_to_binary, 2)
HIPE_WRAPPER_BIF_DISABLE_GC(binary_to_term, 1)
HIPE_WRAPPER_BIF_DISABLE_GC(binary_to_term, 2)
