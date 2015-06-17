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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "global.h"
#include "slave_command.h"

LoaderTarget loader_target_self;

void init_load(void)
{
    /* FloatDef f; */

    /* erts_total_code_size = 0; */

    /* beam_catches_init(); */

    /* f.fd = 1.0; */
    /* must_swap_floats = (f.fw[0] == 0); */

    erts_init_ranges();

#ifndef NO_JUMP_TABLE
    ASSERT(beam_ops);
    loader_target_self.beam_ops = beam_ops;
#endif

    erts_master_setup();
}

/*
 * Build a single {M,F,A,Loction} item to be part of
 * a stack trace.
 */
Eterm*
erts_build_mfa_item(FunctionInfo* fi, Eterm* hp, Eterm args, Eterm* mfa_p)
{
    BeamInstr* current = fi->current;
    Eterm loc = NIL;

    if (fi->loc != LINE_INVALID_LOCATION) {
	Eterm tuple;
	int line = LOC_LINE(fi->loc);
	int file = LOC_FILE(fi->loc);
	Eterm file_term = NIL;

	if (file == 0) {
	    Atom* ap = atom_tab(atom_val(fi->current[0]));
	    file_term = buf_to_intlist(&hp, ".erl", 4, NIL);
	    file_term = buf_to_intlist(&hp, (char*)ap->name, ap->len, file_term);
	} else {
	    Atom* ap = atom_tab(atom_val((fi->fname_ptr)[file-1]));
	    file_term = buf_to_intlist(&hp, (char*)ap->name, ap->len, NIL);
	}

	tuple = TUPLE2(hp, am_line, make_small(line));
	hp += 3;
	loc = CONS(hp, tuple, loc);
	hp += 2;
	tuple = TUPLE2(hp, am_file, file_term);
	hp += 3;
	loc = CONS(hp, tuple, loc);
	hp += 2;
    }

    if (is_list(args) || is_nil(args)) {
	*mfa_p = TUPLE4(hp, current[0], current[1], args, loc);
    } else {
	Eterm arity = make_small(current[2]);
	*mfa_p = TUPLE4(hp, current[0], current[1], arity, loc);
    }
    return hp + 5;
}

/*
 * Force setting of the current function in a FunctionInfo
 * structure. No source code location will be associated with
 * the function.
 */
void
erts_set_current_function(FunctionInfo* fi, BeamInstr* current)
{
    fi->current = current;
    fi->needed = 5;
    fi->loc = LINE_INVALID_LOCATION;
}

/*
 * Returns a pointer to {module, function, arity}, or NULL if not found.
 */
BeamInstr*
find_function_from_pc(BeamInstr* pc)
{
    FunctionInfo fi;

    erts_lookup_function_info(&fi, pc, 0);
    return fi.current;
}
