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

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "module.h"

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

/*
 * The tables are declared non-static because they are linked and modified from
 * the master
 */
IndexTable module_tables[ERTS_NUM_CODE_IX];

/* void module_info(int to, void *to_arg) */
/* { */
/*     index_info(to, to_arg, &module_tables[erts_active_code_ix()]); */
/* } */

static HashValue module_hash(Module* x)
{
    return (HashValue) x->module;
}

static int module_cmp(Module* tmpl, Module* obj)
{
    return tmpl->module != obj->module;
}

static Module* module_alloc(Module* tmpl)
{
    erl_exit(1, "Cannot alloc module entry from slave");
}

static void module_free(Module* mod)
{
    erl_exit(1, "Cannot free module entry from slave");
}

static HashFunctions fun = {
    (H_FUN)      module_hash,
    (HCMP_FUN)   module_cmp,
    (HALLOC_FUN) module_alloc,
    (HFREE_FUN)  module_free,
};

Module*
erts_get_module(Eterm mod, ErtsCodeIndex code_ix)
{
    Module e;
    int index;
    IndexTable* mod_tab;

    ASSERT(is_atom(mod));

    mod_tab = &module_tables[code_ix];

    e.module = atom_val(mod);
    index = index_get_ext(mod_tab, (void*) &e, &fun);
    if (index == -1) {
	return NULL;
    } else {
	return (Module*) erts_index_lookup(mod_tab, index);
    }
}

Module *module_code(int i, ErtsCodeIndex code_ix)
{
    return (Module*) erts_index_lookup(&module_tables[code_ix], i);
}
