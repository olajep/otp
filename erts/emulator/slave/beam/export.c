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
#include "erl_vm.h"
#include "global.h"
#include "export.h"
#include "hash.h"
#include "slave.h"

#define EXPORT_INITIAL_SIZE   4000
#define EXPORT_LIMIT  (512*1024)

#define EXPORT_HASH(m,f,a) ((m)*(f)+(a))

#ifdef DEBUG
#  define IF_DEBUG(x) x
#else
#  define IF_DEBUG(x)
#endif

/*
 * The tables are declared non-static because they are linked and modified from
 * the master
 */
IndexTable export_tables[ERTS_NUM_CODE_IX];

/* These structures must match exactly those in slave_export.c in the master */
struct SLAVE_SHARED_DATA export_entry
{
    IndexSlot slot; /* MUST BE LOCATED AT TOP OF STRUCT!!! */
    Export* ep;
};

/* Helper struct that brings things together in one allocation
*/
struct SLAVE_SHARED_DATA export_blob
{
    Export exp;
    struct export_entry entryv[ERTS_NUM_CODE_IX];
    /* Note that entryv is not indexed by "code_ix".
     */
};

/* Helper struct only used as template
*/
struct SLAVE_SHARED_DATA export_templ
{
    struct export_entry entry;
    Export exp;
};

static HashValue
export_hash(struct export_entry* ee)
{
    Export* x = ee->ep;
    return EXPORT_HASH(x->code[0], x->code[1], x->code[2]);
}

static int
export_cmp(struct export_entry* tmpl_e, struct export_entry* obj_e)
{
    Export* tmpl = tmpl_e->ep;
    Export* obj = obj_e->ep;
    return !(tmpl->code[0] == obj->code[0] &&
	     tmpl->code[1] == obj->code[1] &&
	     tmpl->code[2] == obj->code[2]);
}


static struct export_entry*
export_alloc(struct export_entry* tmpl_e)
{
    erl_exit(1, "Cannot alloc export entry from slave");
}

static void
export_free(struct export_entry* obj)
{
    erl_exit(1, "Cannot free export entry from slave");
}

static HashFunctions fun = {
    (H_FUN)      export_hash,
    (HCMP_FUN)   export_cmp,
    (HALLOC_FUN) export_alloc,
    (HFREE_FUN)  export_free,
};

/*
 * Return a pointer to the export entry for the given function,
 * or NULL otherwise.  Notes:
 *
 * 1) BIFs have export entries and can be called through
 *    a wrapper in the export entry.
 * 2) Functions referenced by a loaded module, but not yet loaded
 *    also have export entries.  The export entry contains
 *    a wrapper which invokes the error handler if a function is
 *    called through such an export entry.
 * 3) This function is suitable for the implementation of erlang:apply/3.
 */
extern Export* /* inline-helper */
erts_find_export_entry(Eterm m, Eterm f, unsigned int a,ErtsCodeIndex code_ix);

Export*
erts_find_export_entry(Eterm m, Eterm f, unsigned int a, ErtsCodeIndex code_ix)
{
    HashValue hval = EXPORT_HASH((BeamInstr) m, (BeamInstr) f, (BeamInstr) a);
    int ix;
    HashBucket* b;

    ix = hval % export_tables[code_ix].htable.size;
    b = export_tables[code_ix].htable.bucket[ix];

    /*
     * Note: We have inlined the code from hash.c for speed.
     */

    while (b != (HashBucket*) 0) {
	Export* ep = ((struct export_entry*) b)->ep;
	if (ep->code[0] == m && ep->code[1] == f && ep->code[2] == a) {
	    return ep;
	}
	b = b->next;
    }
    return NULL;
}

static struct export_entry* init_template(struct export_templ* templ,
					  Eterm m, Eterm f, unsigned a)
{
    templ->entry.ep = &templ->exp;
    templ->entry.slot.index = -1;
    templ->exp.code[0] = m;
    templ->exp.code[1] = f;
    templ->exp.code[2] = a;
    return &templ->entry;
}

/*
 * Find the export entry for a loaded function.
 * Returns a NULL pointer if the given function is not loaded, or
 * a pointer to the export entry.
 *
 * Note: This function never returns export entries for BIFs
 * or functions which are not yet loaded.  This makes it suitable
 * for use by the erlang:function_exported/3 BIF or whenever you
 * cannot depend on the error_handler.
 */

Export*
erts_find_function(Eterm m, Eterm f, unsigned int a, ErtsCodeIndex code_ix)
{
    struct export_templ templ;
    struct export_entry* ee;

    ee = hash_get_ext(&export_tables[code_ix].htable,
		      init_template(&templ, m, f, a), &fun);
    if (ee == NULL ||
	(ee->ep->addressv[code_ix] == ee->ep->code+3 &&
	 ee->ep->code[3] != (BeamInstr) BeamOp(op_i_generic_breakpoint))) {
	return NULL;
    }
    return ee->ep;
}
