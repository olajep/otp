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
