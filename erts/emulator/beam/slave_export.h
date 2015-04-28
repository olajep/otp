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

#ifndef __SLAVE_EXPORT_H__
#define __SLAVE_EXPORT_H__

#ifndef __SYS_H__
#include "sys.h"
#endif

#ifndef __INDEX_H__
#include "index.h"
#endif

#include "code_ix.h"

typedef struct export Export;

void slave_init_export_table(void);
/* void export_info(int, void *); */

ERTS_GLB_INLINE Export* slave_active_export_entry(Eterm m, Eterm f, unsigned a);
Export* slave_export_put(Eterm mod, Eterm func, unsigned int arity);

Export* slave_export_get_or_make_stub(Eterm, Eterm, unsigned);

/* Export *export_list(int,ErtsCodeIndex); */
/* int export_list_size(ErtsCodeIndex); */
/* int export_table_sz(void); */
/* int export_entries_sz(void); */
/* Export *export_get(Export*); */
void slave_export_start_staging(void);
void slave_export_end_staging(int commit);

extern erts_smp_mtx_t slave_export_staging_lock;
#define slave_export_staging_lock()	erts_smp_mtx_lock(&slave_export_staging_lock)
#define slave_export_staging_unlock()	erts_smp_mtx_unlock(&slave_export_staging_lock)

#include "beam_load.h" /* For em_* extern declarations */
#define SlaveExportIsBuiltIn(EntryPtr) 			\
(((EntryPtr)->addressv[erts_active_code_ix()] == (EntryPtr)->code + 3) && \
 ((EntryPtr)->code[3] == (BeamInstr) SlaveOp(op_em_apply_bif)))

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ERTS_GLB_INLINE Export*
slave_active_export_entry(Eterm m, Eterm f, unsigned int a)
{
    extern Export* slave_find_export_entry(Eterm m, Eterm f, unsigned a, ErtsCodeIndex);
    return slave_find_export_entry(m, f, a, erts_active_code_ix());
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* __SLAVE_EXPORT_H__ */
