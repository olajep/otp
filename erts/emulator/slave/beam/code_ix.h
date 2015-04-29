/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 2012-2013. All Rights Reserved.
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

/* Description:
 *	Code indices are rotated by the master emulator poking our address
 *	space.
 */

#ifndef __CODE_IX_H__
#define __CODE_IX_H__

#ifndef __SYS_H__
#  ifdef HAVE_CONFIG_H
#    include "config.h"
#  endif
#  include "sys.h"
#endif

#define ERTS_NUM_CODE_IX 3
typedef unsigned ErtsCodeIndex;


/* Return active code index.
 * Is guaranteed to be valid until the calling BIF returns.
 * To get a consistent view of the code, only one call to erts_active_code_ix()
 * should be made and the returned ix reused within the same BIF call.
 */
ERTS_GLB_INLINE
ErtsCodeIndex erts_active_code_ix(void);

/* Return staging code ix.
 * Only used by a process performing code loading/upgrading/deleting/purging.
 * Code write permission must be seized.
 */
ERTS_GLB_INLINE
ErtsCodeIndex erts_staging_code_ix(void);

#if ERTS_GLB_INLINE_INCL_FUNC_DEF

ErtsCodeIndex the_active_code_index;
ErtsCodeIndex the_staging_code_index;

ERTS_GLB_INLINE ErtsCodeIndex erts_active_code_ix(void)
{
    /* Assumes that this load is atomic */
    return the_active_code_index;
}
ERTS_GLB_INLINE ErtsCodeIndex erts_staging_code_ix(void)
{
    /* Assumes that this load is atomic */
    return the_staging_code_index;
}

#endif /* ERTS_GLB_INLINE_INCL_FUNC_DEF */

#endif /* !__CODE_IX_H__ */
