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

#define HIPE_EPIPHANY_C_SLAVE_MODE
#include "hipe_epiphany.c"

#include "slave_syms.h"

#undef THE_NON_VALUE
#undef F_TIMO
#undef ERL_FUN_SIZE
#include "hipe_literals.h"

extern const void *
hipe_slave_closure_stub_address(unsigned int arity) {
#if EPIPHANY_NR_ARG_REGS == 0
    return (const void*) SLAVE_SYM_nbif_ccallemu0;
#else	/* > 0 */
    switch (arity) {
      case 0:	return (const void*) SLAVE_SYM_nbif_ccallemu0;
#if EPIPHANY_NR_ARG_REGS == 1
      default:	return (const void*) SLAVE_SYM_nbif_ccallemu1;
#else	/* > 1 */
      case 1:	return (const void*) SLAVE_SYM_nbif_ccallemu1;
#if EPIPHANY_NR_ARG_REGS == 2
      default:	return (const void*) SLAVE_SYM_nbif_ccallemu2;
#else	/* > 2 */
      case 2:	return (const void*) SLAVE_SYM_nbif_ccallemu2;
#if EPIPHANY_NR_ARG_REGS == 3
      default:	return (const void*) SLAVE_SYM_nbif_ccallemu3;
#else	/* > 3 */
      case 3:	return (const void*) SLAVE_SYM_nbif_ccallemu3;
#if EPIPHANY_NR_ARG_REGS == 4
      default:	return (const void*) SLAVE_SYM_nbif_ccallemu4;
#else	/* > 4 */
      case 4:	return (const void*) SLAVE_SYM_nbif_ccallemu4;
#if EPIPHANY_NR_ARG_REGS == 5
      default:	return (const void*) SLAVE_SYM_nbif_ccallemu5;
#else	/* > 5 */
      case 5:	return (const void*) SLAVE_SYM_nbif_ccallemu5;
#if EPIPHANY_NR_ARG_REGS == 6
      default:	return (const void*) SLAVE_SYM_nbif_ccallemu6;
#else
#error "EPIPHANY_NR_ARG_REGS > 6 NOT YET IMPLEMENTED"
#endif	/* > 6 */
#endif	/* > 5 */
#endif	/* > 4 */
#endif	/* > 3 */
#endif	/* > 2 */
#endif	/* > 1 */
    }
#endif	/* > 0 */
}
