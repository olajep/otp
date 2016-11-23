changecom(`/*', `*/')dnl
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


`#ifndef HIPE_EPIPHANY_ASM_H
#define HIPE_EPIPHANY_ASM_H'

`#ifdef __ASSEMBLER__'
/* We need the literals already for the context switching macros. */
`#  include' "hipe_literals.h"
`#endif'

/* ANSI concatenation macros.  */
#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b
/* Use the right prefix for global labels.  */
#define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)

/*
 * Tunables.
 */
define(LEAF_WORDS,16)dnl number of stack words for leaf functions
define(NR_ARG_REGS,5)dnl admissible values are 0 to 8, inclusive
define(NR_RET_REGS,4)dnl admissible values are 0 to 4, inclusive

`#define EPIPHANY_LEAF_WORDS	'LEAF_WORDS

/*
 * Reserved registers.
 */
`#define P	r32'
`#define NSP	r6'
`#define HP	r7'
`#define TEMP_LR	r8'
`#define HFV	r33'

/*
 * Context switching macros.
 *
 * Note: We *must not* use #define for multi-statement macros, since ';' starts
 *       comments rather than separates statements in Epiphany
 *       assembler. Failure to do this will result in macros expanding to their
 *       first instruction only!
 *
 * RESTORE_CONTEXT and RESTORE_CONTEXT_QUICK do not affect
 * the condition register.
 */
`#ifdef __ASSEMBLER__'
`       .macro SAVE_CONTEXT_QUICK
        mov	TEMP_LR, lr
        .endm'

`       .macro RESTORE_CONTEXT_QUICK
        mov	lr, TEMP_LR
        .endm'

`       .macro SAVE_CACHED_STATE
        str	HP, [P, #P_HP/4]
        str	NSP, [P, #P_NSP/4]
        .endm'

`       .macro RESTORE_CACHED_STATE
        ldr	HP, [P, #P_HP/4]
        ldr	NSP, [P, #P_NSP/4]
        .endm'

`       .macro SAVE_CONTEXT_BIF
        mov	TEMP_LR, lr
        str	HP, [P, #P_HP/4]
        .endm'

`       .macro RESTORE_CONTEXT_BIF
        ldr	HP, [P, #P_HP/4]
        .endm'

`       .macro SAVE_CONTEXT_GC
        mov	TEMP_LR, lr
        str	lr, [P, #P_NRA/4]
        str	NSP, [P, #P_NSP/4]
        str	HP, [P, #P_HP/4]
        .endm'

`       .macro RESTORE_CONTEXT_GC
        ldr	HP, [P, #P_HP/4]
        .endm'

`#endif /* __ASSEMBLER__ */'

/*
 * Argument (parameter) registers.
 */
`#define EPIPHANY_NR_ARG_REGS	'NR_ARG_REGS
`#define NR_ARG_REGS	'NR_ARG_REGS

/*
 * Return value registers.
 */
`#define EPIPHANY_NR_RET_REGS	'NR_RET_REGS
`#define NR_RET_REGS	'NR_RET_REGS

define(defarg,`define(ARG$1,`$2')dnl
#`define ARG'$1	$2'
)dnl

ifelse(eval(NR_ARG_REGS >= 1),0,,
`defarg(0,`r1')')dnl
ifelse(eval(NR_ARG_REGS >= 2),0,,
`defarg(1,`r2')')dnl
ifelse(eval(NR_ARG_REGS >= 3),0,,
`defarg(2,`r3')')dnl
ifelse(eval(NR_ARG_REGS >= 4),0,,
`defarg(3,`r4')')dnl
ifelse(eval(NR_ARG_REGS >= 5),0,,
`defarg(4,`r5')')dnl
ifelse(eval(NR_ARG_REGS >= 6),0,,
`defarg(5,`r16')')dnl
ifelse(eval(NR_ARG_REGS >= 7),0,,
`defarg(6,`r17')')dnl
ifelse(eval(NR_ARG_REGS >= 8),0,,
`defarg(7,`r18')')dnl

/*
 * TEMP_ARG0:
 *	Used in nbif_stack_trap_ra to preserve the return value.
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the return path.
 *
 * TEMP_ARG0:
 *	Used in hipe_epiphany_inc_stack to preserve the return address
 *	(TEMP_LR contains the caller's saved return address).
 *	Must be a C callee-save register.
 *	Must be otherwise unused in the call path.
 */
`#define TEMP_ARG0	r9'

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_epiphany_glue.S support		X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

`#ifdef __ASSEMBLER__'
dnl
dnl LOAD_ARG_REGS
dnl
define(LAR_1,`       ldr ARG$1, [P, #P_ARG$1/4]
')dnl
define(LAR_N,`ifelse(eval($1 >= 0),0,,`LAR_N(eval($1-1))LAR_1($1)')')dnl
define(LOAD_ARG_REGS,`LAR_N(eval(NR_ARG_REGS-1))')dnl
`       .macro LOAD_ARG_REGS'
LOAD_ARG_REGS`       .endm'

dnl
dnl STORE_ARG_REGS
dnl
define(SAR_1,`       str ARG$1, [P, #P_ARG$1/4]
')dnl
define(SAR_N,`ifelse(eval($1 >= 0),0,,`SAR_N(eval($1-1))SAR_1($1)')')dnl
define(STORE_ARG_REGS,`SAR_N(eval(NR_ARG_REGS-1))')dnl
`       .macro STORE_ARG_REGS'
STORE_ARG_REGS`       .endm'

`#endif /* __ASSEMBLER__ */'

dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
dnl X								X
dnl X			hipe_epiphany_bifs.m4 support		X
dnl X								X
dnl XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

dnl
dnl NBIF_ARG(DST,ARITY,ARGNO)
dnl Access a formal parameter.
dnl It will be a memory load via NSP when ARGNO >= NR_ARG_REGS.
dnl It will be a register move when 0 <= ARGNO < NR_ARG_REGS; if
dnl the source and destination are the same, the move is suppressed.
dnl
define(NBIF_MOVE_REG,`ifelse($1,$2,`# mov	$1, $2',`mov	$1, $2')')dnl
define(NBIF_REG_ARG,`NBIF_MOVE_REG($1,ARG$2)')dnl
define(NBIF_STK_LOAD,`ldr	$1, [NSP, #$2]')dnl
dnl Note well the omission of 4*(...) below; it removes the need for .../4 above
define(NBIF_STK_ARG,`NBIF_STK_LOAD($1,eval(($2-$3)-1))')dnl
define(NBIF_ARG,`ifelse(eval($3 >= NR_ARG_REGS),0,`NBIF_REG_ARG($1,$3)',`NBIF_STK_ARG($1,$2,$3)')')dnl
`/* #define NBIF_ARG_1_0	'NBIF_ARG(r1,1,0)` */'
`/* #define NBIF_ARG_2_0	'NBIF_ARG(r1,2,0)` */'
`/* #define NBIF_ARG_2_1	'NBIF_ARG(r2,2,1)` */'
`/* #define NBIF_ARG_3_0	'NBIF_ARG(r1,3,0)` */'
`/* #define NBIF_ARG_3_1	'NBIF_ARG(r2,3,1)` */'
`/* #define NBIF_ARG_3_2	'NBIF_ARG(r3,3,2)` */'
`/* #define NBIF_ARG_5_0	'NBIF_ARG(r1,5,0)` */'
`/* #define NBIF_ARG_5_1	'NBIF_ARG(r2,5,1)` */'
`/* #define NBIF_ARG_5_2	'NBIF_ARG(r3,5,2)` */'
`/* #define NBIF_ARG_5_3	'NBIF_ARG(r4,5,3)` */'
`/* #define NBIF_ARG_5_4	'NBIF_ARG(r5,5,4)` */'

dnl
dnl NBIF_RET(ARITY)
dnl Generates a return from a native BIF, taking care to pop
dnl any stacked formal parameters.
dnl May only be used in BIF/primop wrappers where SAVE_CONTEXT
dnl has saved LR in TEMP_LR.
dnl
define(NSP_RETN,`add	NSP, NSP, #$1
        jr	TEMP_LR')dnl
define(NSP_RET0,`jr	TEMP_LR')dnl
define(RET_POP,`ifelse(eval($1 > NR_ARG_REGS),0,0,eval(4*($1 - NR_ARG_REGS)))')dnl
define(NBIF_RET_N,`ifelse(eval($1),0,`NSP_RET0',`NSP_RETN($1)')')dnl
define(NBIF_RET,`NBIF_RET_N(eval(RET_POP($1)))')dnl
`/* #define NBIF_RET_0	'NBIF_RET(0)` */'
`/* #define NBIF_RET_1	'NBIF_RET(1)` */'
`/* #define NBIF_RET_2	'NBIF_RET(2)` */'
`/* #define NBIF_RET_3	'NBIF_RET(3)` */'
`/* #define NBIF_RET_5	'NBIF_RET(5)` */'

dnl
dnl QUICK_CALL_RET(CFUN,ARITY)
dnl Used in nocons_nofail and noproc primop interfaces to optimise
dnl      SAVE_CONTEXT_QUICK; bl CFUN; RESTORE_CONTEXT_QUICK; NBIF_RET(ARITY).
dnl
define(NBIF_POP_N,`ifelse(eval($1),0,`',`add NSP, NSP, #$1
')')dnl
define(QUICK_CALL_RET,`NBIF_POP_N(eval(RET_POP($2)))b $1')dnl
`/* #define QUICK_CALL_RET_F_0 'QUICK_CALL_RET(F,0)` */'
`/* #define QUICK_CALL_RET_F_1 'QUICK_CALL_RET(F,1)` */'
`/* #define QUICK_CALL_RET_F_2 'QUICK_CALL_RET(F,2)` */'
`/* #define QUICK_CALL_RET_F_3 'QUICK_CALL_RET(F,3)` */'
`/* #define QUICK_CALL_RET_F_5 'QUICK_CALL_RET(F,5)` */'

`#endif /* HIPE_EPIPHANY_ASM_H */'
