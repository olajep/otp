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

#include "hipe_literals.h"
#define ASM
#include "hipe_epiphany_asm.h"
#include "hipe_mode_switch.h"

	.text
	.p2align 1

/*
 * Enter Erlang from C.
 * Create a new frame on the C stack.
 * Save C callee-save registers in the frame.
 * Do not clobber the C argument registers.
 * Retrieve the process pointer from the C argument registers.
 *
 * Our C frame includes:
 * - 4 bytes to pad the frame size to a multiple of 8
 * - 4 bytes for saving ARITHMODE and RMODE.
 * - 22*4 == 88 bytes for saving r4-r11, r14-r15, r32-r43
 * - 4*4 == 16 bytes for calls to hipe_bs_put_{big_integer,small_float}.
 *   They take 5-6 parameter words: 4 in registers and 1-2 on the stack.
 *   (They take 5 regular parameters, and an additional P parameter on SMP.)
 */
	.macro ENTER_FROM_C
	sub	sp,  sp, #28*4
	strd	r14, [sp, #8]
	bl	.enter
	.endm
/*
 * Note: always use the ENTER_FROM_C macro rather than directly calling .enter
 */
.enter:
	strd	r4,  [sp, #12]
	strd	r6,  [sp, #11]
	strd	r8,  [sp, #10]
	strd	r10, [sp, #9]
	strd	r32, [sp, #7]
	strd	r34, [sp, #6]
	strd	r36, [sp, #5]
	strd	r38, [sp, #4]
	strd	r40, [sp, #3]
	/* Save and set ARITHMODE */
	mov	r4,  %low(#0b100 << 17)
	/* This store was switched with the *
	 * mov above to avoid a RaW stall. */
	strd	r42, [sp, #2]
	movt	r4,  %high(#0b100 << 17)
	gid
	movfs	r5,  config
	orr	r6,  r5, r4
	movts	config, r6
	gie
	str	r5,  [sp, #26]
	mov	P,   r0
	ldr	HFV, [P, #P_HEAP_FENCE/4]
	RESTORE_CACHED_STATE
	rts

/*
 * Return to the calling C function.
 * The return value is in r0.
 *
 * .nosave_exit saves no state
 * .flush_exit saves NSP and other cached P state.
 * .suspend_exit also saves RA.
 */
.suspend_exit:
	/* save RA, so we can be resumed */
	str	lr, [P, #P_NRA/4]
.flush_exit:
	/* flush cached P state */
	SAVE_CACHED_STATE
.nosave_exit:
	/* restore RMODE&ARITHMODE, callee-save registers, drop frame, return */
	mov	r4,  %low(#~((0b111 << 17) | 1))
	ldr	r5,  [sp, #26]
	movt	r4,  %high(#~((0b111 << 17) | 1))
	gid
	movfs	r6, config
	eor	r6, r6, r5
	and	r6, r6, r4
	eor	r6, r6, r5
	movts	config, r6
	gie
	ldrd	r42, [sp, #2]
	ldrd	r40, [sp, #3]
	ldrd	r38, [sp, #4]
	ldrd	r36, [sp, #5]
	ldrd	r34, [sp, #6]
	ldrd	r32, [sp, #7]
	ldrd	r14, [sp, #8]
	ldrd	r10, [sp, #9]
	ldrd	r8,  [sp, #10]
	ldrd	r6,  [sp, #11]
	ldrd	r4,  [sp, #12]
	add	sp,  sp, #28*4
	rts

/*
 * int hipe_epiphany_call_to_native(Process *p);
 * Emulated code recursively calls native code.
 */
	.global	SYM(hipe_epiphany_call_to_native)
SYM(hipe_epiphany_call_to_native):
	ENTER_FROM_C
	/* Load the target (hoisted to avoid stall) */
	ldr	r0, [P, #P_NCALLEE/4]
	/* get argument registers */
	LOAD_ARG_REGS
	/* call the target */
	jalr	r0
/* FALLTHROUGH
 *
 * We export this return address so that hipe_mode_switch() can discover
 * when native code tailcalls emulated code.
 *
 * This is where native code returns to emulated code.
 */
	.global	SYM(nbif_return)
SYM(nbif_return):
	str	r0, [P, #P_ARG0/4]			/* save retval */
	mov	r0, #HIPE_MODE_SWITCH_RES_RETURN
	b	.flush_exit

/*
 * int hipe_epiphany_return_to_native(Process *p);
 * Emulated code returns to its native code caller.
 */
	.global	SYM(hipe_epiphany_return_to_native)
SYM(hipe_epiphany_return_to_native):
	ENTER_FROM_C
	/* restore return address */
	ldr	lr, [P, #P_NRA/4]
	/* get return value */
	ldr	r0, [P, #P_ARG0/4]
	/*
	 * Return using the current return address.
	 * The parameters were popped at the original native-to-emulated
	 * call (hipe_call_from_native_is_recursive), so a plain ret suffices.
	 */
	rts

/*
 * int hipe_epiphany_tailcall_to_native(Process *p);
 * Emulated code tailcalls native code.
 */
	.global	SYM(hipe_epiphany_tailcall_to_native)
SYM(hipe_epiphany_tailcall_to_native):
	ENTER_FROM_C
	/* Load the target (hoisted to avoid stall) */
	ldr	r0, [P, #P_NCALLEE/4]
	/* get argument registers */
	LOAD_ARG_REGS
	/* restore return address */
	ldr	lr, [P, #P_NRA/4]
	/* call the target */
	jr	r0

/*
 * int hipe_epiphany_throw_to_native(Process *p);
 * Emulated code throws an exception to its native code caller.
 */
	.global	SYM(hipe_epiphany_throw_to_native)
SYM(hipe_epiphany_throw_to_native):
	ENTER_FROM_C
	/* invoke the handler */
	ldr	r0, [P, #P_NCALLEE/4]	/* set by hipe_find_handler() */
	jr	r0

/*
 * Native code calls emulated code via a stub
 * which should look as follows:
 *
 * stub for f/N:
 *	<set r8 to f's BEAM code address>
 *	<set r0 to N>
 *	b SYM(nbif_callemu)
 *
 * XXX: Different stubs for different number of register parameters?
 */
	.global	SYM(nbif_callemu)
SYM(nbif_callemu):
	str	r8, [P, #P_BEAM_IP/4]
	str	r0, [P, #P_ARITY/4]
	STORE_ARG_REGS
	mov	r0, #HIPE_MODE_SWITCH_RES_CALL
	b	.suspend_exit

/*
 * _nbif_apply
 */
	.global	SYM(nbif_apply)
SYM(nbif_apply):
	STORE_ARG_REGS
	mov	r0, #HIPE_MODE_SWITCH_RES_APPLY
	b	.suspend_exit

/*
 * Native code calls an emulated-mode closure via a stub defined below.
 *
 * The closure is appended as the last actual parameter, and parameters
 * beyond the first few passed in registers are pushed onto the stack in
 * left-to-right order.
 * Hence, the location of the closure parameter only depends on the number
 * of parameters in registers, not the total number of parameters.
 */
#if NR_ARG_REGS >= 6
	.global	SYM(nbif_ccallemu6)
SYM(nbif_ccallemu6):
	str	ARG5, [P, #P_ARG5/4]
#if NR_ARG_REGS > 6
	mov	ARG5, ARG6
#else
	ldr	ARG5, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 5
	.global	SYM(nbif_ccallemu5)
SYM(nbif_ccallemu5):
	str	ARG4, [P, #P_ARG4/4]
#if NR_ARG_REGS > 5
	mov	ARG4, ARG5
#else
	ldr	ARG4, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 4
	.global	SYM(nbif_ccallemu4)
SYM(nbif_ccallemu4):
	str	ARG3, [P, #P_ARG3/4]
#if NR_ARG_REGS > 4
	mov	ARG3, ARG4
#else
	ldr	ARG3, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 3
	.global	SYM(nbif_ccallemu3)
SYM(nbif_ccallemu3):
	str	ARG2, [P, #P_ARG2/4]
#if NR_ARG_REGS > 3
	mov	ARG2, ARG3
#else
	ldr	ARG2, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 2
	.global	SYM(nbif_ccallemu2)
SYM(nbif_ccallemu2):
	str	ARG1, [P, #P_ARG1/4]
#if NR_ARG_REGS > 2
	mov	ARG1, ARG2
#else
	ldr	ARG1, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

#if NR_ARG_REGS >= 1
	.global	SYM(nbif_ccallemu1)
SYM(nbif_ccallemu1):
	str	ARG0, [P, #P_ARG0/4]
#if NR_ARG_REGS > 1
	mov	ARG0, ARG1
#else
	ldr	ARG0, [NSP, #0]
#endif
	/*FALLTHROUGH*/
#endif

	.global	SYM(nbif_ccallemu0)
SYM(nbif_ccallemu0):
	/* We use r1 not ARG0 here because ARG0 is not
	   defined when NR_ARG_REGS == 0. */
#if NR_ARG_REGS == 0
	ldr	r1, [NSP, #0]		/* get the closure */
#endif
	str	r1, [P, #P_CLOSURE/4]	/* save the closure */
	mov	r0, #HIPE_MODE_SWITCH_RES_CALL_CLOSURE
	b	.suspend_exit

/*
 * This is where native code suspends.
 */
	.global	SYM(nbif_suspend_0)
SYM(nbif_suspend_0):
	mov	r0, #HIPE_MODE_SWITCH_RES_SUSPEND
	b	.suspend_exit

/*
 * Suspend from a receive (waiting for a message)
 */
	.global	SYM(nbif_suspend_msg)
SYM(nbif_suspend_msg):
	mov	r0, #HIPE_MODE_SWITCH_RES_WAIT
	b	.suspend_exit

/*
 * Suspend from a receive with a timeout (waiting for a message)
 *	if (!(p->flags & F_TIMO)) { suspend }
 *	else { return 0; }
 */
	.global	SYM(nbif_suspend_msg_timeout)
SYM(nbif_suspend_msg_timeout):
	ldr	r1, [P, #P_FLAGS/4]
	/* this relies on F_TIMO (1<<2) fitting in a uimm16 */
	mov	r2, #F_TIMO
	mov	r0, #HIPE_MODE_SWITCH_RES_WAIT_TIMEOUT
	and	r1, r1, r2
	beq	.suspend_exit
	/* timeout has occurred */
	mov	r0, #0
	rts

/*
 * This is the default exception handler for native code.
 */
	.global	SYM(nbif_fail)
SYM(nbif_fail):
	mov	r0, #HIPE_MODE_SWITCH_RES_THROW
	b	.flush_exit	/* no need to save RA */

	.global	SYM(nbif_0_gc_after_bif)
	.global	SYM(nbif_1_gc_after_bif)
	.global	SYM(nbif_2_gc_after_bif)
	.global	SYM(nbif_3_gc_after_bif)
SYM(nbif_0_gc_after_bif):
	mov	r1, #0
	b	.gc_after_bif
SYM(nbif_1_gc_after_bif):
	mov	r1, #1
	b	.gc_after_bif
SYM(nbif_2_gc_after_bif):
	mov	r1, #2
	b	.gc_after_bif
SYM(nbif_3_gc_after_bif):
	mov	r1, #3
	/*FALLTHROUGH*/
.gc_after_bif:
	str	r1, [P, #P_NARITY/4]
	str	TEMP_LR, [P, #P_NRA/4]
	str	NSP, [P, #P_NSP/4]
	mov	TEMP_LR, lr
	mov	r3, #0			/* Pass 0 in arity */
	mov	r2, #0			/* Pass NULL in regs */
	mov	r1, r0
	mov	r0, P
	bl	SYM(erts_gc_after_bif_call)
	mov	lr, TEMP_LR
	mov	r1, #0
	ldr	TEMP_LR, [P, #P_NRA/4]
	str	r1, [P, #P_NARITY/4]
	rts

/*
 * We end up here when a BIF called from native signals an
 * exceptional condition.
 * HP was just read from P.
 * NSP has not been saved in P.
 * TEMP_LR contains a copy of LR
 */
	.global	SYM(nbif_0_simple_exception)
SYM(nbif_0_simple_exception):
	mov	r1, #0
	b	.nbif_simple_exception
	.global	SYM(nbif_1_simple_exception)
SYM(nbif_1_simple_exception):
	mov	r1, #1
	b	.nbif_simple_exception
	.global	SYM(nbif_2_simple_exception)
SYM(nbif_2_simple_exception):
	mov	r1, #2
	b	.nbif_simple_exception
	.global	SYM(nbif_3_simple_exception)
SYM(nbif_3_simple_exception):
	mov	r1, #3
	/*FALLTHROUGH*/
.nbif_simple_exception:
	ldr	r0, [P, #P_FREASON/4]
	sub	r0, r0, #FREASON_TRAP
	beq	.handle_trap
	/*
	 * Find and invoke catch handler (it must exist).
	 * HP was just read from P.
	 * NSP has not been saved in P.
	 * TEMP_LR should contain the current call's return address.
	 * r1 should contain the current call's arity.
	 */
	str	NSP, [P, #P_NSP/4]
	str	TEMP_LR, [P, #P_NRA/4]
	str	r1, [P, #P_NARITY/4]
	/* find and prepare to invoke the handler */
	mov	r0, P
	bl	SYM(hipe_handle_exception)	/* Note: hipe_handle_exception() conses */
	ldr	r0, [P, #P_NCALLEE/4]	/* set by hipe_find_handler() */
	RESTORE_CACHED_STATE		/* NSP updated by hipe_find_handler() */
	/* now invoke the handler */
	jr	r0

	/*
	 * A BIF failed with freason TRAP:
	 * - the BIF's arity is in r1
	 * - the native RA was saved in TEMP_LR before the BIF call
	 * - HP was just read from P
	 * - NSP has not been saved in P
	 */
.handle_trap:
	mov	r0, #HIPE_MODE_SWITCH_RES_TRAP
	str	NSP, [P, #P_NSP/4]
	str	r1, [P, #P_NARITY/4]
	str	TEMP_LR, [P, #P_NRA/4]
	b	.nosave_exit

/*
 * _nbif_stack_trap_ra: trap return address for maintaining
 * the gray/white stack boundary
 */
	.global	SYM(nbif_stack_trap_ra)
SYM(nbif_stack_trap_ra):	/* a return address, not a function */
	# This only handles a single return value.
	# If we have more, we need to save them in the PCB.
	mov	TEMP_ARG0, r0		/* save retval */
	str	NSP, [P, #P_NSP/4]
	mov	r0, P
	bl	SYM(hipe_handle_stack_trap)	/* must not cons */
	mov	lr, r0			/* original RA */
	mov	r0, TEMP_ARG0		/* restore retval */
	rts

/*
 * hipe_epiphany_inc_stack
 * Caller saved its LR in TEMP_LR (== TEMP1) before calling us.
 */
	.global	SYM(hipe_epiphany_inc_stack)
SYM(hipe_epiphany_inc_stack):
	STORE_ARG_REGS
	mov	TEMP_ARG0, lr
	str	NSP, [P, #P_NSP/4]
	mov	r0, P
	# hipe_inc_nstack reads and writes NSP and NSP_LIMIT,
	# but does not access LR/RA, HP, or FCALLS.
	bl	SYM(hipe_inc_nstack)
	ldr	NSP, [P, #P_NSP/4]
	LOAD_ARG_REGS
	# this relies on LOAD_ARG_REGS not clobbering TEMP_ARG0
	jr	TEMP_ARG0

#if defined(__linux__) && defined(__ELF__)
.section .note.GNU-stack,"",%progbits
#endif
