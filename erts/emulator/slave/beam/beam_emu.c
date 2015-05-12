/*
 * %CopyrightBegin%
 *
 * Copyright Ericsson AB 1996-2014. All Rights Reserved.
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

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"
#include "error.h"
#include "bif.h"
#include "big.h"
#include "beam_load.h"
#include "erl_binary.h"
#include "erl_map.h"
#include "erl_bits.h"
#include "dist.h"
#include "beam_bp.h"
#include "beam_catches.h"
#include "erl_thr_progress.h"
#ifdef HIPE
#include "hipe_mode_switch.h"
#include "hipe_bif1.h"
#endif
#include "dtrace-wrapper.h"

#ifdef DEBUG
// For sanity-checking pointers
#include "epiphany.h"
#endif

/* #define HARDDEBUG 1 */

#if defined(NO_JUMP_TABLE)
#  define OpCase(OpCode)    case op_##OpCode
#  define CountCase(OpCode) case op_count_##OpCode
#  define OpCode(OpCode)    ((Uint*)op_##OpCode)
#  define Goto(Rel) {Go = (int)(UWord)(Rel); goto emulator_loop;}
#  define LabelAddr(Addr) &&##Addr
#else
#  define OpCase(OpCode)    lb_##OpCode
#  define CountCase(OpCode) lb_count_##OpCode
#  define Goto(Rel) goto *((void *)Rel)
#  define LabelAddr(Label) &&Label
#  define OpCode(OpCode)  (&&lb_##OpCode)
#endif

/* Instruction-by-instruction tracing */
#define HARDTRACE 0
#if HARDTRACE
#  undef OpCase
#  define OpCase(OpCode) {			\
    lb_##OpCode:				\
	erts_printf(" Trace @ " #OpCode "\n");	\
	goto __lb_##OpCode##_no_print_;		\
    } __lb_##OpCode##_no_print_
#endif

#ifdef ERTS_ENABLE_LOCK_CHECK
#  ifdef ERTS_SMP
#    define PROCESS_MAIN_CHK_LOCKS(P)					\
do {									\
    if ((P)) {								\
	erts_proc_lc_chk_only_proc_main((P));				\
    }									\
    else								\
	erts_lc_check_exact(NULL, 0);					\
    	ERTS_SMP_LC_ASSERT(!erts_thr_progress_is_blocking());		\
} while (0)
#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P) \
        if ((P)) erts_proc_lc_require_lock((P), ERTS_PROC_LOCK_MAIN,\
					   __FILE__, __LINE__)
#    define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P) \
        if ((P)) erts_proc_lc_unrequire_lock((P), ERTS_PROC_LOCK_MAIN)
#  else
#    define ERTS_SMP_REQ_PROC_MAIN_LOCK(P)
#    define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P)
#    define PROCESS_MAIN_CHK_LOCKS(P) erts_lc_check_exact(NULL, 0)
#  endif
#else
#  define PROCESS_MAIN_CHK_LOCKS(P)
#  define ERTS_SMP_REQ_PROC_MAIN_LOCK(P)
#  define ERTS_SMP_UNREQ_PROC_MAIN_LOCK(P)
#endif

/*
 * Define macros for deep checking of terms.
 */

#if defined(HARDDEBUG)

#  define CHECK_TERM(T) size_object(T)

#  define CHECK_ARGS(PC)                 \
do {                                     \
  int i_;                                \
  int Arity_ = PC[-1];                   \
  if (Arity_ > 0) {                      \
	CHECK_TERM(r(0));                \
  }                                      \
  for (i_ = 1; i_ < Arity_; i_++) {      \
	CHECK_TERM(x(i_));               \
  }                                      \
} while (0)
    
#else
#  define CHECK_TERM(T) ASSERT(!is_CP(T))
#  define CHECK_ARGS(T)
#endif

#ifndef MAX
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#endif

#define GET_BIF_ADDRESS(p) ((BifFunction) (((Export *) p)->code[4]))
#define TermWords(t) (((t) / (sizeof(BeamInstr)/sizeof(Eterm))) + !!((t) % (sizeof(BeamInstr)/sizeof(Eterm))))


/*
 * We reuse some of fields in the save area in the process structure.
 * This is safe to do, since this space is only activly used when
 * the process is switched out.
 */
#define REDS_IN(p)  ((p)->def_arg_reg[5])

/*
 * Add a byte offset to a pointer to Eterm.  This is useful when the
 * the loader has precalculated a byte offset.
 */
#define ADD_BYTE_OFFSET(ptr, offset) \
   ((Eterm *) (((unsigned char *)ptr) + (offset)))

/* We don't check the range if an ordinary switch is used */
#ifdef NO_JUMP_TABLE
#define VALID_INSTR(IP) ((UWord)(IP) < (NUMBER_OF_OPCODES*2+10))
#else
#define VALID_INSTR(IP) \
   ((SWord)LabelAddr(emulator_loop) <= (SWord)(IP) && \
    (SWord)(IP) < (SWord)LabelAddr(end_emulator_loop))
#endif /* NO_JUMP_TABLE */

#define SET_CP(p, ip)           \
   ASSERT(VALID_INSTR(*(ip)));  \
   (p)->cp = (ip)

#define SET_I(ip) \
   ASSERT(VALID_INSTR(* (Eterm *)(ip))); \
   I = (ip)

#define FetchArgs(S1, S2) tmp_arg1 = (S1); tmp_arg2 = (S2)

/*
 * Store a result into a register given a destination descriptor.
 */

#define StoreResult(Result, DestDesc)               \
  do {                                              \
    Eterm stb_reg;                                  \
    stb_reg = (DestDesc);                           \
    CHECK_TERM(Result);                             \
    switch (beam_reg_tag(stb_reg)) {                \
    case R_REG_DEF:                                 \
      r(0) = (Result); break;                       \
    case X_REG_DEF:                                 \
      xb(x_reg_offset(stb_reg)) = (Result); break;  \
    default:                                        \
      yb(y_reg_offset(stb_reg)) = (Result); break;  \
    }                                               \
  } while (0)

#define StoreSimpleDest(Src, Dest) Dest = (Src)

/*
 * Store a result into a register and execute the next instruction.
 * Dst points to the word with a destination descriptor, which MUST
 * be just before the next instruction.
 */
 
#define StoreBifResult(Dst, Result)                          \
  do {                                                       \
    BeamInstr* stb_next;                                         \
    Eterm stb_reg;                                           \
    stb_reg = Arg(Dst);                                      \
    I += (Dst) + 2;                                          \
    stb_next = (BeamInstr *) *I;                                 \
    CHECK_TERM(Result);                                      \
    switch (beam_reg_tag(stb_reg)) {                         \
    case R_REG_DEF:                                          \
      r(0) = (Result); Goto(stb_next);                       \
    case X_REG_DEF:                                          \
      xb(x_reg_offset(stb_reg)) = (Result); Goto(stb_next);  \
    default:                                                 \
      yb(y_reg_offset(stb_reg)) = (Result); Goto(stb_next);  \
    }                                                        \
  } while (0)

#define ClauseFail() goto jump_f

#define SAVE_CP(X)				\
   do {						\
      *(X) = make_cp(c_p->cp);			\
      c_p->cp = 0;				\
   } while(0)

#define RESTORE_CP(X)		SET_CP(c_p, (BeamInstr *) cp_val(*(X)))

#define ISCATCHEND(instr) ((Eterm *) *(instr) == OpCode(catch_end_y))

/*
 * Special Beam instructions.
 */

BeamInstr beam_apply[2];
BeamInstr beam_exit[1];
BeamInstr beam_continue_exit[1];

BeamInstr* em_call_error_handler;
BeamInstr* em_apply_bif;
BeamInstr* em_call_nif;


/* NOTE These should be the only variables containing trace instructions.
**      Sometimes tests are form the instruction value, and sometimes
**      for the refering variable (one of these), and rouge references
**      will most likely cause chaos.
*/
BeamInstr beam_return_to_trace[1];   /* OpCode(i_return_to_trace) */
BeamInstr beam_return_trace[1];      /* OpCode(i_return_trace) */
BeamInstr beam_exception_trace[1];   /* UGLY also OpCode(i_return_trace) */
BeamInstr beam_return_time_trace[1]; /* OpCode(i_return_time_trace) */


/*
 * All Beam instructions in numerical order.
 */

#ifndef NO_JUMP_TABLE
void** beam_ops;
#endif

#ifndef ERTS_SMP /* Not supported with smp emulator */
extern int count_instructions;
#endif

#define SWAPIN             \
    HTOP = HEAP_TOP(c_p);  \
    E = c_p->stop

#define SWAPOUT            \
    HEAP_TOP(c_p) = HTOP;  \
    c_p->stop = E

/*
 * Use LIGHT_SWAPOUT when the called function
 * will call HeapOnlyAlloc() (and never HAlloc()).
 */
#ifdef DEBUG
#  /* The stack pointer is used in an assertion. */
#  define LIGHT_SWAPOUT SWAPOUT
#else
#  define LIGHT_SWAPOUT HEAP_TOP(c_p) = HTOP
#endif

/*
 * Use LIGHT_SWAPIN when we know that c_p->stop cannot
 * have been updated (i.e. if there cannot have been
 * a garbage-collection).
 */

#define LIGHT_SWAPIN HTOP = HEAP_TOP(c_p)

#ifdef FORCE_HEAP_FRAGS
#  define HEAP_SPACE_VERIFIED(Words) do { \
      c_p->space_verified = (Words);	  \
      c_p->space_verified_from = HTOP;	  \
    }while(0)
#else
#  define HEAP_SPACE_VERIFIED(Words) ((void)0)
#endif

#define PRE_BIF_SWAPOUT(P)						\
     HEAP_TOP((P)) = HTOP;  						\
     (P)->stop = E;  							\
     PROCESS_MAIN_CHK_LOCKS((P));					\
     ERTS_SMP_UNREQ_PROC_MAIN_LOCK((P))

#define db(N) (N)
#define tb(N) (N)
#define xb(N) (*(Eterm *) (((unsigned char *)reg) + (N)))
#define yb(N) (*(Eterm *) (((unsigned char *)E) + (N)))
#define fb(N) (*(double *) (((unsigned char *)&(freg[0].fd)) + (N)))
#define Qb(N) (N)
#define Ib(N) (N)
#define x(N) reg[N]
#define y(N) E[N]
#define r(N) x##N

/*
 * Makes sure that there are StackNeed + HeapNeed + 1 words available
 * on the combined heap/stack segment, then allocates StackNeed + 1
 * words on the stack and saves CP.
 *
 * M is number of live registers to preserve during garbage collection
 */

#define AH(StackNeed, HeapNeed, M) \
  do { \
     int needed; \
     needed = (StackNeed) + 1; \
     if (E - HTOP < (needed + (HeapNeed))) { \
           SWAPOUT; \
           reg[0] = r(0); \
           PROCESS_MAIN_CHK_LOCKS(c_p); \
           FCALLS -= erts_garbage_collect(c_p, needed + (HeapNeed), reg, (M)); \
           ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p); \
           PROCESS_MAIN_CHK_LOCKS(c_p); \
           r(0) = reg[0]; \
           SWAPIN; \
     } \
     E -= needed; \
     SAVE_CP(E); \
  } while (0)

#define Allocate(Ns, Live) AH(Ns, 0, Live)

#define AllocateZero(Ns, Live)             \
 do { Eterm* ptr;                          \
      int i = (Ns);                        \
      AH(i, 0, Live);                      \
      for (ptr = E + i; ptr > E; ptr--) {  \
	 make_blank(*ptr);                 \
     }                                     \
  } while (0)

#define AllocateHeap(Ns, Nh, Live) AH(Ns, Nh, Live)

#define AllocateHeapZero(Ns, Nh, Live)     \
 do { Eterm* ptr;                          \
      int i = (Ns);                        \
      AH(i, Nh, Live);                     \
      for (ptr = E + i; ptr > E; ptr--) {  \
	 make_blank(*ptr);                 \
     }                                     \
  } while (0)

#define AllocateInit(Ns, Live, Y) \
  do { AH(Ns, 0, Live); make_blank(Y); } while (0)

/*
 * Like the AH macro, but allocates no additional heap space.
 */

#define A(StackNeed, M) AH(StackNeed, 0, M)

#define D(N)             \
     RESTORE_CP(E);      \
     E += (N) + 1;



#define TestBinVHeap(VNh, Nh, Live)                             		\
  do {                                                          		\
    unsigned need = (Nh);                                       		\
    if ((E - HTOP < need) || (MSO(c_p).overhead + (VNh) >= BIN_VHEAP_SZ(c_p))) {\
       SWAPOUT;                                                 		\
       reg[0] = r(0);                                           		\
       PROCESS_MAIN_CHK_LOCKS(c_p);                             		\
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live));  		\
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);					\
       PROCESS_MAIN_CHK_LOCKS(c_p);                             		\
       r(0) = reg[0];                                           		\
       SWAPIN;                                                  		\
    }                                                           		\
    HEAP_SPACE_VERIFIED(need);                                                  \
  } while (0)



/*
 * Check if Nh words of heap are available; if not, do a garbage collection.
 * Live is number of active argument registers to be preserved.
 */

#define TestHeap(Nh, Live)                                      \
  do {                                                          \
    unsigned need = (Nh);                                       \
    if (E - HTOP < need) {                                      \
       SWAPOUT;                                                 \
       reg[0] = r(0);                                           \
       PROCESS_MAIN_CHK_LOCKS(c_p);                             \
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live));  \
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);			\
       PROCESS_MAIN_CHK_LOCKS(c_p);                             \
       r(0) = reg[0];                                           \
       SWAPIN;                                                  \
    }                                                           \
    HEAP_SPACE_VERIFIED(need);                             \
  } while (0)

/*
 * Check if Nh words of heap are available; if not, do a garbage collection.
 * Live is number of active argument registers to be preserved.
 * Takes special care to preserve Extra if a garbage collection occurs.
 */

#define TestHeapPreserve(Nh, Live, Extra)				\
  do {									\
    unsigned need = (Nh);						\
    if (E - HTOP < need) {						\
       SWAPOUT;								\
       reg[0] = r(0);							\
       reg[Live] = Extra;						\
       PROCESS_MAIN_CHK_LOCKS(c_p);					\
       FCALLS -= erts_garbage_collect(c_p, need, reg, (Live)+1);	\
       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);				\
       PROCESS_MAIN_CHK_LOCKS(c_p);					\
       if (Live > 0) {							\
	   r(0) = reg[0];						\
       }								\
       Extra = reg[Live];						\
       SWAPIN;								\
    }									\
    HEAP_SPACE_VERIFIED(need);                                      \
  } while (0)

#define TestHeapPutList(Need, Reg)		\
  do {						\
     TestHeap((Need), 1);			\
     PutList(Reg, r(0), r(0), StoreSimpleDest);	\
     CHECK_TERM(r(0));				\
  } while (0)

#define Init(N) make_blank(yb(N))

#define Init2(Y1, Y2) do { make_blank(Y1); make_blank(Y2); } while (0)
#define Init3(Y1, Y2, Y3) \
   do { make_blank(Y1); make_blank(Y2); make_blank(Y3); } while (0)

#define MakeFun(FunP, NumFree)					\
  do {								\
     SWAPOUT;							\
     reg[0] = r(0);						\
     r(0) = new_fun(c_p, reg, (ErlFunEntry *) FunP, NumFree);	\
     SWAPIN;							\
  } while (0)

#define PutTuple(Dst, Arity)			\
 do {						\
   Dst = make_tuple(HTOP);			\
   pt_arity = (Arity);				\
 } while (0)

/*
 * Check that we haven't used the reductions and jump to function pointed to by
 * the I register.  If we are out of reductions, do a context switch.
 */

#define DispatchMacro()				\
  do {						\
     BeamInstr* dis_next;				\
     dis_next = (BeamInstr *) *I;			\
     CHECK_ARGS(I);				\
     if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch;			\
     }						\
 } while (0)

#define DispatchMacroFun()			\
  do {						\
     BeamInstr* dis_next;				\
     dis_next = (BeamInstr *) *I;			\
     CHECK_ARGS(I);				\
     if (FCALLS > 0 || FCALLS > neg_o_reds) {	\
        FCALLS--;				\
        Goto(dis_next);				\
     } else {					\
	goto context_switch_fun;		\
     }						\
 } while (0)

#define DispatchMacrox()					\
  do {								\
     if (FCALLS > 0) {						\
        Eterm* dis_next;					\
        SET_I(((Export *) Arg(0))->addressv[erts_active_code_ix()]); \
        dis_next = (Eterm *) *I;				\
        FCALLS--;						\
        CHECK_ARGS(I);						\
        Goto(dis_next);						\
     /* ESTUB: no tracing */					\
     } else {							\
        SET_I(((Export *) Arg(0))->addressv[erts_active_code_ix()]); \
        CHECK_ARGS(I);						\
	goto context_switch;					\
     }								\
 } while (0)

#ifdef DEBUG
/*
 * To simplify breakpoint setting, put the code in one place only and jump to it.
 */
#  define Dispatch() goto do_dispatch
#  define Dispatchx() goto do_dispatchx
#  define Dispatchfun() goto do_dispatchfun
#else
/*
 * Inline for speed.
 */
#  define Dispatch() DispatchMacro()
#  define Dispatchx() DispatchMacrox()
#  define Dispatchfun() DispatchMacroFun()
#endif

#define Self(R) R = c_p->common.id
#define Node(R) EPIPHANY_STUB(erts_this_node)

#define Arg(N)       I[(N)+1]
#define Next(N)                \
    I += (N) + 1;              \
    ASSERT(VALID_INSTR(*I));   \
    Goto(*I)

#define PreFetch(N, Dst) do { Dst = (BeamInstr *) *(I + N + 1); } while (0)
#define NextPF(N, Dst)         \
    I += N + 1;                \
    ASSERT(VALID_INSTR(Dst));  \
    Goto(Dst)

#define GetR(pos, tr) \
   do { \
     tr = Arg(pos); \
     switch (beam_reg_tag(tr)) { \
     case R_REG_DEF: tr = r(0); break; \
     case X_REG_DEF: tr = xb(x_reg_offset(tr)); break; \
     case Y_REG_DEF: ASSERT(y_reg_offset(tr) >= 1); tr = yb(y_reg_offset(tr)); break; \
     } \
     CHECK_TERM(tr); \
   } while (0)

#define GetArg1(N, Dst) GetR((N), Dst)

#define GetArg2(N, Dst1, Dst2)     \
   do {                            \
     GetR(N, Dst1);                \
     GetR((N)+1, Dst2);            \
   } while (0)

#define PutList(H, T, Dst, Store)  \
  do {                             \
   HTOP[0] = (H); HTOP[1] = (T);   \
   Store(make_list(HTOP), Dst);    \
   HTOP += 2;                      \
  } while (0)

#define Move(Src, Dst, Store)      \
   do {                            \
       Eterm term = (Src);         \
       Store(term, Dst);           \
   } while (0)

#define Move2(src1, dst1, src2, dst2) dst1 = (src1); dst2 = (src2)

#define MoveGenDest(src, dstp) \
   if ((dstp) == NULL) { r(0) = (src); } else { *(dstp) = src; }

#define MoveReturn(Src, Dest)       \
    (Dest) = (Src);                 \
    I = c_p->cp;                    \
    ASSERT(VALID_INSTR(*c_p->cp));  \
    c_p->cp = 0;                    \
    CHECK_TERM(r(0));               \
    Goto(*I)

#define DeallocateReturn(Deallocate)       \
  do {                                     \
    int words_to_pop = (Deallocate);       \
    SET_I((BeamInstr *) cp_val(*E));                     \
    E = ADD_BYTE_OFFSET(E, words_to_pop);  \
    CHECK_TERM(r(0));                      \
    Goto(*I);                              \
  } while (0)

#define MoveDeallocateReturn(Src, Dest, Deallocate)  \
    (Dest) = (Src);                                  \
    DeallocateReturn(Deallocate)

#define MoveCall(Src, Dest, CallDest, Size)	\
    (Dest) = (Src);				\
    SET_CP(c_p, I+Size+1);			\
    SET_I((BeamInstr *) CallDest);			\
    Dispatch();

#define MoveCallLast(Src, Dest, CallDest, Deallocate)	\
    (Dest) = (Src);					\
    RESTORE_CP(E);					\
    E = ADD_BYTE_OFFSET(E, (Deallocate));		\
    SET_I((BeamInstr *) CallDest);				\
    Dispatch();

#define MoveCallOnly(Src, Dest, CallDest)	\
    (Dest) = (Src);				\
    SET_I((BeamInstr *) CallDest);			\
    Dispatch();

#define MoveJump(Src)				\
     r(0) = (Src);				\
     SET_I((BeamInstr *) Arg(0));		\
     Goto(*I);

#define GetList(Src, H, T) do {			\
   Eterm* tmp_ptr = list_val(Src);		\
   H = CAR(tmp_ptr);				\
   T = CDR(tmp_ptr); } while (0)

#define GetTupleElement(Src, Element, Dest)				\
  do {									\
    tmp_arg1 = (Eterm) COMPRESS_POINTER(((unsigned char *) tuple_val(Src)) + 	\
				(Element));				\
    (Dest) = (*(Eterm *) EXPAND_POINTER(tmp_arg1));			\
  } while (0)

#define ExtractNextElement(Dest)					  \
    tmp_arg1 += sizeof(Eterm);						  \
    (Dest) = (* (Eterm *) (((unsigned char *) EXPAND_POINTER(tmp_arg1))))

#define ExtractNextElement2(Dest)				\
  do {								\
    Eterm* ene_dstp = &(Dest);					\
    ene_dstp[0] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[1];	\
    ene_dstp[1] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[2];	\
    tmp_arg1 += sizeof(Eterm) + sizeof(Eterm);			\
  } while (0)

#define ExtractNextElement3(Dest)		\
  do {						\
    Eterm* ene_dstp = &(Dest);			\
    ene_dstp[0] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[1];	\
    ene_dstp[1] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[2];	\
    ene_dstp[2] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[3];	\
    tmp_arg1 += 3*sizeof(Eterm);		\
  } while (0)

#define ExtractNextElement4(Dest)		\
  do {						\
    Eterm* ene_dstp = &(Dest);			\
    ene_dstp[0] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[1];	\
    ene_dstp[1] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[2];	\
    ene_dstp[2] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[3];	\
    ene_dstp[3] = ((Eterm *) EXPAND_POINTER(tmp_arg1))[4];	\
    tmp_arg1 += 4*sizeof(Eterm);		\
  } while (0)

#define ExtractElement(Element, Dest)		\
  do {						\
     tmp_arg1 += (Element);			\
     (Dest) = (* (Eterm *) EXPAND_POINTER(tmp_arg1));		\
  } while (0)

#define EqualImmed(X, Y, Action) if (X != Y) { Action; }
#define NotEqualImmed(X, Y, Action) if (X == Y) { Action; }

#define IsFloat(Src, Fail) if (is_not_float(Src)) { Fail; }

#define IsInteger(Src, Fail) if (is_not_integer(Src)) { Fail; }

#define IsNumber(X, Fail) if (is_not_integer(X) && is_not_float(X)) { Fail; }

#define IsAtom(Src, Fail) if (is_not_atom(Src)) { Fail; }

#define IsIntegerAllocate(Src, Need, Alive, Fail)  \
    if (is_not_integer(Src)) { Fail; }             \
    A(Need, Alive)

#define IsNil(Src, Fail) if (is_not_nil(Src)) { Fail; }

#define IsList(Src, Fail) if (is_not_list(Src) && is_not_nil(Src)) { Fail; }

#define IsNonemptyList(Src, Fail) if (is_not_list(Src)) { Fail; }

#define IsNonemptyListAllocate(Src, Need, Alive, Fail)  \
    if (is_not_list(Src)) { Fail; }                     \
    A(Need, Alive)

#define IsNonemptyListTestHeap(Src, Need, Alive, Fail)  \
    if (is_not_list(Src)) { Fail; }                     \
    TestHeap(Need, Alive)

#define IsTuple(X, Action) if (is_not_tuple(X)) Action

#define IsArity(Pointer, Arity, Fail)					  \
    if (*(Eterm *)							  \
	EXPAND_POINTER(tmp_arg1 = (Eterm) 				  \
		       COMPRESS_POINTER(tuple_val(Pointer))) != (Arity))  \
    { 									  \
        Fail; 								  \
    }

#define IsMap(Src, Fail) if (is_not_map(Src)) { Fail; }

#define HasMapField(Src, Key, Fail) if (has_not_map_field(Src, Key)) { Fail; }

#define GetMapElement(Src, Key, Dst, Fail)	\
  do {						\
     Eterm _res = get_map_element(Src, Key);	\
     if (is_non_value(_res)) {			\
        Fail;					\
     }						\
     Dst = _res;				\
  } while (0)

#define IsFunction(X, Action)			\
  do {						\
     if ( !(is_any_fun(X)) ) {			\
          Action;				\
     }						\
  } while (0)

#define IsFunction2(F, A, Action)			\
  do {							\
     if (erl_is_function(c_p, F, A) != am_true ) {	\
          Action;					\
     }							\
  } while (0)

#define IsTupleOfArity(Src, Arity, Fail)				      \
  do {									      \
    if (is_not_tuple(Src) || 						      \
	*(Eterm *)							      \
	EXPAND_POINTER(tmp_arg1 = 					      \
		       (Eterm) COMPRESS_POINTER(tuple_val(Src))) != Arity) { \
        Fail;								      \
    }									      \
  } while (0)

#define IsBoolean(X, Fail) if ((X) != am_true && (X) != am_false) { Fail; }

#define IsBinary(Src, Fail) \
 if (is_not_binary(Src) || binary_bitsize(Src) != 0) { Fail; }

#define IsBitstring(Src, Fail) \
  if (is_not_binary(Src)) { Fail; }

#define BsSafeMul(A, B, Fail, Target) EPIPHANY_STUB(BsSafeMul)

#define BsGetFieldSize(Bits, Unit, Fail, Target) EPIPHANY_STUB(BsGetFieldSize)

#define BsGetUncheckedFieldSize(Bits, Unit, Fail, Target) EPIPHANY_STUB(BsGetUncheckedFieldSize)

#define BsGetFloat2(Ms, Live, Sz, Flags, Dst, Store, Fail) EPIPHANY_STUB(BsGetFloat2)

#define BsGetBinaryImm_2(Ms, Live, Sz, Flags, Dst, Store, Fail) EPIPHANY_STUB(BsGetBinaryImm_2)

#define BsGetBinary_2(Ms, Live, Sz, Flags, Dst, Store, Fail) EPIPHANY_STUB(BsGetBinary_2)

#define BsGetBinaryAll_2(Ms, Live, Unit, Dst, Store, Fail) EPIPHANY_STUB(BsGetBinaryAll_2)

#define BsSkipBits2(Ms, Bits, Unit, Fail) EPIPHANY_STUB(BsSkipBits2)

#define BsSkipBitsAll2(Ms, Unit, Fail) EPIPHANY_STUB(BsSkipBitsAll2)

#define BsSkipBitsImm2(Ms, Bits, Fail) EPIPHANY_STUB(BsSkipBitsImm2)

#define NewBsPutIntegerImm(Sz, Flags, Src) EPIPHANY_STUB(NewBsPutIntegerImm)

#define NewBsPutInteger(Sz, Flags, Src) EPIPHANY_STUB(NewBsPutInteger)

#define NewBsPutFloatImm(Sz, Flags, Src) EPIPHANY_STUB(NewBsPutFloatImm)

#define NewBsPutFloat(Sz, Flags, Src) EPIPHANY_STUB(NewBsPutFloat)

#define NewBsPutBinary(Sz, Flags, Src) EPIPHANY_STUB(NewBsPutBinary)

#define NewBsPutBinaryImm(Sz, Src) EPIPHANY_STUB(NewBsPutBinaryImm)

#define NewBsPutBinaryAll(Src, Unit) EPIPHANY_STUB(NewBsPutBinaryAll);

#define IsPort(Src, Fail) if (is_not_port(Src)) { Fail; }
#define IsPid(Src, Fail) if (is_not_pid(Src)) { Fail; }
#define IsRef(Src, Fail) if (is_not_ref(Src)) { Fail; }

/*
 * process_main() is already huge, so we want to avoid inlining
 * into it. Especially functions that are seldom used.
 */
#ifdef __GNUC__
#  define NOINLINE __attribute__((__noinline__))
#else
#  define NOINLINE
#endif


/*
 * The following functions are called directly by process_main().
 * Don't inline them.
 */
static BifFunction translate_gc_bif(void* gcf) NOINLINE;
static BeamInstr* handle_error(Process* c_p, BeamInstr* pc,
			       Eterm* reg, BifFunction bf) NOINLINE;
static BeamInstr* call_error_handler(Process* p, BeamInstr* ip,
				     Eterm* reg, Eterm func) NOINLINE;
static BeamInstr* fixed_apply(Process* p, Eterm* reg, Uint arity) NOINLINE;
static BeamInstr* apply(Process* p, Eterm module, Eterm function,
			Eterm args, Eterm* reg) NOINLINE;
static BeamInstr* call_fun(Process* p, int arity,
			   Eterm* reg, Eterm args) NOINLINE;
static BeamInstr* apply_fun(Process* p, Eterm fun,
			    Eterm args, Eterm* reg) NOINLINE;
static Eterm new_fun(Process* p, Eterm* reg,
		     ErlFunEntry* fe, int num_free) NOINLINE;
static Eterm new_map(Process* p, Eterm* reg, BeamInstr* I) NOINLINE;
static Eterm update_map_assoc(Process* p, Eterm* reg,
			      Eterm map, BeamInstr* I) NOINLINE;
static Eterm update_map_exact(Process* p, Eterm* reg,
			      Eterm map, BeamInstr* I) NOINLINE;
static int has_not_map_field(Eterm map, Eterm key);
static Eterm get_map_element(Eterm map, Eterm key);

/*
 * Functions not directly called by process_main(). OK to inline.
 */
static BeamInstr* next_catch(Process* c_p, Eterm *reg);
static void terminate_proc(Process* c_p, Eterm Value);
static Eterm add_stacktrace(Process* c_p, Eterm Value, Eterm exc);
static void save_stacktrace(Process* c_p, BeamInstr* pc, Eterm* reg,
			     BifFunction bf, Eterm args);
static struct StackTrace * get_trace_from_exc(Eterm exc);
static Eterm make_arglist(Process* c_p, Eterm* reg, int a);

void
init_emulator(void)
{
    process_main();
}

/*
 * On certain platforms, make sure that the main variables really are placed
 * in registers.
 */

#if defined(__GNUC__) && defined(sparc) && !defined(DEBUG)
#  define REG_x0 asm("%l0")
#  define REG_xregs asm("%l1")
#  define REG_htop asm("%l2")
#  define REG_stop asm("%l3")
#  define REG_I asm("%l4")
#  define REG_fcalls asm("%l5")
#  define REG_tmp_arg1 asm("%l6")
#  define REG_tmp_arg2 asm("%l7")
#else
#  define REG_x0
#  define REG_xregs
#  define REG_htop
#  define REG_stop
#  define REG_I
#  define REG_fcalls
#  define REG_tmp_arg1
#  define REG_tmp_arg2
#endif

#ifdef USE_VM_PROBES
#  define USE_VM_CALL_PROBES
#endif

#ifdef USE_VM_CALL_PROBES

#define DTRACE_LOCAL_CALL(p, m, f, a)					\
    if (DTRACE_ENABLED(local_function_entry)) {				\
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);		\
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);			\
        int depth = STACK_START(p) - STACK_TOP(p);			\
        dtrace_fun_decode(p, m, f, a,					\
                          process_name, mfa);				\
        DTRACE3(local_function_entry, process_name, mfa, depth);	\
    }

#define DTRACE_GLOBAL_CALL(p, m, f, a)					\
    if (DTRACE_ENABLED(global_function_entry)) {			\
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);		\
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);			\
        int depth = STACK_START(p) - STACK_TOP(p);			\
        dtrace_fun_decode(p, m, f, a,					\
                          process_name, mfa);				\
        DTRACE3(global_function_entry, process_name, mfa, depth);	\
    }

#define DTRACE_RETURN(p, m, f, a)                               \
    if (DTRACE_ENABLED(function_return)) {                      \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        int depth = STACK_START(p) - STACK_TOP(p);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE3(function_return, process_name, mfa, depth);     \
    }

#define DTRACE_BIF_ENTRY(p, m, f, a)                            \
    if (DTRACE_ENABLED(bif_entry)) {                            \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE2(bif_entry, process_name, mfa);                  \
    }

#define DTRACE_BIF_RETURN(p, m, f, a)                           \
    if (DTRACE_ENABLED(bif_return)) {                           \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE2(bif_return, process_name, mfa);                 \
    }

#define DTRACE_NIF_ENTRY(p, m, f, a)                            \
    if (DTRACE_ENABLED(nif_entry)) {                            \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE2(nif_entry, process_name, mfa);                  \
    }

#define DTRACE_NIF_RETURN(p, m, f, a)                           \
    if (DTRACE_ENABLED(nif_return)) {                           \
        DTRACE_CHARBUF(process_name, DTRACE_TERM_BUF_SIZE);     \
        DTRACE_CHARBUF(mfa, DTRACE_TERM_BUF_SIZE);              \
        dtrace_fun_decode(p, m, f, a,                           \
                          process_name, mfa);                   \
        DTRACE2(nif_return, process_name, mfa);                 \
    }

#else /* USE_VM_PROBES */

#define DTRACE_LOCAL_CALL(p, m, f, a)  do {} while (0)
#define DTRACE_GLOBAL_CALL(p, m, f, a) do {} while (0)
#define DTRACE_RETURN(p, m, f, a)      do {} while (0)
#define DTRACE_BIF_ENTRY(p, m, f, a)   do {} while (0)
#define DTRACE_BIF_RETURN(p, m, f, a)  do {} while (0)
#define DTRACE_NIF_ENTRY(p, m, f, a)   do {} while (0)
#define DTRACE_NIF_RETURN(p, m, f, a)  do {} while (0)

#endif /* USE_VM_PROBES */

/*
 * process_main() is called twice:
 * The first call performs some initialisation, including exporting
 * the instructions' C labels to the loader.
 * The second call starts execution of BEAM code. This call never returns.
 */
void process_main(void)
{
    static int init_done = 0;
    Process* c_p = NULL;
    int reds_used;
#ifdef DEBUG
    ERTS_DECLARE_DUMMY(Eterm pid);
#endif

    /*
     * X register zero; also called r(0)
     */
    register Eterm x0 REG_x0 = NIL;

    /* Pointer to X registers: x(1)..x(N); reg[0] is used when doing GC,
     * in all other cases x0 is used.
     */
    register Eterm* reg REG_xregs = NULL;

    /*
     * Top of heap (next free location); grows upwards.
     */
    register Eterm* HTOP REG_htop = NULL;

    /* Stack pointer.  Grows downwards; points
     * to last item pushed (normally a saved
     * continuation pointer).
     */
    register Eterm* E REG_stop = NULL;

    /*
     * Pointer to next threaded instruction.
     */
    register BeamInstr *I REG_I = NULL;

    /* Number of reductions left.  This function
     * returns to the scheduler when FCALLS reaches zero.
     */
    register Sint FCALLS REG_fcalls = 0;

    /*
     * Temporaries used for picking up arguments for instructions.
     */
    register Eterm tmp_arg1 REG_tmp_arg1 = NIL;
    register Eterm tmp_arg2 REG_tmp_arg2 = NIL;
#if HEAP_ON_C_STACK
    Eterm tmp_big[2];           /* Temporary buffer for small bignums if HEAP_ON_C_STACK. */
#else
    Eterm *tmp_big;		/* Temporary buffer for small bignums if !HEAP_ON_C_STACK. */
#endif

    /*
     * X registers and floating point registers are located in
     * scheduler specific data.
     */
    register FloatDef *freg;

    /*
     * For keeping the negative old value of 'reds' when call saving is active.
     */
    int neg_o_reds = 0;

    Eterm (*arith_func)(Process* p, Eterm* reg, Uint live);

#ifndef NO_JUMP_TABLE
    static void* opcodes[] = { DEFINE_OPCODES };
#ifdef ERTS_OPCODE_COUNTER_SUPPORT
    static void* counting_opcodes[] = { DEFINE_COUNTING_OPCODES };
#endif
#else
    int Go;
#endif

    Eterm pt_arity;		/* Used by do_put_tuple */

#pragma GCC diagnostic push
    /*
     * Although all uses of the bits state have been stubbed out, we don't want
     * to remove the places or opcodes that set it, in case we later reintroduce
     * the bits opcodes.
     */
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
    ERL_BITS_DECLARE_STATEP; /* Has to be last declaration */
#pragma GCC diagnostic pop


    /*
     * Note: In this function, we attempt to place rarely executed code towards
     * the end of the function, in the hope that the cache hit rate will be better.
     * The initialization code is only run once, so it is at the very end.
     *
     * Note: c_p->arity must be set to reflect the number of useful terms in
     * c_p->arg_reg before calling the scheduler.
     */
    if (!init_done) {
       /* This should only be reached during the init phase when only the main
        * process is running. I.e. there is no race for init_done.
        */
	init_done = 1;
	goto init_emulator;
    }

    c_p = NULL;
    reds_used = 0;

    goto do_schedule1;

 do_schedule:
    reds_used = REDS_IN(c_p) - FCALLS;
 do_schedule1:

    // ESTUB: No tracing

    PROCESS_MAIN_CHK_LOCKS(c_p);
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
#if HALFWORD_HEAP
    ASSERT(erts_get_scheduler_data()->num_tmp_heap_used == 0);
#endif
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
    c_p = schedule(c_p, reds_used);
    ASSERT(epiphany_in_dram(c_p));
    ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
#ifdef DEBUG
    pid = c_p->common.id; /* Save for debugging purpouses */
#endif
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    PROCESS_MAIN_CHK_LOCKS(c_p);

    ASSERT(epiphany_sane_address(ERTS_PROC_GET_SCHDATA(c_p)));
    reg = ERTS_PROC_GET_SCHDATA(c_p)->x_reg_array;
    ASSERT(epiphany_sane_address(reg));
    freg = ERTS_PROC_GET_SCHDATA(c_p)->f_reg_array;
    ASSERT(epiphany_sane_address(freg));
#if !HEAP_ON_C_STACK
    tmp_big = ERTS_PROC_GET_SCHDATA(c_p)->beam_emu_tmp_heap;
#endif
    ERL_BITS_RELOAD_STATEP(c_p);
    {
	int reds;
	Eterm* argp;
	BeamInstr *next;
	int i;

	argp = c_p->arg_reg;
	for (i = c_p->arity - 1; i > 0; i--) {
	    reg[i] = argp[i];
	    CHECK_TERM(reg[i]);
	}

	/*
	 * We put the original reduction count in the process structure, to reduce
	 * the code size (referencing a field in a struct through a pointer stored
	 * in a register gives smaller code than referencing a global variable).
	 */

	SET_I(c_p->i);

	reds = c_p->fcalls;
	if (ERTS_PROC_GET_SAVED_CALLS_BUF(c_p)
	    && (ERTS_TRACE_FLAGS(c_p) & F_SENSITIVE) == 0) {
	    neg_o_reds = -reds;
	    FCALLS = REDS_IN(c_p) = 0;
	} else {
	    neg_o_reds = 0;
	    FCALLS = REDS_IN(c_p) = reds;
	}

	next = (BeamInstr *) *I;
	r(0) = c_p->arg_reg[0];
#ifdef HARDDEBUG
	if (c_p->arity > 0) {
	    CHECK_TERM(r(0));
	}
#endif
	SWAPIN;
	ASSERT(VALID_INSTR(next));

#ifdef USE_VM_PROBES
        if (DTRACE_ENABLED(process_scheduled)) {
            DTRACE_CHARBUF(process_buf, DTRACE_TERM_BUF_SIZE);
            DTRACE_CHARBUF(fun_buf, DTRACE_TERM_BUF_SIZE);
            dtrace_proc_str(c_p, process_buf);

            if (ERTS_PROC_IS_EXITING(c_p)) {
                strcpy(fun_buf, "<exiting>");
            } else {
                BeamInstr *fptr = find_function_from_pc(c_p->i);
                if (fptr) {
                    dtrace_fun_decode(c_p, (Eterm)fptr[0],
                                      (Eterm)fptr[1], (Uint)fptr[2],
                                      NULL, fun_buf);
                } else {
                    erts_snprintf(fun_buf, sizeof(DTRACE_CHARBUF_NAME(fun_buf)),
                                  "<unknown/%p>", next);
                }
            }

            DTRACE2(process_scheduled, process_buf, fun_buf);
        }
#endif
	Goto(next);
    }

#if defined(DEBUG) || defined(NO_JUMP_TABLE)
 emulator_loop:
#endif

#ifdef NO_JUMP_TABLE
    switch (Go) {
#endif
#include "beam_hot.h"

#define STORE_ARITH_RESULT(res) StoreBifResult(2, (res));
#define ARITH_FUNC(name) erts_gc_##name

	{
	    Eterm increment_reg_val;
	    Eterm increment_val;
	    Uint live;
	    Eterm result;

	OpCase(i_increment_yIId):
	    increment_reg_val = yb(Arg(0));
	    goto do_increment;

	OpCase(i_increment_xIId):
	    increment_reg_val = xb(Arg(0));
	    goto do_increment;

	OpCase(i_increment_rIId):
	    increment_reg_val = r(0);
	    I--;

	do_increment:
	    increment_val = Arg(1);
	    if (is_small(increment_reg_val)) {
		Sint i = signed_val(increment_reg_val) + increment_val;
		ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
		if (MY_IS_SSMALL(i)) {
		    result = make_small(i);
		store_result:
		    StoreBifResult(3, result);
		}
	    }

	    live = Arg(2);
	    SWAPOUT;
	    reg[0] = r(0);
	    reg[live] = increment_reg_val;
	    reg[live+1] = make_small(increment_val);
	    result = erts_gc_mixed_plus(c_p, reg, live);
	    r(0) = reg[0];
	    SWAPIN;
	    ERTS_HOLE_CHECK(c_p);
	    if (is_value(result)) {
		goto store_result;
	    }
	    ASSERT(c_p->freason != BADMATCH || is_value(c_p->fvalue));
	    goto find_func_info;
	}
	    
 OpCase(i_plus_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 Sint i = signed_val(tmp_arg1) + signed_val(tmp_arg2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
	     STORE_ARITH_RESULT(result);
	 }
     
     }
     arith_func = ARITH_FUNC(mixed_plus);
     goto do_big_arith2;
 }

 OpCase(i_minus_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 Sint i = signed_val(tmp_arg1) - signed_val(tmp_arg2);
	 ASSERT(MY_IS_SSMALL(i) == IS_SSMALL(i));
	 if (MY_IS_SSMALL(i)) {
	     result = make_small(i);
	     STORE_ARITH_RESULT(result);
	 }
     }
     arith_func = ARITH_FUNC(mixed_minus);
     goto do_big_arith2;
 }

 OpCase(i_is_lt_f):
    if (CMP_GE(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_ge_f):
    if (CMP_LT(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_eq_f):
    if (CMP_NE(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_ne_f):
    if (CMP_EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(i_is_eq_exact_f):
    if (!EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

    {
	Eterm is_eq_exact_lit_val;

    OpCase(i_is_eq_exact_literal_xfc):
	is_eq_exact_lit_val = xb(Arg(0));
	I++;
	goto do_is_eq_exact_literal;

    OpCase(i_is_eq_exact_literal_yfc):
	is_eq_exact_lit_val = yb(Arg(0));
	I++;
	goto do_is_eq_exact_literal;

    OpCase(i_is_eq_exact_literal_rfc):
	is_eq_exact_lit_val = r(0);

    do_is_eq_exact_literal:
	if (!eq(Arg(1), is_eq_exact_lit_val)) {
	    ClauseFail();
	}
	Next(2);
    }

    {
	Eterm is_ne_exact_lit_val;

    OpCase(i_is_ne_exact_literal_xfc):
	is_ne_exact_lit_val = xb(Arg(0));
	I++;
	goto do_is_ne_exact_literal;

    OpCase(i_is_ne_exact_literal_yfc):
	is_ne_exact_lit_val = yb(Arg(0));
	I++;
	goto do_is_ne_exact_literal;

    OpCase(i_is_ne_exact_literal_rfc):
	is_ne_exact_lit_val = r(0);

    do_is_ne_exact_literal:
	if (eq(Arg(1), is_ne_exact_lit_val)) {
	    ClauseFail();
	}
	Next(2);
    }

 OpCase(i_move_call_only_fcr): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
 OpCase(i_call_only_f): {
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, (Eterm)I[-3], (Eterm)I[-2], I[-1]);
     Dispatch();
 }

 OpCase(i_move_call_last_fPcr): {
     r(0) = Arg(2);
 }
 /* FALL THROUGH */
 OpCase(i_call_last_fP): {
     RESTORE_CP(E);
     E = ADD_BYTE_OFFSET(E, Arg(1));
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, (Eterm)I[-3], (Eterm)I[-2], I[-1]);
     Dispatch();
 }

 OpCase(i_move_call_crf): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_f): {
     SET_CP(c_p, I+2);
     SET_I((BeamInstr *) Arg(0));
     DTRACE_LOCAL_CALL(c_p, (Eterm)I[-3], (Eterm)I[-2], I[-1]);
     Dispatch();
 }

 OpCase(i_move_call_ext_last_ePcr): {
     r(0) = Arg(2);
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_last_eP):
    RESTORE_CP(E);
    E = ADD_BYTE_OFFSET(E, Arg(1));

    /*
     * Note: The pointer to the export entry is never NULL; if the module
     * is not loaded, it points to code which will invoke the error handler
     * (see lb_call_error_handler below).
     */
#ifdef USE_VM_CALL_PROBES
    if (DTRACE_ENABLED(global_function_entry)) {
	BeamInstr* fp = (BeamInstr *) (((Export *) Arg(0))->addressv[erts_active_code_ix()]);
	DTRACE_GLOBAL_CALL(c_p, (Eterm)fp[-3], (Eterm)fp[-2], fp[-1]);
    }
#endif
    Dispatchx();

 OpCase(i_move_call_ext_cre): {
     r(0) = Arg(0);
     I++;
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_e):
    SET_CP(c_p, I+2);
#ifdef USE_VM_CALL_PROBES
    if (DTRACE_ENABLED(global_function_entry)) {
	BeamInstr* fp = (BeamInstr *) (((Export *) Arg(0))->addressv[erts_active_code_ix()]);
	DTRACE_GLOBAL_CALL(c_p, (Eterm)fp[-3], (Eterm)fp[-2], fp[-1]);
    }
#endif
    Dispatchx();

 OpCase(i_move_call_ext_only_ecr): {
     r(0) = Arg(1);
 }
 /* FALL THROUGH */
 OpCase(i_call_ext_only_e):
#ifdef USE_VM_CALL_PROBES
    if (DTRACE_ENABLED(global_function_entry)) {
	BeamInstr* fp = (BeamInstr *) (((Export *) Arg(0))->addressv[erts_active_code_ix()]);
	DTRACE_GLOBAL_CALL(c_p, (Eterm)fp[-3], (Eterm)fp[-2], fp[-1]);
    }
#endif
    Dispatchx();

 OpCase(init_y): {
     BeamInstr *next;

     PreFetch(1, next);
     make_blank(yb(Arg(0)));
     NextPF(1, next);
 }

 OpCase(i_trim_I): {
     BeamInstr *next;
     Uint words;
     Uint cp;

     words = Arg(0);
     cp = E[0];
     PreFetch(1, next);
     E += words;
     E[0] = cp;
     NextPF(1, next);
 }

 OpCase(move_x1_c): {
	x(1) = Arg(0);
	Next(1);
    }

 OpCase(move_x2_c): {
	x(2) = Arg(0);
	Next(1);
    }


 OpCase(return): {
#ifdef USE_VM_CALL_PROBES
    BeamInstr* fptr;
#endif
    SET_I(c_p->cp);

#ifdef USE_VM_CALL_PROBES
    if (DTRACE_ENABLED(function_return) && (fptr = find_function_from_pc(c_p->cp))) {
        DTRACE_RETURN(c_p, (Eterm)fptr[0], (Eterm)fptr[1], (Uint)fptr[2]);
    }
#endif
    /*
     * We must clear the CP to make sure that a stale value do not
     * create a false module dependcy preventing code upgrading.
     * It also means that we can use the CP in stack backtraces.
     */
    c_p->cp = 0;
    CHECK_TERM(r(0));
    HEAP_SPACE_VERIFIED(0);
    Goto(*I);
 }

    /*
     * Send is almost a standard call-BIF with two arguments, except for:
     *    1) It cannot be traced.
     *	  2) There is no pointer to the send_2 function stored in
     *       the instruction.
     */

 OpCase(send): {
     BeamInstr *next;
     Eterm result;

     PRE_BIF_SWAPOUT(c_p);
     c_p->fcalls = FCALLS - 1;
     reg[0] = r(0);
     result = erl_send(c_p, r(0), x(1));
     PreFetch(0, next);
     ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
     ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
     PROCESS_MAIN_CHK_LOCKS(c_p);
     if (c_p->mbuf || MSO(c_p).overhead >= BIN_VHEAP_SZ(c_p)) {
	 result = erts_gc_after_bif_call(c_p, result, reg, 2);
	 r(0) = reg[0];
	 E = c_p->stop;
     }
     HTOP = HEAP_TOP(c_p);
     FCALLS = c_p->fcalls;
     if (is_value(result)) {
	 r(0) = result;
	 CHECK_TERM(r(0));
	 NextPF(0, next);
     } else if (c_p->freason == TRAP) {
	 SET_CP(c_p, I+1);
	 SET_I(c_p->i);
	 SWAPIN;
	 r(0) = reg[0];
	 Dispatch();
     }
     goto find_func_info;
 }

    {
	Eterm element_index;
	Eterm element_tuple;

    OpCase(i_element_xjsd):
	element_tuple = xb(Arg(0));
	I++;
	goto do_element;

    OpCase(i_element_yjsd):
	element_tuple = yb(Arg(0));
	I++;
	goto do_element;

    OpCase(i_element_rjsd):
	element_tuple = r(0);
	/* Fall through */

    do_element:
	GetArg1(1, element_index);
	if (is_small(element_index) && is_tuple(element_tuple)) {
	    Eterm* tp = tuple_val(element_tuple);

	    if ((signed_val(element_index) >= 1) &&
		(signed_val(element_index) <= arityval(*tp))) {
		Eterm result = tp[signed_val(element_index)];
		StoreBifResult(2, result);
	    }
	}
    }
 /* Fall through */

 OpCase(badarg_j):
 badarg:
    c_p->freason = BADARG;
    goto lb_Cl_error;

    {
	Eterm fast_element_tuple;

    OpCase(i_fast_element_rjId):
	fast_element_tuple = r(0);

    do_fast_element:
	if (is_tuple(fast_element_tuple)) {
	    Eterm* tp = tuple_val(fast_element_tuple);
	    Eterm pos = Arg(1);	/* Untagged integer >= 1 */
	    if (pos <= arityval(*tp)) {
		Eterm result = tp[pos];
		StoreBifResult(2, result);
	    }
	}
     goto badarg;

    OpCase(i_fast_element_xjId):
     fast_element_tuple = xb(Arg(0));
     I++;
     goto do_fast_element;

    OpCase(i_fast_element_yjId):
     fast_element_tuple = yb(Arg(0));
     I++;
     goto do_fast_element;
 }

 OpCase(catch_yf):
     c_p->catches++;
     yb(Arg(0)) = Arg(1);
     Next(2);

 OpCase(catch_end_y): {
     c_p->catches--;
     make_blank(yb(Arg(0)));
     if (is_non_value(r(0))) {
	 if (x(1) == am_throw) {
	     r(0) = x(2);
	 } else {
	     if (x(1) == am_error) {
	         SWAPOUT;
		 x(2) = add_stacktrace(c_p, x(2), x(3));
		 SWAPIN;
	     }
	     /* only x(2) is included in the rootset here */
	     if (E - HTOP < 3 || c_p->mbuf) {	/* Force GC in case add_stacktrace()
						 * created heap fragments */
		 SWAPOUT;
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 FCALLS -= erts_garbage_collect(c_p, 3, reg+2, 1);
		 ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
		 PROCESS_MAIN_CHK_LOCKS(c_p);
		 SWAPIN;
	     }
	     r(0) = TUPLE2(HTOP, am_EXIT, x(2));
	     HTOP += 3;
	 }
     }
     CHECK_TERM(r(0));
     Next(1);
 }

 OpCase(try_end_y): {
     c_p->catches--;
     make_blank(yb(Arg(0)));
     if (is_non_value(r(0))) {
	 r(0) = x(1);
	 x(1) = x(2);
	 x(2) = x(3);
     }
     Next(1);
 }

 /*
  * Skeleton for receive statement:
  *
  *             recv_mark L1                     Optional
  *             call make_ref/monitor            Optional
  *             ...
  *             recv_set L1                      Optional
  *      L1:          <-------------------+
  *                   <-----------+       |
  *     	     	       	  |   	  |
  *             loop_rec L2 ------+---+   |
  *             ...               |   |   |
  *             remove_message 	  |   |	  |
  *             jump L3           |   |   |
  *		...	          |   |   |
  *		loop_rec_end L1 --+   |   |
  *      L2:          <---------------+   |
  *	   	wait L1  -----------------+      or wait_timeout
  *		timeout
  *
  *	 L3:    Code after receive...
  *
  *
  */

 OpCase(recv_mark_f): {
     /*
      * Save the current position in message buffer and the
      * the label for the loop_rec/2 instruction for the
      * the receive statement.
      */
     c_p->msg.mark = (BeamInstr *) Arg(0);
     c_p->msg.saved_last = c_p->msg.last;
     Next(1);
 }

 OpCase(i_recv_set): {
     /*
      * If the mark is valid (points to the loop_rec/2
      * instruction that follows), we know that the saved
      * position points to the first message that could
      * possibly be matched out.
      *
      * If the mark is invalid, we do nothing, meaning that
      * we will look through all messages in the message queue.
      */
     if (c_p->msg.mark == (BeamInstr *) (I+1)) {
	 c_p->msg.save = c_p->msg.saved_last;
     }
     I++;
     /* Fall through to the loop_rec/2 instruction */
 }

    /*
     * Pick up the next message and place it in x(0).
     * If no message, jump to a wait or wait_timeout instruction.
     */
 OpCase(i_loop_rec_fr):
 {
     BeamInstr *next;
     ErlMessage* msgp;

 loop_rec__:

     PROCESS_MAIN_CHK_LOCKS(c_p);

     msgp = PEEK_MESSAGE(c_p);

     if (!msgp) {
#ifdef ERTS_SMP
     EPIPHANY_STUB(OpCase(i_loop_rec_fr));
#endif
	 {
	     SET_I((BeamInstr *) Arg(0));
	     Goto(*I);		/* Jump to a wait or wait_timeout instruction */
	 }
     }
     ErtsMoveMsgAttachmentIntoProc(msgp, c_p, E, HTOP, FCALLS,
				   {
				       SWAPOUT;
				       reg[0] = r(0);
				       PROCESS_MAIN_CHK_LOCKS(c_p);
				   },
				   {
				       ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
				       PROCESS_MAIN_CHK_LOCKS(c_p);
				       r(0) = reg[0];
				       SWAPIN;
				   });
     if (is_non_value(ERL_MESSAGE_TERM(msgp))) {
	 /*
	  * A corrupt distribution message that we weren't able to decode;
	  * remove it...
	  */
	 ASSERT(!msgp->data.attached);
         /* TODO: Add DTrace probe for this bad message situation? */
	 UNLINK_MESSAGE(c_p, msgp);
	 free_message(msgp);
	 goto loop_rec__;
     }
     PreFetch(1, next);
     r(0) = ERL_MESSAGE_TERM(msgp);
     NextPF(1, next);
 }

 /*
  * Remove a (matched) message from the message queue.
  */
 OpCase(remove_message): {
    EPIPHANY_STUB(OpCase(remove_message));
 }

    /*
     * Advance the save pointer to the next message (the current
     * message didn't match), then jump to the loop_rec instruction.
     */
 OpCase(loop_rec_end_f): {
     SET_I((BeamInstr *) Arg(0));
     SAVE_MESSAGE(c_p);
     goto loop_rec__;
 }
    /*
     * Prepare to wait for a message or a timeout, whichever occurs first.
     *
     * Note: In order to keep the compatibility between 32 and 64 bits
     * emulators, only timeout values that can be represented in 32 bits
     * (unsigned) or less are allowed.
     */


 OpCase(i_wait_timeout_fs): {
     EPIPHANY_STUB(OpCase(i_wait_timeout_fs));

     /* Fall through */
 }
 OpCase(i_wait_timeout_locked_fs): {
     Eterm timeout_value;

     /*
      * If we have already set the timer, we must NOT set it again.  Therefore,
      * we must test the F_INSLPQUEUE flag as well as the F_TIMO flag.
      */
     if (c_p->flags & (F_INSLPQUEUE | F_TIMO)) {
	 goto wait2;
     }
     GetArg1(1, timeout_value);
     if (timeout_value != make_small(0)) {
#if !defined(ARCH_64) || HALFWORD_HEAP
	 Uint time_val;
#endif

	 if (is_small(timeout_value) && signed_val(timeout_value) > 0 &&
#if defined(ARCH_64) && !HALFWORD_HEAP
	     ((unsigned_val(timeout_value) >> 32) == 0)
#else
	     1
#endif
	     ) {
             EPIPHANY_STUB(OpCase(i_wait_timeout_locked_fs));
	 } else if (timeout_value == am_infinity) {
	     c_p->flags |= F_TIMO;
#if !defined(ARCH_64) || HALFWORD_HEAP
	 } else if (term_to_Uint(timeout_value, &time_val)) {
             EPIPHANY_STUB(OpCase(i_wait_timeout_locked_fs));
#endif
	 } else {		/* Wrong time */
	     OpCase(i_wait_error_locked): {
		 EPIPHANY_STUB(OpCase(i_wait_error_locked));
		 /* Fall through */
	     }
	     OpCase(i_wait_error): {
		 c_p->freason = EXC_TIMEOUT_VALUE;
		 goto find_func_info;
	     }
	 }

	 /*
	  * Prepare to wait indefinitely for a new message to arrive
	  * (or the time set above if falling through from above).
	  *
	  * When a new message arrives, control will be transferred
	  * the loop_rec instruction (at label L1).  In case of
	  * of timeout, control will be transferred to the timeout
	  * instruction following the wait_timeout instruction.
	  */

	 OpCase(wait_locked_f):
	 OpCase(wait_f):

	 wait2: {
	     c_p->i = (BeamInstr *) Arg(0); /* L1 */
	     SWAPOUT;
	     c_p->arity = 0;
	     erts_smp_atomic32_read_band_relb(&c_p->state, ~ERTS_PSFLG_ACTIVE);
	     ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	     EPIPHANY_STUB(OpCase(wait_));
	     c_p->current = NULL;
	     goto do_schedule;
	 }
	 OpCase(wait_unlocked_f): {
	     EPIPHANY_STUB(OpCase(wait_unlocked_f));
	     goto wait2;
	 }
     }
     EPIPHANY_STUB(OpCase(i_wait_timeout_locked_fs));
     Next(2);
 }

 OpCase(i_wait_timeout_fI): {
     EPIPHANY_STUB(OpCase(i_wait_timeout_fI));
 }

 OpCase(i_wait_timeout_locked_fI): {
    EPIPHANY_STUB(OpCase(i_wait_timeout_locked_fI));
 }

    /*
     * A timeout has occurred.  Reset the save pointer so that the next
     * receive statement will examine the first message first.
     */
 OpCase(timeout_locked): {
     EPIPHANY_STUB(OpCase(timeout_locked));
 }

 OpCase(timeout): {
     BeamInstr *next;

     PreFetch(0, next);
     // ESTUB: No tracing
     c_p->flags &= ~F_TIMO;
     JOIN_MESSAGE(c_p);
     NextPF(0, next);
 }


 {
     Eterm select_val2;

 OpCase(i_select_tuple_arity2_yfAfAf):
     select_val2 = yb(Arg(0));
     goto do_select_tuple_arity2;

 OpCase(i_select_tuple_arity2_xfAfAf):
     select_val2 = xb(Arg(0));
     goto do_select_tuple_arity2;

 OpCase(i_select_tuple_arity2_rfAfAf):
     select_val2 = r(0);
     I--;

 do_select_tuple_arity2:
     if (is_not_tuple(select_val2)) {
	 goto select_val2_fail;
     }
     select_val2 = *tuple_val(select_val2);
     goto do_select_val2;

 OpCase(i_select_val2_yfcfcf):
     select_val2 = yb(Arg(0));
     goto do_select_val2;

 OpCase(i_select_val2_xfcfcf):
     select_val2 = xb(Arg(0));
     goto do_select_val2;

 OpCase(i_select_val2_rfcfcf):
     select_val2 = r(0);
     I--;

 do_select_val2:
     if (select_val2 == Arg(2)) {
	 I += 2;
     } else if (select_val2 == Arg(4)) {
	 I += 4;
     }

 select_val2_fail:
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }

 {
     Eterm select_val;

 OpCase(i_select_tuple_arity_xfI):
     select_val = xb(Arg(0));
     goto do_select_tuple_arity;

 OpCase(i_select_tuple_arity_yfI):
     select_val = yb(Arg(0));
     goto do_select_tuple_arity;

 OpCase(i_select_tuple_arity_rfI):
     select_val = r(0);
     I--;

 do_select_tuple_arity:
     if (is_tuple(select_val)) {
	 select_val = *tuple_val(select_val);
	 goto do_binary_search;
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);

 OpCase(i_select_val_xfI):
     select_val = xb(Arg(0));
     goto do_binary_search;

 OpCase(i_select_val_yfI):
     select_val = yb(Arg(0));
     goto do_binary_search;
     
 OpCase(i_select_val_rfI):
     select_val = r(0);
     I--;

 do_binary_search:
 {
     struct Pairs {
	 BeamInstr val;
	 BeamInstr* addr;
     };
     struct Pairs* low;
     struct Pairs* high;
     struct Pairs* mid;
     int bdiff; /* int not long because the arrays aren't that large */

     low = (struct Pairs *) &Arg(3);
     high = low + Arg(2);

     /* The pointer subtraction (high-low) below must produce
      * a signed result, because high could be < low. That
      * requires the compiler to insert quite a bit of code.
      *
      * However, high will be > low so the result will be
      * positive. We can use that knowledge to optimise the
      * entire sequence, from the initial comparison to the
      * computation of mid.
      *
      * -- Mikael Pettersson, Acumem AB
      *
      * Original loop control code:
      *
      * while (low < high) {
      *    mid = low + (high-low) / 2;
      *
      */
     while ((bdiff = (int)((char*)high - (char*)low)) > 0) {
	 unsigned int boffset = ((unsigned int)bdiff >> 1) & ~(sizeof(struct Pairs)-1);

	 mid = (struct Pairs*)((char*)low + boffset);
	 if (select_val < mid->val) {
	     high = mid;
	 } else if (select_val > mid->val) {
	     low = mid + 1;
	 } else {
	     SET_I(mid->addr);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }
 }

 {
     Eterm jump_on_val_zero_index;
     
 OpCase(i_jump_on_val_zero_yfI):
     jump_on_val_zero_index = yb(Arg(0));
     goto do_jump_on_val_zero_index;

 OpCase(i_jump_on_val_zero_xfI):
     jump_on_val_zero_index = xb(Arg(0));
     goto do_jump_on_val_zero_index;

 OpCase(i_jump_on_val_zero_rfI):
     jump_on_val_zero_index = r(0);
     I--;

 do_jump_on_val_zero_index:
     if (is_small(jump_on_val_zero_index)) {
	 jump_on_val_zero_index = signed_val(jump_on_val_zero_index);
	 if (jump_on_val_zero_index < Arg(2)) {
	     SET_I((BeamInstr *) (&Arg(3))[jump_on_val_zero_index]);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }

 {
     Eterm jump_on_val_index;

 
 OpCase(i_jump_on_val_yfII):
     jump_on_val_index = yb(Arg(0));
     goto do_jump_on_val_index;

 OpCase(i_jump_on_val_xfII):
     jump_on_val_index = xb(Arg(0));
     goto do_jump_on_val_index;

 OpCase(i_jump_on_val_rfII):
     jump_on_val_index = r(0);
     I--;

 do_jump_on_val_index:
     if (is_small(jump_on_val_index)) {
	 jump_on_val_index = (Uint) (signed_val(jump_on_val_index) - Arg(3));
	 if (jump_on_val_index < Arg(2)) {
	     SET_I((BeamInstr *) (&Arg(4))[jump_on_val_index]);
	     Goto(*I);
	 }
     }
     SET_I((BeamInstr *) Arg(1));
     Goto(*I);
 }

 do_put_tuple: {
     Eterm* hp = HTOP;

     *hp++ = make_arityval(pt_arity);

     do {
	 Eterm term = *I++;
	 switch (term & _TAG_IMMED1_MASK) {
	 case (R_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:
	     *hp++ = r(0);
	     break;
	 case (X_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:
	     *hp++ = x(term >> _TAG_IMMED1_SIZE);
	     break;
	 case (Y_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:
	     *hp++ = y(term >> _TAG_IMMED1_SIZE);
	     break;
	 default:
	     *hp++ = term;
	     break;
	 }
     } while (--pt_arity != 0);
     HTOP = hp;
     Goto(*I);
 }

 OpCase(new_map_jdII): {
     Eterm res;

     x(0) = r(0);
     SWAPOUT;
     res = new_map(c_p, reg, I);
     SWAPIN;
     r(0) = x(0);
     StoreResult(res, Arg(1));
     Next(4+Arg(3));
 }

 OpCase(i_has_map_fields_fsI): {
    map_t* mp;
    Eterm map;
    Eterm field;
    Eterm *ks;
    BeamInstr* fs;
    Uint sz,n;

    GetArg1(1, map);

    /* this instruction assumes Arg1 is a map,
     * i.e. that it follows a test is_map if needed.
     */

    mp = (map_t *)map_val(map);
    sz = map_get_size(mp);

    if (sz == 0) {
	SET_I((BeamInstr *) Arg(0));
	goto has_map_fields_fail;
    }

    ks = map_get_keys(mp);
    n  = (Uint)Arg(2);
    fs = &Arg(3); /* pattern fields */

    ASSERT(n>0);

    while(sz) {
	field = (Eterm)*fs;
	if (EQ(field,*ks)) {
	    n--;
	    fs++;
	    if (n == 0) break;
	}
	ks++; sz--;
    }

    if (n) {
	SET_I((BeamInstr *) Arg(0));
	goto has_map_fields_fail;
    }

    I += 4 + Arg(2);
has_map_fields_fail:
    ASSERT(VALID_INSTR(*I));
    Goto(*I);
 }

#define PUT_TERM_REG(term, desc)				\
do {								\
    switch ((desc) & _TAG_IMMED1_MASK) {			\
    case (R_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	r(0) = (term);						\
	break;							\
    case (X_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	x((desc) >> _TAG_IMMED1_SIZE) = (term);			\
	break;							\
    case (Y_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	y((desc) >> _TAG_IMMED1_SIZE) = (term);			\
	break;							\
    default:							\
	ASSERT(0);						\
	break;							\
    }								\
} while(0)

 OpCase(i_get_map_elements_fsI): {
    Eterm map;
    map_t *mp;
    Eterm field;
    Eterm *ks;
    Eterm *vs;
    BeamInstr *fs;
    Uint sz,n;

    GetArg1(1, map);

    /* this instruction assumes Arg1 is a map,
     * i.e. that it follows a test is_map if needed.
     */

    mp = (map_t *)map_val(map);
    sz = map_get_size(mp);

    if (sz == 0) {
	SET_I((BeamInstr *) Arg(0));
	goto get_map_elements_fail;
    }

    n  = (Uint)Arg(2) / 2;
    fs = &Arg(3); /* pattern fields and target registers */
    ks = map_get_keys(mp);
    vs = map_get_values(mp);

    while(sz) {
	field = (Eterm)*fs;
	if (EQ(field,*ks)) {
	    PUT_TERM_REG(*vs, fs[1]);
	    n--;
	    fs += 2;
	    /* no more values to fetch, we are done */
	    if (n == 0) break;
	}
	ks++; sz--;
	vs++;
    }

    if (n) {
	SET_I((BeamInstr *) Arg(0));
	goto get_map_elements_fail;
    }

    I += 4 + Arg(2);
get_map_elements_fail:
    ASSERT(VALID_INSTR(*I));
    Goto(*I);
 }
#undef PUT_TERM_REG

 OpCase(update_map_assoc_jsdII): {
     Eterm res;
     Eterm map;

     GetArg1(1, map);
     x(0) = r(0);
     SWAPOUT;
     res = update_map_assoc(c_p, reg, map, I);
     SWAPIN;
     if (is_value(res)) {
	 r(0) = x(0);
	 StoreResult(res, Arg(2));
	 Next(5+Arg(4));
     } else {
	 goto badarg;
     }
 }

 OpCase(update_map_exact_jsdII): {
     Eterm res;
     Eterm map;

     GetArg1(1, map);
     x(0) = r(0);
     SWAPOUT;
     res = update_map_exact(c_p, reg, map, I);
     SWAPIN;
     if (is_value(res)) {
	 r(0) = x(0);
	 StoreResult(res, Arg(2));
	 Next(5+Arg(4));
     } else {
	 goto badarg;
     }
 }


    /*
     * All guards with zero arguments have special instructions:
     * 	self/0
     * 	node/0
     *
     * All other guard BIFs take one or two arguments.
     */

    /*
     * Guard BIF in head.  On failure, ignore the error and jump
     * to the code for the next clause.  We don't support tracing
     * of guard BIFs.
     */

 OpCase(bif1_fbsd):
    {
	Eterm (*bf)(Process*, Eterm*);
	Eterm tmp_reg[1];
	Eterm result;

	GetArg1(2, tmp_reg[0]);
	bf = (BifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_reg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(3, result);
	}
	SET_I((BeamInstr *) Arg(0));
	Goto(*I);
    }

    /*
     * Guard BIF in body.  It can fail like any BIF.  No trace support.
     */

 OpCase(bif1_body_bsd):
    {
	Eterm (*bf)(Process*, Eterm*);

	Eterm tmp_reg[1];
	Eterm result;

	GetArg1(1, tmp_reg[0]);
	bf = (BifFunction) Arg(0);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_reg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(2, result);
	}
	reg[0] = tmp_reg[0];
	SWAPOUT;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 OpCase(i_gc_bif1_jIsId):
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm arg;
	Eterm result;
	Uint live = (Uint) Arg(3);

	GetArg1(2, arg);
	reg[0] = r(0);
	reg[live] = arg;
	bf = (GcBifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	r(0) = reg[0];
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(4, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	reg[0] = arg;
	I = handle_error(c_p, I, reg, translate_gc_bif((void *) bf));
	goto post_error_handling;
    }

 OpCase(i_gc_bif2_jIId): /* Note, one less parameter than the i_gc_bif1
			    and i_gc_bif3 */
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm result;
	Uint live = (Uint) Arg(2);

	reg[0] = r(0);
	reg[live++] = tmp_arg1;
	reg[live] = tmp_arg2;
	bf = (GcBifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	r(0) = reg[0];
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(3, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	reg[0] = tmp_arg1;
	reg[1] = tmp_arg2;
	I = handle_error(c_p, I, reg, translate_gc_bif((void *) bf));
	goto post_error_handling;
    }

 OpCase(i_gc_bif3_jIsId):
    {
	typedef Eterm (*GcBifFunction)(Process*, Eterm*, Uint);
	GcBifFunction bf;
	Eterm arg;
	Eterm result;
	Uint live = (Uint) Arg(3);

	GetArg1(2, arg);
	reg[0] = r(0);
	reg[live++] = arg;
	reg[live++] = tmp_arg1;
	reg[live] = tmp_arg2;
	bf = (GcBifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	SWAPOUT;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	result = (*bf)(c_p, reg, live);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	SWAPIN;
	r(0) = reg[0];
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(4, result);
	}
	if (Arg(0) != 0) {
	    SET_I((BeamInstr *) Arg(0));
	    Goto(*I);
	}
	reg[0] = arg;
	reg[1] = tmp_arg1;
	reg[2] = tmp_arg2;
	I = handle_error(c_p, I, reg, translate_gc_bif((void *) bf));
	goto post_error_handling;
    }

 /*
  * Guards bifs and, or, xor in guards.
  */
 OpCase(i_bif2_fbd):
    {
	Eterm tmp_reg[2] = {tmp_arg1, tmp_arg2};
	Eterm (*bf)(Process*, Eterm*);
	Eterm result;

	bf = (BifFunction) Arg(1);
	c_p->fcalls = FCALLS;
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_reg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    StoreBifResult(2, result);
	}
	SET_I((BeamInstr *) Arg(0));
	Goto(*I);
    }

 /*
  * Guards bifs and, or, xor, relational operators in body.
  */
 OpCase(i_bif2_body_bd):
    {
	Eterm tmp_reg[2] = {tmp_arg1, tmp_arg2};
	Eterm (*bf)(Process*, Eterm*);
	Eterm result;

	bf = (BifFunction) Arg(0);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	result = (*bf)(c_p, tmp_reg);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	ERTS_HOLE_CHECK(c_p);
	if (is_value(result)) {
	    ASSERT(!is_CP(result));
	    StoreBifResult(1, result);
	}
	reg[0] = tmp_arg1;
	reg[1] = tmp_arg2;
	SWAPOUT;
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

    /*
     * The most general BIF call.  The BIF may build any amount of data
     * on the heap.  The result is always returned in r(0).
     */
 OpCase(call_bif_e):
    {
	Eterm (*bf)(Process*, Eterm*, BeamInstr*) = GET_BIF_ADDRESS(Arg(0));
	Eterm result;
	BeamInstr *next;

	ASSERT(epiphany_in_dram(bf));
	PRE_BIF_SWAPOUT(c_p);
	c_p->fcalls = FCALLS - 1;
        // ESTUB: No tracing
	PreFetch(1, next);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p));
	reg[0] = r(0);
	result = (*bf)(c_p, reg, I);
	ASSERT(!ERTS_PROC_IS_EXITING(c_p) || is_non_value(result));
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
	ERTS_HOLE_CHECK(c_p);
	ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	PROCESS_MAIN_CHK_LOCKS(c_p);
	if (c_p->mbuf || MSO(c_p).overhead >= BIN_VHEAP_SZ(c_p)) {
	    Uint arity = ((Export *)Arg(0))->code[2];
	    result = erts_gc_after_bif_call(c_p, result, reg, arity);
	    E = c_p->stop;
	}
	HTOP = HEAP_TOP(c_p);
	FCALLS = c_p->fcalls;
	if (is_value(result)) {
	    r(0) = result;
	    CHECK_TERM(r(0));
	    NextPF(1, next);
	} else if (c_p->freason == TRAP) {
	    SET_CP(c_p, I+2);
	    SET_I(c_p->i);
	    SWAPIN;
	    r(0) = reg[0];
	    Dispatch();
	}

	/*
	 * Error handling.  SWAPOUT is not needed because it was done above.
	 */
	ASSERT(c_p->stop == E);
	I = handle_error(c_p, I, reg, bf);
	goto post_error_handling;
    }

 /*
  * Arithmetic operations.
  */

 OpCase(i_times_jId):
 {
     arith_func = ARITH_FUNC(mixed_times);
     goto do_big_arith2;
 }

 OpCase(i_m_div_jId):
 {
     arith_func = ARITH_FUNC(mixed_div);
     goto do_big_arith2;
 }

 OpCase(i_int_div_jId):
 {
     Eterm result;

     if (tmp_arg2 == SMALL_ZERO) {
	 goto badarith;
     } else if (is_both_small(tmp_arg1, tmp_arg2)) {
	 Sint ires = signed_val(tmp_arg1) / signed_val(tmp_arg2);
	 if (MY_IS_SSMALL(ires)) {
	     result = make_small(ires);
	     STORE_ARITH_RESULT(result);
	 }
     }
     arith_func = ARITH_FUNC(int_div);
     goto do_big_arith2;
 }

 OpCase(i_rem_jId):
 {
     Eterm result;

     if (tmp_arg2 == SMALL_ZERO) {
	 goto badarith;
     } else if (is_both_small(tmp_arg1, tmp_arg2)) {
	 result = make_small(signed_val(tmp_arg1) % signed_val(tmp_arg2));
	 STORE_ARITH_RESULT(result);
     } else {
	 arith_func = ARITH_FUNC(int_rem);
	 goto do_big_arith2;
     }
 }

 OpCase(i_band_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * No need to untag -- TAG & TAG == TAG.
	  */
	 result = tmp_arg1 & tmp_arg2;
	 STORE_ARITH_RESULT(result);
     }
     arith_func = ARITH_FUNC(band);
     goto do_big_arith2;
 }

 do_big_arith2:
 {
     Eterm result;
     Uint live = Arg(1);

     SWAPOUT;
     reg[0] = r(0);
     reg[live] = tmp_arg1;
     reg[live+1] = tmp_arg2;
     result = arith_func(c_p, reg, live);
     r(0) = reg[0];
     SWAPIN;
     ERTS_HOLE_CHECK(c_p);
     if (is_value(result)) {
	 STORE_ARITH_RESULT(result);
     }
     goto lb_Cl_error;
 }

 /*
  * An error occured in an arithmetic operation or test that could
  * appear either in a head or in a body.
  * In a head, execution should continue at failure address in Arg(0).
  * In a body, Arg(0) == 0 and an exception should be raised.
  */
 lb_Cl_error: {
     if (Arg(0) != 0) {
	 OpCase(jump_f): {
	 jump_f:
	     SET_I((BeamInstr *) Arg(0));
	     Goto(*I);
	 }
     }
     ASSERT(c_p->freason != BADMATCH || is_value(c_p->fvalue));
     goto find_func_info;
 }

 OpCase(i_bor_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * No need to untag -- TAG | TAG == TAG.
	  */
	 result = tmp_arg1 | tmp_arg2;
	 STORE_ARITH_RESULT(result);
     }
     arith_func = ARITH_FUNC(bor);
     goto do_big_arith2;
 }

 OpCase(i_bxor_jId):
 {
     Eterm result;

     if (is_both_small(tmp_arg1, tmp_arg2)) {
	 /*
	  * We could extract the tag from one argument, but a tag extraction
	  * could mean a shift.  Therefore, play it safe here.
	  */
	 result = make_small(signed_val(tmp_arg1) ^ signed_val(tmp_arg2));
	 STORE_ARITH_RESULT(result);
     }
     arith_func = ARITH_FUNC(bxor);
     goto do_big_arith2;
 }

 {
     Sint i;
     Sint ires;
     Eterm* bigp;

     OpCase(i_bsr_jId):
	 if (is_small(tmp_arg2)) {
	     i = -signed_val(tmp_arg2);
	     if (is_small(tmp_arg1)) {
		 goto small_shift;
	     } else if (is_big(tmp_arg1)) {
		 if (i == 0) {
		     StoreBifResult(2, tmp_arg1);
		 }
		 goto big_shift;
	     }
	 } else if (is_big(tmp_arg2)) {
	     /*
	      * N bsr NegativeBigNum == N bsl MAX_SMALL
	      * N bsr PositiveBigNum == N bsl MIN_SMALL
	      */
	     tmp_arg2 = make_small(bignum_header_is_neg(*big_val(tmp_arg2)) ?
				   MAX_SMALL : MIN_SMALL);
	     goto do_bsl;
	}
     goto badarith;
     
     OpCase(i_bsl_jId):
 do_bsl:
	 if (is_small(tmp_arg2)) {
	     i = signed_val(tmp_arg2);

	     if (is_small(tmp_arg1)) {
	     small_shift:
		 ires = signed_val(tmp_arg1);
	     
		 if (i == 0 || ires == 0) {
		     StoreBifResult(2, tmp_arg1);
		 } else if (i < 0)  { /* Right shift */
		     i = -i;
		     if (i >= SMALL_BITS-1) {
			 tmp_arg1 = (ires < 0) ? SMALL_MINUS_ONE : SMALL_ZERO;
		     } else {
			 tmp_arg1 = make_small(ires >> i);
		     }
		     StoreBifResult(2, tmp_arg1);
		 } else if (i < SMALL_BITS-1) { /* Left shift */
		     if ((ires > 0 && ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ires) == 0) ||
			 ((~(Uint)0 << ((SMALL_BITS-1)-i)) & ~ires) == 0) {
			 tmp_arg1 = make_small(ires << i);
			 StoreBifResult(2, tmp_arg1);
		     }
		 }
		 tmp_arg1 = small_to_big(ires, tmp_big);

	     big_shift:
		 if (i > 0) {	/* Left shift. */
		     ires = big_size(tmp_arg1) + (i / D_EXP);
		 } else {	/* Right shift. */
		     ires = big_size(tmp_arg1);
		     if (ires <= (-i / D_EXP))
			 ires = 3; /* ??? */
		     else
			 ires -= (-i / D_EXP);
		 }
		 {
		     ires = BIG_NEED_SIZE(ires+1);
		     /*
		      * Slightly conservative check the size to avoid
		      * allocating huge amounts of memory for bignums that 
		      * clearly would overflow the arity in the header
		      * word.
		      */
		     if (ires-8 > BIG_ARITY_MAX) {
			 c_p->freason = SYSTEM_LIMIT;
			 goto lb_Cl_error;
		     }
		     TestHeapPreserve(ires+1, Arg(1), tmp_arg1);
		     bigp = HTOP;
		     tmp_arg1 = big_lshift(tmp_arg1, i, bigp);
		     if (is_big(tmp_arg1)) {
			 HTOP += bignum_header_arity(*HTOP) + 1;
		     }
		     HEAP_SPACE_VERIFIED(0);
		     if (is_nil(tmp_arg1)) {
			 /*
			  * This result must have been only slight larger
			  * than allowed since it wasn't caught by the
			  * previous test.
			  */
			 c_p->freason = SYSTEM_LIMIT;
			 goto lb_Cl_error;
		     }
		     ERTS_HOLE_CHECK(c_p);
		     StoreBifResult(2, tmp_arg1);
		 }
	     } else if (is_big(tmp_arg1)) {
		 if (i == 0) {
		     StoreBifResult(2, tmp_arg1);
		 }
		 goto big_shift;
	     }
	 } else if (is_big(tmp_arg2)) {
	     if (bignum_header_is_neg(*big_val(tmp_arg2))) {
		 /*
		  * N bsl NegativeBigNum is either 0 or -1, depending on
		  * the sign of N. Since we don't believe this case
		  * is common, do the calculation with the minimum
		  * amount of code.
		  */
		 tmp_arg2 = make_small(MIN_SMALL);
		 goto do_bsl;
	     } else if (is_small(tmp_arg1) || is_big(tmp_arg1)) {
		 /*
		  * N bsl PositiveBigNum is too large to represent.
		  */
		 c_p->freason = SYSTEM_LIMIT;
		 goto lb_Cl_error;
	     }
	     /* Fall through if the left argument is not an integer. */
	 }
     /*
      * One or more non-integer arguments.
      */
     goto badarith;
 }

 OpCase(i_int_bnot_jsId):
 {
     Eterm bnot_val;

     GetArg1(1, bnot_val);
     if (is_small(bnot_val)) {
	 bnot_val = make_small(~signed_val(bnot_val));
     } else {
	 Uint live = Arg(2);
	 SWAPOUT;
	 reg[0] = r(0);
	 reg[live] = bnot_val;
	 bnot_val = erts_gc_bnot(c_p, reg, live);
	 r(0) = reg[0];
	 SWAPIN;
	 ERTS_HOLE_CHECK(c_p);
	 if (is_nil(bnot_val)) {
	     goto lb_Cl_error;
	 }
     }
     StoreBifResult(3, bnot_val);
 }

 badarith:
    c_p->freason = BADARITH;
    goto lb_Cl_error;

 OpCase(i_apply): {
     BeamInstr *next;
     SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_last_P): {
     BeamInstr *next;
     SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (BeamInstr *) EXPAND_POINTER(E[0]));
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_only): {
     BeamInstr *next;
     SWAPOUT;
     next = apply(c_p, r(0), x(1), x(2), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(apply_I): {
     BeamInstr *next;

     reg[0] = r(0);
     SWAPOUT;
     next = fixed_apply(c_p, reg, Arg(0));
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(apply_last_IP): {
     BeamInstr *next;

     reg[0] = r(0);
     SWAPOUT;
     next = fixed_apply(c_p, reg, Arg(0));
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (BeamInstr *) EXPAND_POINTER(E[0]));
	 E = ADD_BYTE_OFFSET(E, Arg(1));
	 SET_I(next);
	 Dispatch();
     }
     I = handle_error(c_p, I, reg, apply_3);
     goto post_error_handling;
 }

 OpCase(i_apply_fun): {
     BeamInstr *next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+1);
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_last_P): {
     BeamInstr *next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, (BeamInstr *) EXPAND_POINTER(E[0]));
	 E = ADD_BYTE_OFFSET(E, Arg(0));
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_apply_fun_only): {
     BeamInstr *next;

     SWAPOUT;
     next = apply_fun(c_p, r(0), x(1), reg);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_I): {
     BeamInstr *next;

     SWAPOUT;
     reg[0] = r(0);

     next = call_fun(c_p, Arg(0), reg, THE_NON_VALUE);
     SWAPIN;
     if (next != NULL) {
	 r(0) = reg[0];
	 SET_CP(c_p, I+2);
	 SET_I(next);
	 Dispatchfun();
     }
     goto find_func_info;
 }

 OpCase(i_call_fun_last_IP): {
     BeamInstr *next;

     SWAPOUT;
     reg[0] = r(0);
     next = call_fun(c_p, Arg(0), reg, THE_NON_VALUE);
     SWAPIN;
     if (next != NULL) {
	r(0) = reg[0];
	SET_CP(c_p, (BeamInstr *) EXPAND_POINTER(E[0]));
	E = ADD_BYTE_OFFSET(E, Arg(1));
	SET_I(next);
	Dispatchfun();
     }
     goto find_func_info;
 }

#ifdef DEBUG
    /*
     * Set a breakpoint here to get control just after a call instruction.
     * I points to the first instruction in the called function.
     *
     * In gdb, use 'call dis(I-5, 1)' to show the name of the function.
     */
 do_dispatch:
     DispatchMacro();

 do_dispatchx:
     DispatchMacrox();

 do_dispatchfun:
     DispatchMacroFun();

#endif

    /*
     * Jumped to from the Dispatch() macro when the reductions are used up.
     *
     * Since the I register points just beyond the FuncBegin instruction, we
     * can get the module, function, and arity for the function being
     * called from I[-3], I[-2], and I[-1] respectively.
     */
 context_switch_fun:
    c_p->arity = I[-1] + 1;
    goto context_switch2;

 context_switch:
    c_p->arity = I[-1];

 context_switch2:		/* Entry for fun calls. */
    c_p->current = I-3;		/* Pointer to Mod, Func, Arity */

 {
     Eterm* argp;
     int i;

     erts_printf("Context switch!\n");

     /*
      * Make sure that there is enough room for the argument registers to be saved.
      */
     if (c_p->arity > c_p->max_arg_reg) {
	 /*
	  * Yes, this is an expensive operation, but you only pay it the first
	  * time you call a function with more than 6 arguments which is
	  * scheduled out.  This is better than paying for 26 words of wasted
	  * space for most processes which never call functions with more than
	  * 6 arguments.
	  */
	 Uint size = c_p->arity * sizeof(c_p->arg_reg[0]);
	 if (c_p->arg_reg != c_p->def_arg_reg) {
	     c_p->arg_reg = (Eterm *) erts_realloc(ERTS_ALC_T_ARG_REG,
						   (void *) c_p->arg_reg,
						   size);
	 } else {
	     c_p->arg_reg = (Eterm *) erts_alloc(ERTS_ALC_T_ARG_REG, size);
	 }
	 c_p->max_arg_reg = c_p->arity;
     }

     /*
      * Since REDS_IN(c_p) is stored in the save area (c_p->arg_reg) we must read it
      * now before saving registers.
      *
      * The '+ 1' compensates for the last increment which was not done
      * (beacuse the code for the Dispatch() macro becomes shorter that way).
      */

     reds_used = REDS_IN(c_p) - FCALLS + 1;
     
     /*
      * Save the argument registers and everything else.
      */

     argp = c_p->arg_reg;
     for (i = c_p->arity - 1; i > 0; i--) {
	 argp[i] = reg[i];
     }
     c_p->arg_reg[0] = r(0);
     SWAPOUT;
     c_p->i = I;
     goto do_schedule1;
 }

 OpCase(set_tuple_element_sdP): {
     Eterm element;
     Eterm tuple;
     BeamInstr *next;
     Eterm* p;
     
     PreFetch(3, next);
     GetArg2(0, element, tuple);
     ASSERT(is_tuple(tuple));
     p = (Eterm *) ((unsigned char *) tuple_val(tuple) + Arg(2));
     *p = element;
     NextPF(3, next);
 }

 OpCase(i_is_ne_exact_f):
    if (EQ(tmp_arg1, tmp_arg2)) {
	ClauseFail();
    }
    Next(1);

 OpCase(normal_exit): {
     SWAPOUT;
     c_p->freason = EXC_NORMAL;
     c_p->arity = 0;		/* In case this process will ever be garbed again. */
     ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
     erts_do_exit_process(c_p, am_normal);
     ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
     goto do_schedule;
 }

 OpCase(continue_exit): {
        EPIPHANY_STUB(OpCase(continue_exit));
 }

 OpCase(raise_ss): {
     /* This was not done very well in R10-0; then, we passed the tag in
	the first argument and hoped that the existing c_p->ftrace was
	still correct. But the ftrace-object already includes the tag
	(or rather, the freason). Now, we pass the original ftrace in
	the first argument. We also handle atom tags in the first
	argument for backwards compatibility.
     */
     Eterm raise_val1;
     Eterm raise_val2;
     GetArg2(0, raise_val1, raise_val2);
     c_p->fvalue = raise_val2;
     if (c_p->freason == EXC_NULL) {
       /* a safety check for the R10-0 case; should not happen */
       c_p->ftrace = NIL;
       c_p->freason = EXC_ERROR;
     }
     /* for R10-0 code, keep existing c_p->ftrace and hope it's correct */
     switch (raise_val1) {
     case am_throw:
       c_p->freason = EXC_THROWN & ~EXF_SAVETRACE;
       break;
     case am_error:
       c_p->freason = EXC_ERROR & ~EXF_SAVETRACE;
       break;
     case am_exit:
       c_p->freason = EXC_EXIT & ~EXF_SAVETRACE;
       break;
     default:
       {/* R10-1 and later
	   XXX note: should do sanity check on given trace if it can be
	   passed from a user! Currently only expecting generated calls.
	*/
	 struct StackTrace *s;
	 c_p->ftrace = raise_val1;
	 s = get_trace_from_exc(raise_val1);
	 if (s == NULL) {
	   c_p->freason = EXC_ERROR;
	 } else {
	   c_p->freason = PRIMARY_EXCEPTION(s->freason);
	 }
       }
     }
     goto find_func_info;
 }

    {
	Eterm badmatch_val;

    OpCase(badmatch_y):
	badmatch_val = yb(Arg(0));
	goto do_badmatch;

    OpCase(badmatch_x):
	badmatch_val = xb(Arg(0));
	goto do_badmatch;

    OpCase(badmatch_r):
	badmatch_val = r(0);

    do_badmatch:
	c_p->fvalue = badmatch_val;
	c_p->freason = BADMATCH;
    }
 /* Fall through here */

 find_func_info: {
     reg[0] = r(0);
     SWAPOUT;
     I = handle_error(c_p, I, reg, NULL);
     goto post_error_handling;
 }

 OpCase(call_error_handler):
    /*
     * At this point, I points to the code[3] in the export entry for
     * a function which is not loaded.
     *
     * code[0]: Module
     * code[1]: Function
     * code[2]: Arity
     * code[3]: &&call_error_handler
     * code[4]: Not used
     */
    SWAPOUT;
    reg[0] = r(0);
    I = call_error_handler(c_p, I-3, reg, am_undefined_function);
    r(0) = reg[0];
    SWAPIN;
    if (I) {
	Goto(*I);
    }

 /* Fall through */
 OpCase(error_action_code): {
    handle_error:
     reg[0] = r(0);
     SWAPOUT;
     I = handle_error(c_p, NULL, reg, NULL);
 post_error_handling:
     if (I == 0) {
	 goto do_schedule;
     } else {
	 r(0) = reg[0];
	 ASSERT(!is_value(r(0)));
	 if (c_p->mbuf) {
	     erts_garbage_collect(c_p, 0, reg+1, 3);
	 }
	 SWAPIN;
	 Goto(*I);
     }
 }

    {
	Eterm nif_bif_result;
	Eterm bif_nif_arity;

    OpCase(call_nif):
	{
	    BifFunction vbf;
            EPIPHANY_STUB(OpCase(call_nif));
	 
	OpCase(apply_bif):
	    /*
	     * At this point, I points to the code[3] in the export entry for
	     * the BIF:
	     *
	     * code[0]: Module
	     * code[1]: Function
	     * code[2]: Arity
	     * code[3]: &&apply_bif
	     * code[4]: Function pointer to BIF function
	     */

	    c_p->current = I-3;	/* In case we apply process_info/1,2 or load_nif/1 */
	    c_p->i = I;		/* In case we apply check_process_code/2. */
	    c_p->arity = 0;		/* To allow garbage collection on ourselves
					 * (check_process_code/2).
					 */
	    DTRACE_BIF_ENTRY(c_p, (Eterm)I[-3], (Eterm)I[-2], (Uint)I[-1]);

	    SWAPOUT;
	    c_p->fcalls = FCALLS - 1;
	    vbf = (BifFunction) Arg(0);
	    PROCESS_MAIN_CHK_LOCKS(c_p);
	    bif_nif_arity = I[-1];
	    ASSERT(bif_nif_arity <= 3);
	    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
	    reg[0] = r(0);
	    {
		Eterm (*bf)(Process*, Eterm*, BeamInstr*) = vbf;
		ASSERT(!ERTS_PROC_IS_EXITING(c_p));
		nif_bif_result = (*bf)(c_p, reg, I);
		ASSERT(!ERTS_PROC_IS_EXITING(c_p) ||
		       is_non_value(nif_bif_result));
		ERTS_VERIFY_UNUSED_TEMP_ALLOC(c_p);
		PROCESS_MAIN_CHK_LOCKS(c_p);
	    }

	    DTRACE_BIF_RETURN(c_p, (Eterm)I[-3], (Eterm)I[-2], (Uint)I[-1]);

            // apply_bif_or_nif_epilogue:
	    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
	    ERTS_HOLE_CHECK(c_p);
	    if (c_p->mbuf) {
		nif_bif_result = erts_gc_after_bif_call(c_p, nif_bif_result,
						  reg, bif_nif_arity);
	    }
	    SWAPIN;  /* There might have been a garbage collection. */
	    FCALLS = c_p->fcalls;
	    if (is_value(nif_bif_result)) {
		r(0) = nif_bif_result;
		CHECK_TERM(r(0));
		SET_I(c_p->cp);
		c_p->cp = 0;
		Goto(*I);
	    } else if (c_p->freason == TRAP) {
		SET_I(c_p->i);
		r(0) = reg[0];
		if (c_p->flags & F_HIBERNATE_SCHED) {
		    c_p->flags &= ~F_HIBERNATE_SCHED;
		    goto do_schedule;
		}
		Dispatch();
	    }
	    I = handle_error(c_p, c_p->cp, reg, vbf);
	    goto post_error_handling;
	}
    }

 OpCase(i_get_sd):
    EPIPHANY_STUB(OpCase(i_get_sd));

    {
	Eterm case_end_val;

    OpCase(case_end_x):
	case_end_val = xb(Arg(0));
	goto do_case_end;

    OpCase(case_end_y):
	case_end_val = yb(Arg(0));
	goto do_case_end;

    OpCase(case_end_r):
	case_end_val = r(0);

    do_case_end:
	c_p->fvalue = case_end_val;
	c_p->freason = EXC_CASE_CLAUSE;
	goto find_func_info;
    }

 OpCase(if_end):
    c_p->freason = EXC_IF_CLAUSE;
    goto find_func_info;

 OpCase(i_func_info_IaaI): {
     c_p->freason = EXC_FUNCTION_CLAUSE;
     c_p->current = I + 2;
     goto handle_error;
 }

 OpCase(try_case_end_s):
    {
	Eterm try_case_end_val;
	GetArg1(0, try_case_end_val);
	c_p->fvalue = try_case_end_val;
	c_p->freason = EXC_TRY_CLAUSE;
	goto find_func_info;
    }

 OpCase(i_bs_init_bits_heap_IIId):
 OpCase(i_bs_init_bits_IId):
 OpCase(i_bs_init_bits_fail_heap_IjId):
 OpCase(i_bs_init_bits_fail_rjId):
 OpCase(i_bs_init_bits_fail_yjId):
 OpCase(i_bs_init_bits_fail_xjId):
    EPIPHANY_STUB(i_bs_init_bits_);

 OpCase(i_bs_init_fail_heap_IjId):
 OpCase(i_bs_init_fail_rjId):
 OpCase(i_bs_init_fail_yjId):
 OpCase(i_bs_init_fail_xjId):
 OpCase(i_bs_init_heap_IIId):
 OpCase(i_bs_init_IId):
 OpCase(i_bs_init_heap_bin_heap_IIId):
 OpCase(i_bs_init_heap_bin_IId):
    EPIPHANY_STUB(OpCase(i_bs_init_));

 OpCase(i_bs_add_jId):
    EPIPHANY_STUB(OpCase(i_bs_add_jId));

 OpCase(bs_put_string_II):
    EPIPHANY_STUB(OpCase(bs_put_string_II));

 OpCase(i_bs_append_jIIId):
    EPIPHANY_STUB(OpCase(i_bs_append_jIIId));

 OpCase(i_bs_private_append_jId):
    EPIPHANY_STUB(OpCase(i_bs_private_append_jId));

 OpCase(bs_init_writable):
    EPIPHANY_STUB(OpCase(bs_init_writable));

 OpCase(i_bs_utf8_size_sd):
    EPIPHANY_STUB(OpCase(i_bs_utf8_size_s));

 OpCase(i_bs_put_utf8_js):
    EPIPHANY_STUB(OpCase(i_bs_put_utf8_js));

 OpCase(i_bs_utf16_size_sd):
    EPIPHANY_STUB(OpCase(i_bs_utf16_size_sd));

 OpCase(i_bs_put_utf16_jIs):
    EPIPHANY_STUB(OpCase(i_bs_put_utf16_jIs));

 OpCase(i_bs_validate_unicode_js):
 OpCase(i_bs_validate_unicode_retract_j):
    EPIPHANY_STUB(OpCase(i_bs_validate_unicode_));

 OpCase(i_bs_start_match2_rfIId):
 OpCase(i_bs_start_match2_xfIId):
 OpCase(i_bs_start_match2_yfIId):
    EPIPHANY_STUB(OpCase(i_bs_start_match2_));

 OpCase(bs_test_zero_tail2_fr):
 OpCase(bs_test_zero_tail2_fx):
 OpCase(bs_test_tail_imm2_frI):
 OpCase(bs_test_tail_imm2_fxI):
 OpCase(bs_test_unit_frI):
 OpCase(bs_test_unit_fxI):
 OpCase(bs_test_unit8_fr):
 OpCase(bs_test_unit8_fx):
    EPIPHANY_STUB(OpCase(bs_test_));

 OpCase(i_bs_get_integer_8_rfd):
 OpCase(i_bs_get_integer_8_xfd):
 OpCase(i_bs_get_integer_16_rfd):
 OpCase(i_bs_get_integer_16_xfd):
 OpCase(i_bs_get_integer_32_rfId):
 OpCase(i_bs_get_integer_32_xfId):
 OpCase(i_bs_get_integer_imm_rIIfId):
 OpCase(i_bs_get_integer_imm_xIIfId):
 OpCase(i_bs_get_integer_small_imm_rIfId):
 OpCase(i_bs_get_integer_small_imm_xIfId):
 OpCase(i_bs_get_integer_fIId):
    EPIPHANY_STUB(OpCase(i_bs_get_integer_));

 OpCase(i_bs_get_utf8_rfd):
 OpCase(i_bs_get_utf8_xfd):
 OpCase(i_bs_get_utf16_rfId):
 OpCase(i_bs_get_utf16_xfId):
    EPIPHANY_STUB(OpCase(i_bs_get_utf));

 OpCase(bs_context_to_binary_r):
 OpCase(bs_context_to_binary_y):
 OpCase(bs_context_to_binary_x):
    EPIPHANY_STUB(OpCase(bs_context_to_binary_));

 OpCase(i_bs_get_binary_all_reuse_rfI):
 OpCase(i_bs_get_binary_all_reuse_xfI):
 OpCase(i_bs_match_string_rfII):
 OpCase(i_bs_match_string_xfII):
 OpCase(i_bs_save2_rI):
 OpCase(i_bs_save2_xI):
 OpCase(i_bs_restore2_rI):
 OpCase(i_bs_restore2_xI):
    EPIPHANY_STUB(OpCase(i_bs_));

#pragma GCC diagnostic push
    // beam_cold.h uses the macros we stubbed out above. Since the macros do not
    // use their arguments, we'll get a lot of "set but not used warnings"
#pragma GCC diagnostic ignored "-Wunused-but-set-variable"
#include "beam_cold.h"
#pragma GCC diagnostic pop


 /*
  * This instruction is probably never used (because it is combined with a
  * a return). However, a future compiler might for some reason emit a
  * deallocate not followed by a return, and that should work.
  */
 OpCase(deallocate_I): {
     BeamInstr *next;

     PreFetch(1, next);
     D(Arg(0));
     NextPF(1, next);
 }

    /*
     * Trace and debugging support.
     */

 OpCase(return_trace):
    EPIPHANY_STUB(OpCase(return_trace));

 OpCase(i_generic_breakpoint):
    EPIPHANY_STUB(OpCase(i_generic_breakpoint));

 OpCase(i_return_time_trace):
    EPIPHANY_STUB(OpCase(i_return_time_trace));

 OpCase(i_return_to_trace):
    EPIPHANY_STUB(OpCase(i_return_to_trace));

 /*
  * New floating point instructions.
  */

 OpCase(fmove_ql): {
     Eterm fr = Arg(1);
     BeamInstr *next;

     PreFetch(2, next);
     GET_DOUBLE(Arg(0), *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     NextPF(2, next);
 }

 OpCase(fmove_dl): {
     Eterm targ1;
     Eterm fr = Arg(1);
     BeamInstr *next;

     PreFetch(2, next);
     GetR(0, targ1);
     /* Arg(0) == HEADER_FLONUM */
     GET_DOUBLE(targ1, *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     NextPF(2, next);
 }

 OpCase(fmove_ld): {
     Eterm fr = Arg(0);
     Eterm dest = make_float(HTOP);

     PUT_DOUBLE(*(FloatDef*)ADD_BYTE_OFFSET(freg, fr), HTOP);
     HTOP += FLOAT_SIZE_OBJECT;
     StoreBifResult(1, dest);
 }

 OpCase(fconv_dl): {
     Eterm targ1;
     Eterm fr = Arg(1);
     BeamInstr *next;

     GetR(0, targ1);
     PreFetch(2, next);
     if (is_small(targ1)) {
	 fb(fr) = (double) signed_val(targ1);
     } else if (is_big(targ1)) {
	 if (big_to_double(targ1, &fb(fr)) < 0) {
	     goto fbadarith;
	 }
     } else if (is_float(targ1)) {
	 GET_DOUBLE(targ1, *(FloatDef*)ADD_BYTE_OFFSET(freg, fr));
     } else {
	 goto fbadarith;
     }
     NextPF(2, next);
 }

#ifdef NO_FPE_SIGNALS
     OpCase(fclearerror):
     OpCase(i_fcheckerror):
	 erl_exit(1, "fclearerror/i_fcheckerror without fpe signals (beam_emu)");
#  define ERTS_NO_FPE_CHECK_INIT ERTS_FP_CHECK_INIT
#  define ERTS_NO_FPE_ERROR ERTS_FP_ERROR
#else
#  define ERTS_NO_FPE_CHECK_INIT(p)
#  define ERTS_NO_FPE_ERROR(p, a, b)

     OpCase(fclearerror): {
	 BeamInstr *next;

	 PreFetch(0, next);
	 ERTS_FP_CHECK_INIT(c_p);
	 NextPF(0, next);
     }

     OpCase(i_fcheckerror): {
	 BeamInstr *next;

	 PreFetch(0, next);
	 ERTS_FP_ERROR(c_p, freg[0].fd, goto fbadarith);
	 NextPF(0, next);
     }
#endif


 OpCase(i_fadd_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) + fb(Arg(1));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fsub_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) - fb(Arg(1));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fmul_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) * fb(Arg(1));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fdiv_lll): {
     BeamInstr *next;

     PreFetch(3, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(2)) = fb(Arg(0)) / fb(Arg(1));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(2)), goto fbadarith);
     NextPF(3, next);
 }
 OpCase(i_fnegate_ll): {
     BeamInstr *next;

     PreFetch(2, next);
     ERTS_NO_FPE_CHECK_INIT(c_p);
     fb(Arg(1)) = -fb(Arg(0));
     ERTS_NO_FPE_ERROR(c_p, fb(Arg(1)), goto fbadarith);
     NextPF(2, next);

 fbadarith:
     c_p->freason = BADARITH;
     goto find_func_info;
 }

#ifdef HIPE
 {
     unsigned cmd;

     OpCase(hipe_trap_call): {
	 /*
	  * I[-5]: &&lb_i_func_info_IaaI
	  * I[-4]: Native code callee (inserted by HiPE)
	  * I[-3]: Module (tagged atom)
	  * I[-2]: Function (tagged atom)
	  * I[-1]: Arity (untagged integer)
	  * I[ 0]: &&lb_hipe_trap_call
	  * ... remainder of original BEAM code
	  */
	 ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
	 c_p->hipe.ncallee = (void(*)(void)) I[-4];
	 cmd = HIPE_MODE_SWITCH_CMD_CALL | (I[-1] << 8);
	 ++hipe_trap_count;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_call_closure): {
       ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
       c_p->hipe.ncallee = (void(*)(void)) I[-4];
       cmd = HIPE_MODE_SWITCH_CMD_CALL_CLOSURE | (I[-1] << 8);
       ++hipe_trap_count;
       goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_return): {
	 cmd = HIPE_MODE_SWITCH_CMD_RETURN;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_throw): {
	 cmd = HIPE_MODE_SWITCH_CMD_THROW;
	 goto L_hipe_mode_switch;
     }
     OpCase(hipe_trap_resume): {
	 cmd = HIPE_MODE_SWITCH_CMD_RESUME;
	 goto L_hipe_mode_switch;
     }
 L_hipe_mode_switch:
     /* XXX: this abuse of def_arg_reg[] is horrid! */
     SWAPOUT;
     c_p->fcalls = FCALLS;
     c_p->def_arg_reg[4] = -neg_o_reds;
     reg[0] = r(0);
     c_p = hipe_mode_switch(c_p, cmd, reg);
     reg = ERTS_PROC_GET_SCHDATA(c_p)->x_reg_array;
     freg = ERTS_PROC_GET_SCHDATA(c_p)->f_reg_array;
     ERL_BITS_RELOAD_STATEP(c_p);
     neg_o_reds = -c_p->def_arg_reg[4];
     FCALLS = c_p->fcalls;
     SWAPIN;
     switch( c_p->def_arg_reg[3] ) { /* Halfword wont work with hipe yet! */
       case HIPE_MODE_SWITCH_RES_RETURN:
	 ASSERT(is_value(reg[0]));
	 MoveReturn(reg[0], r(0));
       case HIPE_MODE_SWITCH_RES_CALL:
	 SET_I(c_p->i);
	 r(0) = reg[0];
	 Dispatch();
       case HIPE_MODE_SWITCH_RES_CALL_CLOSURE:
	 /* This can be used to call any function value, but currently it's
	    only used to call closures referring to unloaded modules. */
	 {
	     BeamInstr *next;

	     next = call_fun(c_p, c_p->arity - 1, reg, THE_NON_VALUE);
	     SWAPIN;
	     if (next != NULL) {
		 r(0) = reg[0];
		 SET_I(next);
		 Dispatchfun();
	     }
	     goto find_func_info;
	 }
       case HIPE_MODE_SWITCH_RES_THROW:
	 c_p->cp = NULL;
	 I = handle_error(c_p, I, reg, NULL);
	 goto post_error_handling;
       default:
	 erl_exit(1, "hipe_mode_switch: result %u\n", c_p->def_arg_reg[3]);
     }
 }
 OpCase(hipe_call_count): {
     /*
      * I[-5]: &&lb_i_func_info_IaaI
      * I[-4]: pointer to struct hipe_call_count (inserted by HiPE)
      * I[-3]: Module (tagged atom)
      * I[-2]: Function (tagged atom)
      * I[-1]: Arity (untagged integer)
      * I[ 0]: &&lb_hipe_call_count
      * ... remainder of original BEAM code
      */
     struct hipe_call_count *hcc = (struct hipe_call_count*)I[-4];
     ASSERT(I[-5] == (Uint) OpCode(i_func_info_IaaI));
     ASSERT(hcc != NULL);
     ASSERT(VALID_INSTR(hcc->opcode));
     ++(hcc->count);
     Goto(hcc->opcode);
 }
#endif /* HIPE */

 OpCase(i_yield):
 {
     /* This is safe as long as REDS_IN(c_p) is never stored 
      * in c_p->arg_reg[0]. It is currently stored in c_p->def_arg_reg[5],
      * which may be c_p->arg_reg[5], which is close, but no banana.
      */
     c_p->arg_reg[0] = am_true;
     c_p->arity = 1; /* One living register (the 'true' return value) */
     SWAPOUT;
     c_p->i = I + 1; /* Next instruction */
     c_p->current = NULL;
     goto do_schedule;
 }

 OpCase(i_hibernate): {
     SWAPOUT;
     if (erts_hibernate(c_p, r(0), x(1), x(2), reg)) {
	 c_p->flags &= ~F_HIBERNATE_SCHED;
	 goto do_schedule;
     } else {
	 I = handle_error(c_p, I, reg, hibernate_3);
	 goto post_error_handling;
     }
 }

 OpCase(i_debug_breakpoint): {
     SWAPOUT;
     reg[0] = r(0);
     I = call_error_handler(c_p, I-3, reg, am_breakpoint);
     r(0) = reg[0];
     SWAPIN;
     if (I) {
	 Goto(*I);
     }
     goto handle_error;
 }


 OpCase(system_limit_j):
    c_p->freason = SYSTEM_LIMIT;
    goto lb_Cl_error;

#ifdef ERTS_OPCODE_COUNTER_SUPPORT
    DEFINE_COUNTING_LABELS;
#endif

#ifndef NO_JUMP_TABLE
#ifdef DEBUG
 end_emulator_loop:
#endif
#endif

 OpCase(int_code_end):
 OpCase(label_L):
 OpCase(on_load):
 OpCase(line_I):
    erl_exit(1, "meta op\n");

    /*
     * One-time initialization of Beam emulator.
     */

 init_emulator:
 {

#ifndef NO_JUMP_TABLE
#ifdef ERTS_OPCODE_COUNTER_SUPPORT

     /* Are tables correctly generated by beam_makeops? */
     ASSERT(sizeof(counting_opcodes) == sizeof(opcodes));

     if (count_instructions) {
#ifdef DEBUG
	 counting_opcodes[op_catch_end_y] = LabelAddr(lb_catch_end_y);
#endif
	 counting_opcodes[op_i_func_info_IaaI] = LabelAddr(lb_i_func_info_IaaI);
	 beam_ops = counting_opcodes;
     }
     else
#endif /* #ifndef ERTS_OPCODE_COUNTER_SUPPORT */
     {
	 beam_ops = opcodes;
     }
#endif /* NO_JUMP_TABLE */
     
     em_call_error_handler = OpCode(call_error_handler);
     em_apply_bif = OpCode(apply_bif);
     em_call_nif = OpCode(call_nif);

     beam_apply[0]             = (BeamInstr) OpCode(i_apply);
     beam_apply[1]             = (BeamInstr) OpCode(normal_exit);
     beam_exit[0]              = (BeamInstr) OpCode(error_action_code);
     beam_continue_exit[0]     = (BeamInstr) OpCode(continue_exit);
     beam_return_to_trace[0]   = (BeamInstr) OpCode(i_return_to_trace);
     beam_return_trace[0]      = (BeamInstr) OpCode(return_trace);
     beam_exception_trace[0]   = (BeamInstr) OpCode(return_trace); /* UGLY */
     beam_return_time_trace[0] = (BeamInstr) OpCode(i_return_time_trace);

     // ESTUB: erl_bif_table, export
     /* int i; */
     /* Export* ep; */
     /* /\* */
     /*  * Enter all BIFs into the export table. */
     /*  *\/ */
     /* for (i = 0; i < BIF_SIZE; i++) { */
     /*     ep = erts_export_put(bif_table[i].module, */
     /*    		      bif_table[i].name, */
     /*    		      bif_table[i].arity); */
     /*     bif_export[i] = ep; */
     /*     ep->code[3] = (BeamInstr) OpCode(apply_bif); */
     /*     ep->code[4] = (BeamInstr) bif_table[i].f; */
     /*     /\* XXX: set func info for bifs *\/ */
     /*     ep->fake_op_func_info_for_hipe[0] = (BeamInstr) BeamOp(op_i_func_info_IaaI); */
     /* } */

     return;
 }
#ifdef NO_JUMP_TABLE
 default:
    erl_exit(1, "unexpected op code %d\n",Go);
  }
#endif
    return;			/* Never executed */
}

static BifFunction
translate_gc_bif(void* gcf)
{
    if (gcf == erts_gc_length_1) {
	return length_1;
    } else if (gcf == erts_gc_size_1) {
	return size_1;
    } else if (gcf == erts_gc_bit_size_1) {
	return bit_size_1;
    } else if (gcf == erts_gc_byte_size_1) {
	return byte_size_1;
    } else if (gcf == erts_gc_map_size_1) {
        return map_size_1;
    } else if (gcf == erts_gc_abs_1) {
	return abs_1;
    } else if (gcf == erts_gc_float_1) {
	return float_1;
    } else if (gcf == erts_gc_round_1) {
	return round_1;
    } else if (gcf == erts_gc_trunc_1) {
	return round_1;
    } else if (gcf == erts_gc_binary_part_2) {
        return binary_part_2;
    } else if (gcf == erts_gc_binary_part_3) {
        return binary_part_3;
    } else {
	erl_exit(1, "bad gc bif");
    }
}

/*
 * Mapping from the error code 'class tag' to atoms.
 */
Eterm exception_tag[NUMBER_EXC_TAGS] = {
  am_error,	/* 0 */
  am_exit,	/* 1 */
  am_throw,	/* 2 */
};

/*
 * Mapping from error code 'index' to atoms.
 */
Eterm error_atom[NUMBER_EXIT_CODES] = {
  am_internal_error,	/* 0 */
  am_normal,		/* 1 */
  am_internal_error,	/* 2 */
  am_badarg,		/* 3 */
  am_badarith,		/* 4 */
  am_badmatch,		/* 5 */
  am_function_clause,	/* 6 */
  am_case_clause,	/* 7 */
  am_if_clause,		/* 8 */
  am_undef,		/* 9 */
  am_badfun,		/* 10 */
  am_badarity,		/* 11 */
  am_timeout_value,	/* 12 */
  am_noproc,		/* 13 */
  am_notalive,		/* 14 */
  am_system_limit,	/* 15 */
  am_try_clause,	/* 16 */
  am_notsup		/* 17 */
};

/*
 * To fully understand the error handling, one must keep in mind that
 * when an exception is thrown, the search for a handler can jump back
 * and forth between Beam and native code. Upon each mode switch, a
 * dummy handler is inserted so that if an exception reaches that point,
 * the handler is invoked (like any handler) and transfers control so
 * that the search for a real handler is continued in the other mode.
 * Therefore, c_p->freason and c_p->fvalue must still hold the exception
 * info when the handler is executed, but normalized so that creation of
 * error terms and saving of the stack trace is only done once, even if
 * we pass through the error handling code several times.
 *
 * When a new exception is raised, the current stack trace information
 * is quick-saved in a small structure allocated on the heap. Depending
 * on how the exception is eventually caught (perhaps by causing the
 * current process to terminate), the saved information may be used to
 * create a symbolic (human-readable) representation of the stack trace
 * at the point of the original exception.
 */

static BeamInstr*
handle_error(Process* c_p, BeamInstr* pc, Eterm* reg, BifFunction bf)
{
    Eterm* hp;
    Eterm Value = c_p->fvalue;
    Eterm Args = am_true;
    c_p->i = pc;    /* In case we call erl_exit(). */
    erts_printf("in handle_error\n");

    ASSERT(c_p->freason != TRAP); /* Should have been handled earlier. */

    /*
     * Check if we have an arglist for the top level call. If so, this
     * is encoded in Value, so we have to dig out the real Value as well
     * as the Arglist.
     */
    if (c_p->freason & EXF_ARGLIST) {
	  Eterm* tp;
	  ASSERT(is_tuple(Value));
	  tp = tuple_val(Value);
	  Value = tp[1];
	  Args = tp[2];
    }

    /*
     * Save the stack trace info if the EXF_SAVETRACE flag is set. The
     * main reason for doing this separately is to allow throws to later
     * become promoted to errors without losing the original stack
     * trace, even if they have passed through one or more catch and
     * rethrow. It also makes the creation of symbolic stack traces much
     * more modular.
     */
    if (c_p->freason & EXF_SAVETRACE) {
        save_stacktrace(c_p, pc, reg, bf, Args);
    }

    /*
     * Throws that are not caught are turned into 'nocatch' errors
     */
    if ((c_p->freason & EXF_THROWN) && (c_p->catches <= 0) ) {
	hp = HAlloc(c_p, 3);
        Value = TUPLE2(hp, am_nocatch, Value);
        c_p->freason = EXC_ERROR;
    }

    /* Get the fully expanded error term */
    Value = expand_error_value(c_p, c_p->freason, Value);

    /* Save final error term and stabilize the exception flags so no
       further expansion is done. */
    c_p->fvalue = Value;
    c_p->freason = PRIMARY_EXCEPTION(c_p->freason);

    /* Find a handler or die */
    if ((c_p->catches > 0 || IS_TRACED_FL(c_p, F_EXCEPTION_TRACE))
	&& !(c_p->freason & EXF_PANIC)) {
	BeamInstr *new_pc;
        /* The Beam handler code (catch_end or try_end) checks reg[0]
	   for THE_NON_VALUE to see if the previous code finished
	   abnormally. If so, reg[1], reg[2] and reg[3] should hold the
	   exception class, term and trace, respectively. (If the
	   handler is just a trap to native code, these registers will
	   be ignored.) */
	reg[0] = THE_NON_VALUE;
	reg[1] = exception_tag[GET_EXC_CLASS(c_p->freason)];
	reg[2] = Value;
	reg[3] = c_p->ftrace;
        if ((new_pc = next_catch(c_p, reg))) {
	    c_p->cp = 0;	/* To avoid keeping stale references. */
	    return new_pc;
	}
	if (c_p->catches > 0) erl_exit(1, "Catch not found");
    }
    ERTS_SMP_UNREQ_PROC_MAIN_LOCK(c_p);
    terminate_proc(c_p, Value);
    ERTS_SMP_REQ_PROC_MAIN_LOCK(c_p);
    return NULL;
}

/*
 * Find the nearest catch handler
 */
static BeamInstr*
next_catch(Process* c_p, Eterm *reg) {
    // ESTUB: Tracing omitted
    int active_catches = c_p->catches > 0;
    Eterm *ptr, *prev = NULL;
    ptr = prev = c_p->stop;

    ASSERT(is_CP(*ptr));
    ASSERT(ptr <= STACK_START(c_p));
    if (ptr == STACK_START(c_p)) return NULL;
    while (ptr < STACK_START(c_p)) {
	if (is_catch(*ptr)) {
	    if (active_catches) goto found_catch;
	    ptr++;
	}
	else ptr++;
    }
    return NULL;
    
 found_catch:
    ASSERT(ptr < STACK_START(c_p));
    c_p->stop = prev;
    return catch_pc(*ptr);
}

/*
 * Terminating the process when an exception is not caught
 */
static void
terminate_proc(Process* c_p, Eterm Value)
{
    /* Add a stacktrace if this is an error. */
    if (GET_EXC_CLASS(c_p->freason) == EXTAG_ERROR) {
        Value = add_stacktrace(c_p, Value, c_p->ftrace);
    }
    /* EXF_LOG is a primary exception flag */
    if (c_p->freason & EXF_LOG) {
	erts_dsprintf_buf_t *dsbufp = erts_create_logger_dsbuf();
	erts_dsprintf(dsbufp, "Error in process %T ", c_p->common.id);
	/* if (erts_is_alive) */
	/*     erts_dsprintf(dsbufp, "on node %T ", erts_this_node->sysname); */
	erts_dsprintf(dsbufp, "on slave %d ", erts_get_scheduler_data()->no - 1);
	erts_dsprintf(dsbufp,"with exit value: %0.*T\n", display_items, Value);
	erts_send_error_to_logger(c_p->group_leader, dsbufp);
    }
    /*
     * If we use a shared heap, the process will be garbage-collected.
     * Must zero c_p->arity to indicate that there are no live registers.
     */
    c_p->arity = 0;
    erts_do_exit_process(c_p, Value);
}

/*
 * Build and add a symbolic stack trace to the error value.
 */
static Eterm
add_stacktrace(Process* c_p, Eterm Value, Eterm exc) {
    Eterm Where = build_stacktrace(c_p, exc);
    Eterm* hp = HAlloc(c_p, 3);
    return TUPLE2(hp, Value, Where);
}

/*
 * Forming the correct error value from the internal error code.
 * This does not update c_p->fvalue or c_p->freason.
 */
Eterm
expand_error_value(Process* c_p, Uint freason, Eterm Value) {
    Eterm* hp;
    Uint r;

    r = GET_EXC_INDEX(freason);
    ASSERT(r < NUMBER_EXIT_CODES); /* range check */
    ASSERT(is_value(Value));

    switch (r) {
    case (GET_EXC_INDEX(EXC_PRIMARY)):
        /* Primary exceptions use fvalue as it is */
	break;
    case (GET_EXC_INDEX(EXC_BADMATCH)):
    case (GET_EXC_INDEX(EXC_CASE_CLAUSE)):
    case (GET_EXC_INDEX(EXC_TRY_CLAUSE)):
    case (GET_EXC_INDEX(EXC_BADFUN)):
    case (GET_EXC_INDEX(EXC_BADARITY)):
        /* Some common exceptions: value -> {atom, value} */
        ASSERT(is_value(Value));
	hp = HAlloc(c_p, 3);
	Value = TUPLE2(hp, error_atom[r], Value);
	break;
    default:
        /* Other exceptions just use an atom as descriptor */
        Value = error_atom[r];
	break;
    }
#ifdef DEBUG
    ASSERT(Value != am_internal_error);
#endif
    return Value;
}

/*
 * Quick-saving the stack trace in an internal form on the heap. Note
 * that c_p->ftrace will point to a cons cell which holds the given args
 * and the saved data (encoded as a bignum).
 *
 * There is an issue with line number information. Line number
 * information is associated with the address *before* an operation
 * that may fail or be stored stored on the stack. But continuation
 * pointers point after its call instruction, not before. To avoid
 * finding the wrong line number, we'll need to adjust them so that
 * they point at the beginning of the call instruction or inside the
 * call instruction. Since its impractical to point at the beginning,
 * we'll do the simplest thing and decrement the continuation pointers
 * by one.
 *
 * Here is an example of what can go wrong. Without the adjustment
 * of continuation pointers, the call at line 42 below would seem to
 * be at line 43:
 *
 * line 42
 * call ...
 * line 43
 * gc_bif ...
 *
 * (It would be much better to put the arglist - when it exists - in the
 * error value instead of in the actual trace; e.g. '{badarg, Args}'
 * instead of using 'badarg' with Args in the trace. The arglist may
 * contain very large values, and right now they will be kept alive as
 * long as the stack trace is live. Preferably, the stack trace should
 * always be small, so that it does not matter if it is long-lived.
 * However, it is probably not possible to ever change the format of
 * error terms.)
 */

static void
save_stacktrace(Process* c_p, BeamInstr* pc, Eterm* reg, BifFunction bf,
		Eterm args) {
    struct StackTrace* s;
    int sz;
    int depth = erts_backtrace_depth;    /* max depth (never negative) */
    if (depth > 0) {
	/* There will always be a current function */
	depth --;
    }

    /* Create a container for the exception data */
    sz = (offsetof(struct StackTrace, trace) + sizeof(BeamInstr *)*depth
          + sizeof(Eterm) - 1) / sizeof(Eterm);
    s = (struct StackTrace *) HAlloc(c_p, 1 + sz);
    /* The following fields are inside the bignum */
    s->header = make_pos_bignum_header(sz);
    s->freason = c_p->freason;
    s->depth = 0;

    /*
     * If the failure was in a BIF other than 'error', 'exit' or
     * 'throw', find the bif-table index and save the argument
     * registers by consing up an arglist.
     */
    if (bf != NULL && bf != error_1 && bf != error_2 &&
	bf != exit_1 && bf != throw_1) {
        int i;
	int a = 0;
	for (i = 0; i < BIF_SIZE; i++) {
	    if (bf == bif_table[i].f || bf == bif_table[i].traced) {
		Export *ep = bif_export[i];
		s->current = ep->code;
	        a = bif_table[i].arity;
		break;
	    }
	}
	if (i >= BIF_SIZE) {
	    /* 
	     * The Bif does not really exist (no BIF entry).  It is a
	     * TRAP and traps are called through apply_bif, which also
	     * sets c_p->current (luckily).
	     * OR it is a NIF called by call_nif where current is also set.
	     */
	    ASSERT(c_p->current);
	    s->current = c_p->current;
	    a = s->current[2];
	}
	/* Save first stack entry */
	ASSERT(pc);
	if (depth > 0) {
	    s->trace[s->depth++] = pc;
	    depth--;
	}
	/* Save second stack entry if CP is valid and different from pc */
	if (depth > 0 && c_p->cp != 0 && c_p->cp != pc) {
	    s->trace[s->depth++] = c_p->cp - 1;
	    depth--;
	}
	s->pc = NULL;
	args = make_arglist(c_p, reg, a); /* Overwrite CAR(c_p->ftrace) */
    } else {
	s->current = c_p->current;
        /* 
	 * For a function_clause error, the arguments are in the beam
	 * registers, c_p->cp is valid, and c_p->current is set.
	 */
	if ( (GET_EXC_INDEX(s->freason)) ==
	     (GET_EXC_INDEX(EXC_FUNCTION_CLAUSE)) ) {
	    int a;
	    ASSERT(s->current);
	    a = s->current[2];
	    args = make_arglist(c_p, reg, a); /* Overwrite CAR(c_p->ftrace) */
	    /* Save first stack entry */
	    ASSERT(c_p->cp);
	    if (depth > 0) {
		s->trace[s->depth++] = c_p->cp - 1;
		depth--;
	    }
	    s->pc = NULL; /* Ignore pc */
	} else {
	    if (depth > 0 && c_p->cp != 0 && c_p->cp != pc) {
		s->trace[s->depth++] = c_p->cp - 1;
		depth--;
	    }
	    s->pc = pc;
	}
    }

    /* Package args and stack trace */
    {
	Eterm *hp;
	hp = HAlloc(c_p, 2);
	c_p->ftrace = CONS(hp, args, make_big((Eterm *) s));
    }

    /* Save the actual stack trace */
    erts_save_stacktrace(c_p, s, depth);
}

void
erts_save_stacktrace(Process* p, struct StackTrace* s, int depth)
{
    if (depth > 0) {
	Eterm *ptr;
	BeamInstr *prev = s->depth ? s->trace[s->depth-1] : NULL;
	BeamInstr i_return_trace = beam_return_trace[0];
	BeamInstr i_return_to_trace = beam_return_to_trace[0];

	/*
	 * Traverse the stack backwards and add all unique continuation
	 * pointers to the buffer, up to the maximum stack trace size.
	 * 
	 * Skip trace stack frames.
	 */
	ptr = p->stop;
	if (ptr < STACK_START(p) &&
	    (is_not_CP(*ptr)|| (*cp_val(*ptr) != i_return_trace &&
				*cp_val(*ptr) != i_return_to_trace)) &&
	    p->cp) {
	    /* Cannot follow cp here - code may be unloaded */
	    BeamInstr *cpp = p->cp;
	    if (cpp == beam_exception_trace || cpp == beam_return_trace) {
		/* Skip return_trace parameters */
		ptr += 2;
	    } else if (cpp == beam_return_to_trace) {
		/* Skip return_to_trace parameters */
		ptr += 1;
	    }
	}
	while (ptr < STACK_START(p) && depth > 0) {
	    if (is_CP(*ptr)) {
		if (*cp_val(*ptr) == i_return_trace) {
		    /* Skip stack frame variables */
		    do ++ptr; while (is_not_CP(*ptr));
		    /* Skip return_trace parameters */
		    ptr += 2;
		} else if (*cp_val(*ptr) == i_return_to_trace) {
		    /* Skip stack frame variables */
		    do ++ptr; while (is_not_CP(*ptr));
		} else {
		    BeamInstr *cp = cp_val(*ptr);
		    if (cp != prev) {
			/* Record non-duplicates only */
			prev = cp;
			s->trace[s->depth++] = cp - 1;
			depth--;
		    }
		    ptr++;
		}
	    } else ptr++;
	}
    }
}

/*
 * Getting the relevant fields from the term pointed to by ftrace
 */

static struct StackTrace *get_trace_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NULL;
    } else {
	ASSERT(is_list(exc));
	return (struct StackTrace *) big_val(CDR(list_val(exc)));
    }
}

static Eterm get_args_from_exc(Eterm exc) {
    if (exc == NIL) {
	return NIL;
    } else {
	ASSERT(is_list(exc));
	return CAR(list_val(exc));
    }
}

static int is_raised_exc(Eterm exc) {
    if (exc == NIL) {
        return 0;
    } else {
        ASSERT(is_list(exc));
        return bignum_header_is_neg(*big_val(CDR(list_val(exc))));
    }
}

/*
 * Creating a list with the argument registers
 */
static Eterm
make_arglist(Process* c_p, Eterm* reg, int a) {
    Eterm args = NIL;
    Eterm* hp = HAlloc(c_p, 2*a);
    while (a > 0) {
        args = CONS(hp, reg[a-1], args);
	hp += 2;
	a--;
    }
    return args;
}

/*
 * Building a symbolic representation of a saved stack trace. Note that
 * the exception object 'exc', unless NIL, points to a cons cell which
 * holds the given args and the quick-saved data (encoded as a bignum).
 *
 * If the bignum is negative, the given args is a complete stacktrace.
 */
Eterm
build_stacktrace(Process* c_p, Eterm exc) {
    struct StackTrace* s;
    Eterm  args;
    int    depth;
    FunctionInfo fi;
    FunctionInfo* stk;
    FunctionInfo* stkp;
    Eterm res = NIL;
    Uint heap_size;
    Eterm* hp;
    Eterm mfa;
    int i;

    if (! (s = get_trace_from_exc(exc))) {
        return NIL;
    }
#ifdef HIPE
    if (s->freason & EXF_NATIVE) {
	return hipe_build_stacktrace(c_p, s);
    }
#endif
    if (is_raised_exc(exc)) {
	return get_args_from_exc(exc);
    }

    /*
     * Find the current function. If the saved s->pc is null, then the
     * saved s->current should already contain the proper value.
     */
    if (s->pc != NULL) {
	erts_lookup_function_info(&fi, s->pc, 1);
    } else if (GET_EXC_INDEX(s->freason) ==
	       GET_EXC_INDEX(EXC_FUNCTION_CLAUSE)) {
	erts_lookup_function_info(&fi, s->current, 1);
    } else {
	erts_set_current_function(&fi, s->current);
    }

    /*
     * If fi.current is still NULL, default to the initial function
     * (e.g. spawn_link(erlang, abs, [1])).
     */
    if (fi.current == NULL) {
	erts_set_current_function(&fi, c_p->initial);
	args = am_true; /* Just in case */
    } else {
	args = get_args_from_exc(exc);
    }

    /*
     * Look up all saved continuation pointers and calculate
     * needed heap space.
     */
    depth = s->depth;
    stk = stkp = (FunctionInfo *) erts_alloc(ERTS_ALC_T_TMP,
				      depth*sizeof(FunctionInfo));
    heap_size = fi.needed + 2;
    for (i = 0; i < depth; i++) {
	erts_lookup_function_info(stkp, s->trace[i], 1);
	if (stkp->current) {
	    heap_size += stkp->needed + 2;
	    stkp++;
	}
    }

    /*
     * Allocate heap space and build the stacktrace.
     */
    hp = HAlloc(c_p, heap_size);
    while (stkp > stk) {
	stkp--;
	hp = erts_build_mfa_item(stkp, hp, am_true, &mfa);
	res = CONS(hp, mfa, res);
	hp += 2;
    }
    hp = erts_build_mfa_item(&fi, hp, args, &mfa);
    res = CONS(hp, mfa, res);

    erts_free(ERTS_ALC_T_TMP, (void *) stk);
    return res;
}

static BeamInstr*
call_error_handler(Process* p, BeamInstr* fi, Eterm* reg, Eterm func)
{
    EPIPHANY_STUB(call_error_handler);
}

static Export*
apply_setup_error_handler(Process* p, Eterm module, Eterm function, Uint arity, Eterm* reg)
{
    Export* ep;

    /*
     * Find the export table index for the error handler. Return NULL if
     * there is no error handler module.
     */

    if ((ep = erts_active_export_entry(erts_proc_get_error_handler(p),
				     am_undefined_function, 3)) == NULL) {
	return NULL;
    } else {
	int i;
	Uint sz = 2*arity;
	Eterm* hp;
	Eterm args = NIL;
	
	/*
	 * Always copy args from registers to a new list; this ensures
	 * that we have the same behaviour whether or not this was
	 * called from apply or fixed_apply (any additional last
	 * THIS-argument will be included, assuming that arity has been
	 * properly adjusted).
	 */

	if (HeapWordsLeft(p) < sz) {
	    erts_garbage_collect(p, sz, reg, arity);
	}
	hp = HEAP_TOP(p);
	HEAP_TOP(p) += sz;
	for (i = arity-1; i >= 0; i--) {
	    args = CONS(hp, reg[i], args);
	    hp += 2;
	}
	reg[0] = module;
	reg[1] = function;
	reg[2] = args;
    }

    return ep;
}

static BeamInstr*
apply(Process* p, Eterm module, Eterm function, Eterm args, Eterm* reg)
{
    int arity;
    Export* ep;
    Eterm tmp, this;

    /*
     * Check the arguments which should be of the form apply(Module,
     * Function, Arguments) where Function is an atom and
     * Arguments is an arity long list of terms.
     */
    if (is_not_atom(function)) {
	/*
	 * No need to test args here -- done below.
	 */
    error:
	p->freason = BADARG;

    error2:
	reg[0] = module;
	reg[1] = function;
	reg[2] = args;
	return 0;
    }

    /* The module argument may be either an atom or an abstract module
     * (currently implemented using tuples, but this might change).
     */
    this = THE_NON_VALUE;
    if (is_not_atom(module)) {
	Eterm* tp;

        if (is_not_tuple(module)) goto error;
        tp = tuple_val(module);
        if (arityval(tp[0]) < 1) goto error;
        this = module;
        module = tp[1];
        if (is_not_atom(module)) goto error;
    }
    
    /*
     * Walk down the 3rd parameter of apply (the argument list) and copy
     * the parameters to the x registers (reg[]). If the module argument
     * was an abstract module, add 1 to the function arity and put the
     * module argument in the n+1st x register as a THIS reference.
     */

    tmp = args;
    arity = 0;
    while (is_list(tmp)) {
	if (arity < (MAX_REG - 1)) {
	    reg[arity++] = CAR(list_val(tmp));
	    tmp = CDR(list_val(tmp));
	} else {
	    p->freason = SYSTEM_LIMIT;
	    goto error2;
	}
    }
    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	goto error;
    }
    if (this != THE_NON_VALUE) {
        reg[arity++] = this;
    }

    /*
     * Get the index into the export table, or failing that the export
     * entry for the error handler.
     *
     * Note: All BIFs have export entries; thus, no special case is needed.
     */

    if ((ep = erts_active_export_entry(module, function, arity)) == NULL) {
	if ((ep = apply_setup_error_handler(p, module, function, arity, reg)) == NULL) goto error;
    }
    // ESTUB: No tracing
    /* else if (ERTS_PROC_GET_SAVED_CALLS_BUF(p)) { */
    /*     save_calls(p, ep); */
    /* } */

#ifdef USE_VM_CALL_PROBES
    if (DTRACE_ENABLED(global_function_entry)) {
        BeamInstr *fptr = (BeamInstr *) ep->addressv[erts_active_code_ix()];
	DTRACE_GLOBAL_CALL(p, (Eterm)fptr[-3], (Eterm)fptr[-2], (Uint)fptr[-1]);
    }
#endif
    return ep->addressv[erts_active_code_ix()];
}

static BeamInstr*
fixed_apply(Process* p, Eterm* reg, Uint arity)
{
    Export* ep;
    Eterm module;
    Eterm function;

    module = reg[arity];    /* The THIS pointer already in place */
    function = reg[arity+1];

    if (is_not_atom(function)) {
    error:
	p->freason = BADARG;
	reg[0] = module;
	reg[1] = function;
	reg[2] = NIL;
	return 0;
    }

    /* The module argument may be either an atom or an abstract module
     * (currently implemented using tuples, but this might change).
     */
    if (is_not_atom(module)) {
	Eterm* tp;
        if (is_not_tuple(module)) goto error;
        tp = tuple_val(module);
        if (arityval(tp[0]) < 1) goto error;
        module = tp[1];
        if (is_not_atom(module)) goto error;
        ++arity;
    }
    
    /*
     * Get the index into the export table, or failing that the export
     * entry for the error handler module.
     *
     * Note: All BIFs have export entries; thus, no special case is needed.
     */

    if ((ep = erts_active_export_entry(module, function, arity)) == NULL) {
	if ((ep = apply_setup_error_handler(p, module, function, arity, reg)) == NULL)
	    goto error;
    }
    // ESTUB: No tracing
    /* else if (ERTS_PROC_GET_SAVED_CALLS_BUF(p)) { */
    /*     save_calls(p, ep); */
    /* } */

#ifdef USE_VM_CALL_PROBES
    if (DTRACE_ENABLED(global_function_entry)) {
        BeamInstr *fptr = (BeamInstr *)  ep->addressv[erts_active_code_ix()];
	DTRACE_GLOBAL_CALL(p, (Eterm)fptr[-3], (Eterm)fptr[-2], (Uint)fptr[-1]);
    }
#endif
    return ep->addressv[erts_active_code_ix()];
}

int
erts_hibernate(Process* c_p, Eterm module, Eterm function, Eterm args, Eterm* reg)
{
    EPIPHANY_STUB(erts_hibernate);
}

static BeamInstr*
call_fun(Process* p,		/* Current process. */
	 int arity,		/* Number of arguments for Fun. */
	 Eterm* reg,		/* Contents of registers. */
	 Eterm args)		/* THE_NON_VALUE or pre-built list of arguments. */
{
    Eterm fun = reg[arity];
    Eterm hdr;
    int i;
    Eterm* hp;

    if (!is_boxed(fun)) {
	goto badfun;
    }
    hdr = *boxed_val(fun);

    if (is_fun_header(hdr)) {
	ErlFunThing* funp = (ErlFunThing *) fun_val(fun);
	ErlFunEntry* fe;
	BeamInstr* code_ptr;
	Eterm* var_ptr;
	int actual_arity;
	unsigned num_free;

	fe = funp->fe;
	num_free = funp->num_free;
	code_ptr = fe->address;
	actual_arity = (int) code_ptr[-1];

	if (actual_arity == arity+num_free) {
	    DTRACE_LOCAL_CALL(p, (Eterm)code_ptr[-3],
			(Eterm)code_ptr[-2],
			code_ptr[-1]);
	    if (num_free == 0) {
		return code_ptr;
	    } else {
		var_ptr = funp->env;
		reg += arity;
		i = 0;
		do {
		    reg[i] = var_ptr[i];
		    i++;
		} while (i < num_free);
		reg[i] = fun;
		return code_ptr;
	    }
	    return code_ptr;
	} else {
	    /*
	     * Something wrong here. First build a list of the arguments.
	     */

	    if (is_non_value(args)) {
		Uint sz = 2 * arity;
		args = NIL;
		if (HeapWordsLeft(p) < sz) {
		    erts_garbage_collect(p, sz, reg, arity+1);
		    fun = reg[arity];
		}
		hp = HEAP_TOP(p);
		HEAP_TOP(p) += sz;
		for (i = arity-1; i >= 0; i--) {
		    args = CONS(hp, reg[i], args);
		    hp += 2;
		}
	    }

	    if (actual_arity >= 0) {
		/*
		 * There is a fun defined, but the call has the wrong arity.
		 */
		hp = HAlloc(p, 3);
		p->freason = EXC_BADARITY;
		p->fvalue = TUPLE2(hp, fun, args);
		return NULL;
	    } else {
                EPIPHANY_STUB(call_fun);
	    }
	}
    } else if (is_export_header(hdr)) {
	Export *ep;
	int actual_arity;

	ep = *((Export **) (export_val(fun) + 1));
	actual_arity = (int) ep->code[2];

	if (arity == actual_arity) {
	    DTRACE_GLOBAL_CALL(p, ep->code[0], ep->code[1], (Uint)ep->code[2]);
	    return ep->addressv[erts_active_code_ix()];
	} else {
	    /*
	     * Wrong arity. First build a list of the arguments.
	     */  

	    if (is_non_value(args)) {
		args = NIL;
		hp = HAlloc(p, arity*2);
		for (i = arity-1; i >= 0; i--) {
		    args = CONS(hp, reg[i], args);
		    hp += 2;
		}
	    }

	    hp = HAlloc(p, 3);
	    p->freason = EXC_BADARITY;
	    p->fvalue = TUPLE2(hp, fun, args);
	    return NULL;
	}
    } else {
    badfun:
	p->current = NULL;
	p->freason = EXC_BADFUN;
	p->fvalue = fun;
	return NULL;
    }
}

static BeamInstr*
apply_fun(Process* p, Eterm fun, Eterm args, Eterm* reg)
{
    int arity;
    Eterm tmp;

    /*
     * Walk down the 3rd parameter of apply (the argument list) and copy
     * the parameters to the x registers (reg[]).
     */

    tmp = args;
    arity = 0;
    while (is_list(tmp)) {
	if (arity < MAX_REG-1) {
	    reg[arity++] = CAR(list_val(tmp));
	    tmp = CDR(list_val(tmp));
	} else {
	    p->freason = SYSTEM_LIMIT;
	    return NULL;
	}
    }

    if (is_not_nil(tmp)) {	/* Must be well-formed list */
	p->freason = EXC_UNDEF;
	return NULL;
    }
    reg[arity] = fun;
    return call_fun(p, arity, reg, args);
}



static Eterm
new_fun(Process* p, Eterm* reg, ErlFunEntry* fe, int num_free)
{
    unsigned needed = ERL_FUN_SIZE + num_free;
    ErlFunThing* funp;
    Eterm* hp;
    int i;

    if (HEAP_LIMIT(p) - HEAP_TOP(p) <= needed) {
	PROCESS_MAIN_CHK_LOCKS(p);
	erts_garbage_collect(p, needed, reg, num_free);
	ERTS_VERIFY_UNUSED_TEMP_ALLOC(p);
	PROCESS_MAIN_CHK_LOCKS(p);
    }
    hp = p->htop;
    p->htop = hp + needed;
    funp = (ErlFunThing *) hp;
    hp = funp->env;
    erts_refc_inc(&fe->refc, 2);
    funp->thing_word = HEADER_FUN;
    funp->next = MSO(p).first;
    MSO(p).first = (struct erl_off_heap_header*) funp;
    funp->fe = fe;
    funp->num_free = num_free;
    funp->creator = p->common.id;
#ifdef HIPE
    funp->native_address = fe->native_address;
#endif
    funp->arity = (int)fe->address[-1] - num_free;
    for (i = 0; i < num_free; i++) {
	*hp++ = reg[i];
    }
    return make_fun(funp);
}

static int has_not_map_field(Eterm map, Eterm key)
{
    map_t* mp;
    Eterm* keys;
    Uint i;
    Uint n;

    mp   = (map_t *)map_val(map);
    keys = map_get_keys(mp);
    n    = map_get_size(mp);
    if (is_immed(key)) {
	for (i = 0; i < n; i++) {
	    if (keys[i] == key) {
		return 0;
	    }
	}
    } else {
	for (i = 0; i <  n; i++) {
	    if (EQ(keys[i], key)) {
		return 0;
	    }
	}
    }
    return 1;
}

static Eterm get_map_element(Eterm map, Eterm key)
{
    map_t *mp;
    Eterm* ks, *vs;
    Uint i;
    Uint n;

    mp = (map_t *)map_val(map);
    ks = map_get_keys(mp);
    vs = map_get_values(mp);
    n  = map_get_size(mp);
    if (is_immed(key)) {
	for (i = 0; i < n; i++) {
	    if (ks[i] == key) {
		return vs[i];
	    }
	}
    } else {
	for (i = 0; i < n; i++) {
	    if (EQ(ks[i], key)) {
		return vs[i];
	    }
	}
    }
    return THE_NON_VALUE;
}

#define GET_TERM(term, dest)					\
do {								\
    Eterm src = (Eterm)(term);					\
    switch (src & _TAG_IMMED1_MASK) {				\
    case (R_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	dest = x(0);						\
	break;							\
    case (X_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	dest = x(src >> _TAG_IMMED1_SIZE);			\
	break;							\
    case (Y_REG_DEF << _TAG_PRIMARY_SIZE) | TAG_PRIMARY_HEADER:	\
	dest = y(src >> _TAG_IMMED1_SIZE);			\
	break;							\
    default:							\
	dest = src;						\
	break;							\
    }								\
} while(0)


static Eterm
new_map(Process* p, Eterm* reg, BeamInstr* I)
{
    Uint n = Arg(3);
    Uint i;
    Uint need = n + 1 /* hdr */ + 1 /*size*/ + 1 /* ptr */ + 1 /* arity */;
    Eterm keys;
    Eterm *mhp,*thp;
    Eterm *E;
    BeamInstr *ptr;
    map_t *mp;

    if (HeapWordsLeft(p) < need) {
	erts_garbage_collect(p, need, reg, Arg(2));
    }

    thp    = p->htop;
    mhp    = thp + 1 + n/2;
    E      = p->stop;
    ptr    = &Arg(4);
    keys   = make_tuple(thp);
    *thp++ = make_arityval(n/2);

    mp = (map_t *)mhp; mhp += MAP_HEADER_SIZE;
    mp->thing_word = MAP_HEADER;
    mp->size = n/2;
    mp->keys = keys;

    for (i = 0; i < n/2; i++) {
	GET_TERM(*ptr++, *thp++);
	GET_TERM(*ptr++, *mhp++);
    }
    p->htop = mhp;
    return make_map(mp);
}

static Eterm
update_map_assoc(Process* p, Eterm* reg, Eterm map, BeamInstr* I)
{
    Uint n;
    Uint num_old;
    Uint num_updates;
    Uint need;
    map_t *old_mp, *mp;
    Eterm res;
    Eterm* hp;
    Eterm* E;
    Eterm* old_keys;
    Eterm* old_vals;
    BeamInstr* new_p;
    Eterm new_key;
    Eterm* kp;

    if (is_not_map(map)) {
	return THE_NON_VALUE;
    }

    old_mp = (map_t *) map_val(map);
    num_old = map_get_size(old_mp);

    /*
     * If the old map is empty, create a new map.
     */

    if (num_old == 0) {
	return new_map(p, reg, I+1);
    }

    /*
     * Allocate heap space for the worst case (i.e. all keys in the
     * update list are new).
     */

    num_updates = Arg(4) / 2;
    need = 2*(num_old+num_updates) + 1 + MAP_HEADER_SIZE;
    if (HeapWordsLeft(p) < need) {
	Uint live = Arg(3);
	reg[live] = map;
	erts_garbage_collect(p, need, reg, live+1);
	map      = reg[live];
	old_mp   = (map_t *)map_val(map);
    }

    /*
     * Build the skeleton for the map, ready to be filled in.
     *
     * +-----------------------------------+
     * | (Space for aritvyal for keys)     | <-----------+
     * +-----------------------------------+		 |
     * | (Space for key 1)		   |		 |    <-- kp
     * +-----------------------------------+		 |
     *        .				    		 |
     *        .				    		 |
     *        .				    		 |
     * +-----------------------------------+		 |
     * | (Space for last key)		   |		 |
     * +-----------------------------------+		 |
     * | MAP_HEADER			   |		 |
     * +-----------------------------------+		 |
     * | (Space for number of keys/values) |		 |
     * +-----------------------------------+		 |
     * | Boxed tuple pointer            >----------------+
     * +-----------------------------------+
     * | (Space for value 1)		   |                  <-- hp
     * +-----------------------------------+
     */

    E = p->stop;
    kp = p->htop + 1;		/* Point to first key */
    hp = kp + num_old + num_updates;

    res = make_map(hp);
    mp = (map_t *)hp;
    hp += MAP_HEADER_SIZE;
    mp->thing_word = MAP_HEADER;
    mp->keys = make_tuple(kp-1);

    old_vals = map_get_values(old_mp);
    old_keys = map_get_keys(old_mp);

    new_p = &Arg(5);
    GET_TERM(*new_p, new_key);
    n = num_updates;

    /*
     * Fill in keys and values, until we run out of either updates
     * or old values and keys.
     */

    for (;;) {
	Eterm key;
	Sint c;

	ASSERT(kp < (Eterm *)mp);
	key = *old_keys;
	if ((c = CMP_TERM(key, new_key)) < 0) {
	    /* Copy old key and value */
	    *kp++ = key;
	    *hp++ = *old_vals;
	    old_keys++, old_vals++, num_old--;
	} else {		/* Replace or insert new */
	    GET_TERM(new_p[1], *hp++);
	    if (c > 0) {	/* If new new key */
		*kp++ = new_key;
	    } else {		/* If replacement */
		*kp++ = key;
		old_keys++, old_vals++, num_old--;
	    }
	    n--;
	    if (n == 0) {
		break;
	    } else {
		new_p += 2;
		GET_TERM(*new_p, new_key);
	    }
	}
	if (num_old == 0) {
	    break;
	}
    }

    /*
     * At this point, we have run out of either old keys and values,
     * or the update list. In other words, at least of one n and
     * num_old must be zero.
     */

    if (n > 0) {
	/*
	 * All old keys and values have been copied, but there
	 * are still new keys and values in the update list that
	 * must be copied.
	 */
	ASSERT(num_old == 0);
	while (n-- > 0) {
	    GET_TERM(new_p[0], *kp++);
	    GET_TERM(new_p[1], *hp++);
	    new_p += 2;
	}
    } else {
	/*
	 * All updates are now done. We may still have old
	 * keys and values that we must copy.
	 */
	ASSERT(n == 0);
	while (num_old-- > 0) {
	    ASSERT(kp < (Eterm *)mp);
	    *kp++ = *old_keys++;
	    *hp++ = *old_vals++;
	}
    }

    /*
     * Calculate how many values that are unused at the end of the
     * key tuple and fill it out with a bignum header.
     */
    if ((n = (Eterm *)mp - kp) > 0) {
	*kp = make_pos_bignum_header(n-1);
    }

    /*
     * Fill in the size of the map in both the key tuple and in the map.
     */

    n = kp - p->htop - 1;	/* Actual number of keys/values */
    *p->htop = make_arityval(n);
    mp->size = n;
    p->htop = hp;
    return res;
}

/*
 * Update values for keys that already exist in the map.
 */

static Eterm
update_map_exact(Process* p, Eterm* reg, Eterm map, BeamInstr* I)
{
    Uint n;
    Uint i;
    Uint num_old;
    Uint need;
    map_t *old_mp, *mp;
    Eterm res;
    Eterm* hp;
    Eterm* E;
    Eterm* old_keys;
    Eterm* old_vals;
    BeamInstr* new_p;
    Eterm new_key;

    if (is_not_map(map)) {
	return THE_NON_VALUE;
    }

    old_mp = (map_t *) map_val(map);
    num_old = map_get_size(old_mp);

    /*
     * If the old map is empty, create a new map.
     */

    if (num_old == 0) {
	return THE_NON_VALUE;
    }

    /*
     * Allocate the exact heap space needed.
     */

    need = num_old + MAP_HEADER_SIZE;
    if (HeapWordsLeft(p) < need) {
	Uint live = Arg(3);
	reg[live] = map;
	erts_garbage_collect(p, need, reg, live+1);
	map      = reg[live];
	old_mp   = (map_t *)map_val(map);
    }

    /*
     * Update map, keeping the old key tuple.
     */

    hp = p->htop;
    E = p->stop;

    old_vals = map_get_values(old_mp);
    old_keys = map_get_keys(old_mp);

    res = make_map(hp);
    mp = (map_t *)hp;
    hp += MAP_HEADER_SIZE;
    mp->thing_word = MAP_HEADER;
    mp->size = num_old;
    mp->keys = old_mp->keys;

    /* Get array of key/value pairs to be updated */
    new_p = &Arg(5);
    GET_TERM(*new_p, new_key);

    /* Update all values */
    n = Arg(4) / 2;		/* Number of values to be updated */
    ASSERT(n > 0);
    for (i = 0; i < num_old; i++) {
	if (!EQ(*old_keys, new_key)) {
	    /* Not same keys */
	    *hp++ = *old_vals;
	} else {
	    GET_TERM(new_p[1], *hp);
	    hp++;
	    n--;
	    if (n == 0) {
		/*
		 * All updates done. Copy remaining values
		 * and return the result.
		 */
		for (i++, old_vals++; i < num_old; i++) {
		    *hp++ = *old_vals++;
		}
		ASSERT(hp == p->htop + need);
		p->htop = hp;
		return res;
	    } else {
		new_p += 2;
		GET_TERM(*new_p, new_key);
	    }
	}
	old_vals++, old_keys++;
    }

    /*
     * Updates left. That means that at least one the keys in the
     * update list did not previously exist.
     */
    ASSERT(hp == p->htop + need);
    return THE_NON_VALUE;
}
#undef GET_TERM

int catchlevel(Process *p)
{
    return p->catches;
}

/*
 * Check if the given function is built-in (i.e. a BIF implemented in C).
 *
 * Returns 0 if not built-in, and a non-zero value if built-in.
 */

int
erts_is_builtin(Eterm Mod, Eterm Name, int arity)
{
    EPIPHANY_STUB(erts_is_builtin);
}


/*
 * Return the current number of reductions for the given process.
 * To get the total number of reductions, p->reds must be added.
 */

Uint
erts_current_reductions(Process *current, Process *p)
{
    if (current != p) {
	return 0;
    } else if (current->fcalls < 0 && ERTS_PROC_GET_SAVED_CALLS_BUF(current)) {
	return -current->fcalls;
    } else {
	return REDS_IN(current) - current->fcalls;
    }
}

int
erts_beam_jump_table(void)
{
#if defined(NO_JUMP_TABLE)
    return 0;
#else
    return 1;
#endif
}
