%%% -*- erlang-indent-level: 2 -*-
%%%
%%% %CopyrightBegin%
%%%
%%% Copyright Ericsson AB 2015. All Rights Reserved.
%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved online at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% %CopyrightEnd%
%%%
%%% The Epiphany instruction set has a few quirks that must be handled by the
%%% translation:
%%%
%%% - Instructions for the second pipeline (called "FPU/IALU2" in the
%%%   architecture reference) have the same encoding (IADD a,b,c =:= FADD a,b,c
%%%   etc.). Rather, they are distinguished by setting a couple of bits in a
%%%   special register "CONFIG", called "ARITHMODE" or just mode. In C, these
%%%   bits (yes, not the entire register) are callee-save. Since we expect
%%%   integer multiplications to outnumber single-precision floating point
%%%   operations, we tweak this convention by requiring that the mode is
%%%   "integer" at both function entry and exit.
%%%
%%% Additionally, the architecture relies heavily on compiler smarts for optimal
%%% performance. Some of the things a compiler should consider are:
%%%
%%% - The architecture is pipelined and in-order. Although it is fully
%%%   interlocked, a compiler unaware of all hazards will generate code that has
%%%   to stall.
%%% - The architecture is capable of dual issue under specific
%%%   conditions. Namely, it can execute an "FPU/IALU2" instruction in parallel
%%%   with a load/store or "IALU" instruction. This is complicated furthermore
%%%   by the fact that integer addition and subtraction can be executed on
%%%   either of these pipelines, but the choice is made at compile-time. Using
%%%   the second pipeline is also subject to the "ARITHMODE" complexity
%%%   mentioned above. If that isn't bad enough, the second pipeline sets
%%%   different condition codes that are less expressive than those set by the
%%%   first pipeline.

-module(hipe_rtl_to_epiphany).
-export([translate/1]).

-include("../rtl/hipe_rtl.hrl").

-define(FITS_SIMM11(Val), ((-16#400 =< (Val)) and ((Val) < 16#400))).
-define(FITS_UIMM5(Val), ((0 =< (Val)) and ((Val) < 16#20))).
-define(FITS_UIMM11(Val), ((0 =< (Val)) and ((Val) < 16#800))).

translate(RTL) ->
  hipe_gensym:init(epiphany),
  hipe_gensym:set_var(epiphany, hipe_epiphany_registers:first_virtual()),
  hipe_gensym:set_label(epiphany, hipe_gensym:get_label(rtl)),
  Map0 = vmap_empty(),
  {Formals, Map1} = conv_formals(hipe_rtl:rtl_params(RTL), Map0),
  OldData = hipe_rtl:rtl_data(RTL),
  {Code0, NewData} = conv_insn_list(hipe_rtl:rtl_code(RTL), Map1, OldData),
  {RegFormals, _} = split_args(Formals),
  Code =
    case RegFormals of
      [] -> Code0;
      _ -> [hipe_epiphany:mk_label(hipe_gensym:get_next_label(epiphany)) |
	    move_formals(RegFormals, Code0)]
    end,
  IsClosure = hipe_rtl:rtl_is_closure(RTL),
  IsLeaf = hipe_rtl:rtl_is_leaf(RTL),
  hipe_epiphany:mk_defun(hipe_rtl:rtl_fun(RTL),
			 Formals,
			 IsClosure,
			 IsLeaf,
			 Code,
			 NewData,
			 [],
			 []).
  %%error({?MODULE, nyi, {translate, [RTL]}}).

conv_insn_list([H|T], Map, Data) ->
  {NewH, NewMap, NewData1} = conv_insn(H, Map, Data),
  %% io:format("~w \n  ==>\n ~w\n- - - - - - - - -\n",[H,NewH]),
  {NewT, NewData2} = conv_insn_list(T, NewMap, NewData1),
  {NewH ++ NewT, NewData2};
conv_insn_list([], _, Data) ->
  {[], Data}.

-spec conv_insn(_, _, _) -> {[hipe_epiphany:instr()], _, _}.
conv_insn(I, Map, Data) ->
  case I of
    #alu{} -> conv_alu(I, Map, Data);
    #alub{} -> conv_alub(I, Map, Data);
    #branch{} -> conv_branch(I, Map, Data);
    #call{} -> conv_call(I, Map, Data);
    #comment{} -> conv_comment(I, Map, Data);
    #enter{} -> conv_enter(I, Map, Data);
    #goto{} -> conv_goto(I, Map, Data);
    #label{} -> conv_label(I, Map, Data);
    #load{} -> conv_load(I, Map, Data);
    #load_address{} -> conv_load_address(I, Map, Data);
    #load_atom{} -> conv_load_atom(I, Map, Data);
    #move{} -> conv_move(I, Map, Data);
    #return{} -> conv_return(I, Map, Data);
    #store{} -> conv_store(I, Map, Data);
    #switch{} -> conv_switch(I, Map, Data);
    _ -> exit({?MODULE,conv_insn,I})
  end.

conv_alu(I, Map, Data) ->
  %% dst = src1 aluop src2
  {Dst, Map0} = conv_dst(hipe_rtl:alu_dst(I), Map),
  {Src1, Map1} = conv_src(hipe_rtl:alu_src1(I), Map0),
  {Src2, Map2} = conv_src(hipe_rtl:alu_src2(I), Map1),
  RtlAluOp = hipe_rtl:alu_op(I),
  I2 = mk_alu(Dst, Src1, RtlAluOp, Src2),
  {I2, Map2, Data}.

mk_alu(Dst, Src1, RtlAluOp, Src2) ->
  case hipe_epiphany:is_temp(Src1) of
    true ->
      case hipe_epiphany:is_temp(Src2) of
	true ->
	  mk_alu_rr(Dst, Src1, RtlAluOp, Src2);
	_ ->
	  mk_alu_ri(Dst, Src1, RtlAluOp, Src2)
      end;
    _ ->
      case hipe_epiphany:is_temp(Src2) of
	true ->
	  mk_alu_ir(Dst, Src1, RtlAluOp, Src2);
	_ ->
	  mk_alu_ii(Dst, Src1, RtlAluOp, Src2)
      end
  end.

mk_alu_ii(Dst, Src1, RtlAluOp, Src2) ->
  io:format("~w: RTL alu with two immediates (~w ~w ~w)\n",
	    [?MODULE, Src1, RtlAluOp, Src2]),
  Tmp = new_untagged_temp(),
  mk_movi(Tmp, Src1,
	  mk_alu_ri(Dst, Tmp, RtlAluOp, Src2)).

mk_alu_ir(Dst, Src1, RtlAluOp, Src2) ->
  case rtl_aluop_commutes(RtlAluOp) of
    true ->
      mk_alu_ri(Dst, Src2, RtlAluOp, Src1);
    _ ->
      Tmp = new_untagged_temp(),
      mk_movi(Tmp, Src1,
	      mk_alu_rr(Dst, Tmp, RtlAluOp, Src2))
  end.

mk_alu_ri(Dst, Src1, RtlAluOp, Src2) ->
  AluOp = conv_aluop(RtlAluOp),
  case alu_allowed_imm(AluOp) of
    simm11 when ?FITS_SIMM11(Src2) ->
      [hipe_epiphany:mk_alu(AluOp, Dst, Src1, hipe_epiphany:mk_simm11(Src2))];
    uimm5 when ?FITS_UIMM5(Src2) ->
      [hipe_epiphany:mk_alu(AluOp, Dst, Src1, hipe_epiphany:mk_uimm5(Src2))];
    _ ->
      Tmp = new_untagged_temp(),
      mk_movi(Tmp, Src2,
	      mk_alu_rr(Dst, Src1, RtlAluOp, Tmp))
  end.

mk_alu_rr(Dst, Src1, RtlAluOp, Src2) ->
  AluOp = conv_aluop(RtlAluOp),
  [hipe_epiphany:mk_alu(AluOp, Dst, Src1, Src2)].

conv_aluop(RtlAluOp) ->
  Map = #{
    'add' => 'add',
    'sub' => 'sub',
    'or'  => 'orr',
    'and' => 'and',
    'xor' => 'eor',
    'mul' => 'imul',
    'sll' => 'lsl',
    'srl' => 'lsr',
    'sra' => 'asr'
   },
  maps:get(RtlAluOp, Map).

alu_allowed_imm(AluOp) ->
  Map = #{
    'add' => simm11,
    'sub' => simm11,
    'lsl' => uimm5,
    'lsr' => uimm5,
    'asr' => uimm5
   },
  maps:get(AluOp, Map, none).

conv_alub(I, Map, Data) ->
  %% dst = src1 aluop src2; if COND goto label
  {Dst, Map0} = conv_dst(hipe_rtl:alub_dst(I), Map),
  {Src1, Map1} = conv_src(hipe_rtl:alub_src1(I), Map0),
  {Src2, Map2} = conv_src(hipe_rtl:alub_src2(I), Map1),
  RtlAluOp = hipe_rtl:alub_op(I),
  RtlCond = hipe_rtl:alub_cond(I),
  Class = case RtlAluOp of
	    add -> au;
	    sub -> au;
	    mul -> fpu; %% sic!
	    _ -> lu
	  end,
  Overflow = RtlCond =:= overflow orelse RtlCond =:= not_overflow,
  {I1, Cond, I3} =
    case {Class, Overflow} of
      {fpu, false} -> {[], conv_fpu_cond(RtlCond), []};
      {au, false} when RtlAluOp =:= 'add', RtlCond =:= 'ltu' ->
	{[], 'gteu', []}; %% unsigned overflow = carry set = gteu
      {_, false} -> {[], conv_ialu_cond(RtlCond), []};
      {au, true} ->
	%% Since there is no condition code that tests just the overflow flag,
	%% we mask it from the STATUS special register instead.
	TmpStatus = new_untagged_temp(),
	TmpMask = new_untagged_temp(),
	OverflowFlagMask = 1 bsl 7,
	MaskInstr = hipe_epiphany:mk_alu('and', TmpStatus, TmpStatus, TmpMask),
	ExtractOverflowFlag =
	  hipe_epiphany:mk_movi(TmpMask, OverflowFlagMask,
				[hipe_epiphany:mk_movfs(TmpStatus, status),
				 MaskInstr]),
	{[], case RtlCond of
	       overflow -> ne; %% non-zero
	       not_overflow -> eq %% zero
	     end, ExtractOverflowFlag};
      {fpu, true} when mul =:= RtlAluOp->
	%% The hardware does not provide any way for us to check for overflow,
	%% we'll do it ourself
	%% XXX: We are overapproximating overflow here in order to save
	%% instructions. This *will* cause subtle compiler bugs if any use of
	%% alub(mul, [not_]overflow) is introduced which does not survive this!
	Src1High = new_untagged_temp(),
	Src2High = new_untagged_temp(),
	Throwaway = new_untagged_temp(),
	Shift1 = mk_alu(Src1High, Src1, 'sra', 14),
	Shift2 = mk_alu(Src2High, Src2, 'sra', 18),
	Comment = hipe_epiphany:mk_comment(
		    "XXX: Overapproximation of overflow"),
	Test = hipe_epiphany:mk_alu('orr', Throwaway, Src1High, Src2High),
	{Shift1 ++ Shift2, case RtlCond of
			     overflow -> ne; %% non-zero
			     not_overflow -> eq %% zero
			   end, [Comment, Test]}

    end,
  I2 = mk_alu(Dst, Src1, RtlAluOp, Src2),
  I4 = [hipe_epiphany:mk_pseudo_bcc(Cond,
				    hipe_rtl:alub_true_label(I),
				    hipe_rtl:alub_false_label(I),
				    hipe_rtl:alub_pred(I))],
  {I1 ++ I2 ++ I3 ++ I4, Map2, Data}.

conv_branch(I, Map, Data) ->
  %% <unused> = src1 - src2; if COND goto label
  {Src1, Map0} = conv_src(hipe_rtl:branch_src1(I), Map),
  {Src2, Map1} = conv_src(hipe_rtl:branch_src2(I), Map0),
  Cond = conv_branch_cond(hipe_rtl:branch_cond(I)),
  I2 = mk_branch(Src1, Cond, Src2,
		 hipe_rtl:branch_true_label(I),
		 hipe_rtl:branch_false_label(I),
		 hipe_rtl:branch_pred(I)),
  {I2, Map1, Data}.

mk_branch(Src1, Cond, Src2, TrueLab, FalseLab, Pred) ->
  case {hipe_epiphany:is_temp(Src1), hipe_epiphany:is_temp(Src2)} of
    {false, true} -> %% Commute
      mk_branch_2(Src2, commute_cond(Cond), Src1, TrueLab, FalseLab, Pred);
    _ ->  %% Leave as-is
      mk_branch_2(Src1, Cond,               Src2, TrueLab, FalseLab, Pred)
  end.

mk_branch_2(Src1, Cond, Src2, TrueLab, FalseLab, Pred) ->
  %% As an addional optimisation, conditions where Src2 is an immediate 0 could
  %% be implemented as "sub Src1,Src1,0", saving a scratch register. However, it
  %% might be a pessimisation instead, since it might introduce additional
  %% hazards and stalls.
  Throwaway = new_untagged_temp(),
  %% Since 'sub' isn't commutative, we don't have to worry about mk_alu
  %% commuting.
  mk_alu(Throwaway, Src1, 'sub', Src2)
    ++ [hipe_epiphany:mk_pseudo_bcc(Cond, TrueLab, FalseLab, Pred)].

conv_ialu_cond(Cond) ->	% only signed
  %% There is an overflow flag from the IALU pipeline (required for [lg]te?),
  %% but it is not available as a condition code.

  %% These condition codes have identical formulae to the x86 equivalents; thus
  %% we can be relatively calm that they will behave as expected for IALU
  %% operations.
  case Cond of
    eq	-> 'eq';
    ne	-> 'ne';
    gt	-> 'gt';
    ge	-> 'gte';
    lt	-> 'lt';
    le	-> 'lte'
  end.

conv_branch_cond(Cond) -> % may be unsigned
  %% The architecture reference mislabels the formulae for these condition
  %% codes. Since GCC trusts them to do what they say in the name, we will too,
  %% but only ever for 'sub'.
  case Cond of
    gtu -> 'gtu';
    geu -> 'gteu';
    ltu -> 'ltu';
    leu -> 'lteu';
    _   -> conv_ialu_cond(Cond)
  end.

conv_fpu_cond(Cond) ->
  %% Does any other cond make sense for multiplication?
  case Cond of
    eq -> 'beq';
    ne -> 'bne'
  end.

%%% Commute an Epiphany condition code.

commute_cond(Cond) ->	% (x Cond y) iff (y commute_cond(Cond) x)
  case Cond of
    'eq'   -> 'eq';	% ==, ==
    'ne'   -> 'ne';	% !=, !=
    'gt'   -> 'lt';	% >, <
    'lt'   -> 'gt';	% <, >
    'gte'  -> 'lte';	% >=, <=
    'lte'  -> 'gte';	% <=, >=
    'gtu'  -> 'ltu';	% >u, <u
    'ltu'  -> 'gtu';	% <u, >u
    'gteu' -> 'lteu';	% >=u, <=u
    'lteu' -> 'gteu'	% <=u, >=u
  end.

conv_call(I, Map, Data) ->
  {Args, Map0} = conv_src_list(hipe_rtl:call_arglist(I), Map),
  {Dsts, Map1} = conv_dst_list(hipe_rtl:call_dstlist(I), Map0),
  {Fun, Map2} = conv_fun(hipe_rtl:call_fun(I), Map1),
  ContLab = hipe_rtl:call_continuation(I),
  ExnLab = hipe_rtl:call_fail(I),
  Linkage = hipe_rtl:call_type(I),
  I2 = mk_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage),
  {I2, Map2, Data}.

mk_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage) ->
  case hipe_epiphany:is_prim(Fun) of
    true ->
      mk_primop_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage);
    false ->
      mk_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage)
  end.

mk_primop_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage) ->
  case hipe_epiphany:prim_prim(Prim) of
    %% no Epiphany-specific primops defined yet
    _ ->
      mk_general_call(Dsts, Prim, Args, ContLab, ExnLab, Linkage)
  end.

mk_general_call(Dsts, Fun, Args, ContLab, ExnLab, Linkage) ->
  %% The backend does not support pseudo_calls without a
  %% continuation label, so we make sure each call has one.
  {RealContLab, Tail} =
    case mk_call_results(0, Dsts, []) of
      [] ->
	%% Avoid consing up a dummy basic block if the moves list
	%% is empty, as is typical for calls to suspend/0.
	%% This should be subsumed by a general "optimise the CFG"
	%% module, and could probably be removed.
	case ContLab of
	  [] ->
	    NewContLab = hipe_gensym:get_next_label(epiphany),
	    {NewContLab, [hipe_epiphany:mk_label(NewContLab)]};
	  _ ->
	    {ContLab, []}
	end;
      Moves ->
	%% Change the call to continue at a new basic block.
	%% In this block move the result registers to the Dsts,
	%% then continue at the call's original continuation.
	NewContLab = hipe_gensym:get_next_label(epiphany),
	case ContLab of
	  [] ->
	    %% This is just a fallthrough
	    %% No jump back after the moves.
	    {NewContLab,
	     [hipe_epiphany:mk_label(NewContLab) |
	      Moves]};
	  _ ->
	    %% The call has a continuation. Jump to it.
	    {NewContLab,
	     [hipe_epiphany:mk_label(NewContLab) |
	      Moves ++
	      [hipe_epiphany:mk_bcc('always', ContLab)]]}
	end
    end,
  SDesc = hipe_epiphany:mk_sdesc(ExnLab, 0, length(Args), {}),
  CallInsn = hipe_epiphany:mk_pseudo_call(Fun, SDesc, RealContLab, Linkage),
  {RegArgs,StkArgs} = split_args(Args),
  mk_push_args(StkArgs, move_actuals(RegArgs, [CallInsn | Tail])).

mk_push_args([], Tail) -> Tail;
mk_push_args(StkArgs, Tail) ->
  %% We could push the arguments with postmodify-store instructions, but since
  %% they are not available in halfword form, this is more compact.
  NrStkArgs = length(StkArgs),
  [hipe_epiphany:mk_pseudo_call_prepare(NrStkArgs)
   | mk_store_args(StkArgs, NrStkArgs, Tail)].

mk_store_args([Arg|Args], PrevOffset, Tail) ->
  Offset = PrevOffset - 1,
  {Src,FixSrc} =
    case hipe_epiphany:is_temp(Arg) of
      true ->
	{Arg, []};
      _ ->
	Tmp = new_tagged_temp(),
	{Tmp, hipe_epiphany:mk_movi(Tmp, Arg)}
    end,
  OffsetArg = hipe_epiphany:mk_uimm11(Offset),
  Store = hipe_epiphany:mk_str('w', Src, mk_sp(), '+', OffsetArg),
  mk_store_args(Args, Offset, FixSrc ++ [Store | Tail]);
mk_store_args([], _, Tail) ->
  Tail.

mk_call_results(_, [], Tail) -> Tail;
mk_call_results(N, [Dst|Dsts], Tail) ->
  Reg = hipe_epiphany:mk_temp(hipe_epiphany_registers:ret(N), 'tagged'),
  mk_call_results(N+1, Dsts, [hipe_epiphany:mk_pseudo_move(Dst, Reg) | Tail]).

conv_comment(I, Map, Data) ->
  I2 = [hipe_epiphany:mk_comment(hipe_rtl:comment_text(I))],
  {I2, Map, Data}.

conv_enter(I, Map, Data) ->
  {Args, Map0} = conv_src_list(hipe_rtl:enter_arglist(I), Map),
  {Fun, Map1} = conv_fun(hipe_rtl:enter_fun(I), Map0),
  I2 = mk_enter(Fun, Args, hipe_rtl:enter_type(I)),
  {I2, Map1, Data}.

mk_enter(Fun, Args, Linkage) ->
  Arity = length(Args),
  {RegArgs,StkArgs} = split_args(Args),
  move_actuals(RegArgs,
	       [hipe_epiphany:mk_pseudo_tailcall_prepare(),
		hipe_epiphany:mk_pseudo_tailcall(Fun, Arity, StkArgs, Linkage)]).

conv_goto(I, Map, Data) ->
  I2 = [hipe_epiphany:mk_bcc('always', hipe_rtl:goto_label(I))],
  {I2, Map, Data}.

conv_label(I, Map, Data) ->
  I2 = [hipe_epiphany:mk_label(hipe_rtl:label_name(I))],
  {I2, Map, Data}.

conv_load(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_dst(I), Map),
  {Base1, Map1} = conv_src(hipe_rtl:load_src(I), Map0),
  {Base2, Map2} = conv_src(hipe_rtl:load_offset(I), Map1),
  LoadSize = hipe_rtl:load_size(I),
  LoadSign = hipe_rtl:load_sign(I),
  %% Sign-extend
  Extend =
    case {LoadSize,LoadSign} of
      {byte,signed} ->
	[hipe_epiphany:mk_alu('lsr', Dst, Dst, hipe_epiphany:mk_uimm5(24)),
	 hipe_epiphany:mk_alu('asr', Dst, Dst, hipe_epiphany:mk_uimm5(24))];
      _ -> []
    end,
  I2 = mk_mem(ldr, Dst, Base1, Base2, LoadSize, Extend),
  {I2, Map2, Data}.

mk_mem(Instr, Reg0, Base1, Base2, RtlSize, Tail) ->
  {Size, SizeBytes} =
    case RtlSize of
      byte  -> {'b', 1};
      int32 -> {'w', 4};
      word  -> {'w', 4}
    end,
  %% Ensure reg is a temp
  {I1, Reg} =
    case {hipe_epiphany:is_temp(Reg0), Instr} of
      {true, _} -> {[], Reg0};
      {false, str} ->
	RegTmp = new_untagged_temp(),
	MovReg = hipe_epiphany:mk_movi(RegTmp, Reg0),
	{MovReg, RegTmp}
    end,
  %% Ensure lhs is a temp
  {I2, Lhs, Rhs1} =
    case {hipe_epiphany:is_temp(Base1), hipe_epiphany:is_temp(Base2)} of
      {false, false} ->
	io:format("~w: RTL mem with two immediates\n", [?MODULE]),
	Tmp = new_untagged_temp(),
	Movi = hipe_epiphany:mk_movi(Tmp, (Base1 + Base2) band (1 bsl 32 - 1)),
	{Movi, Tmp, 0};
      {false, true} -> {[], Base2, Base1};
      _ -> {[], Base1, Base2}
    end,
  %% Ensure rhs is a temp or fits in an immediate
  {I3, Sign, Rhs} =
    case hipe_epiphany:is_temp(Rhs1) of
      true -> {[], '+', Rhs1};
      false ->
	%% Immediates are multiplied by the alignment size
	ImmVal = Rhs1 div SizeBytes,
	case Rhs1 rem SizeBytes of
	  0 when ?FITS_UIMM11(ImmVal) ->
	    {[], '+', hipe_epiphany:mk_uimm11(ImmVal)};
	  0 when ?FITS_UIMM11(-ImmVal) ->
	    {[], '-', hipe_epiphany:mk_uimm11(-ImmVal)};
	  _ ->
	    Tmp2 = new_untagged_temp(),
	    Movi2 = hipe_epiphany:mk_movi(Tmp2, Rhs1),
	    {Movi2, '+', Tmp2}
	end
    end,
  MkFun = case Instr of
	    ldr -> fun hipe_epiphany:mk_ldr/5;
	    str -> fun hipe_epiphany:mk_str/5
	  end,
  I1 ++ I2 ++ I3 ++ [MkFun(Size, Reg, Lhs, Sign, Rhs)] ++ Tail.

conv_store(I, Map, Data) ->
  {Base, Map0} = conv_dst(hipe_rtl:store_base(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:store_src(I), Map0),
  {Offset, Map2} = conv_src(hipe_rtl:store_offset(I), Map1),
  StoreSize = hipe_rtl:store_size(I),
  I2 = mk_mem(str, Src, Base, Offset, StoreSize, []),
  {I2, Map2, Data}.

conv_load_address(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_address_dst(I), Map),
  Addr = hipe_rtl:load_address_addr(I),
  Type = hipe_rtl:load_address_type(I),
  I2 = hipe_epiphany:mk_movi(Dst, {label, {Addr,Type}}),
  {I2, Map0, Data}.

conv_load_atom(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:load_atom_dst(I), Map),
  Src = hipe_rtl:load_atom_atom(I),
  I2 = hipe_epiphany:mk_movi(Dst, Src),
  {I2, Map0, Data}.

conv_move(I, Map, Data) ->
  {Dst, Map0} = conv_dst(hipe_rtl:move_dst(I), Map),
  {Src, Map1} = conv_src(hipe_rtl:move_src(I), Map0),
  I2 = mk_move(Dst, Src, []),
  {I2, Map1, Data}.

mk_move(Dst, Src, Tail) ->
  case hipe_epiphany:is_temp(Src) of
    true -> [hipe_epiphany:mk_pseudo_move(Dst, Src) | Tail];
    _ -> mk_movi(Dst, Src, Tail)
  end.

conv_return(I, Map, Data) ->
  {Args, Map0} = conv_src_list(hipe_rtl:return_varlist(I), Map),
  I2 = move_returns(0, Args, [hipe_epiphany:mk_rts(length(Args))]),
  {I2, Map0, Data}.

move_returns(_, [], Tail) -> Tail;
move_returns(N, [Ret|Rets], Tail) ->
  Reg = hipe_epiphany:mk_temp(hipe_epiphany_registers:ret(N), 'tagged'),
  move_returns(N-1, Rets, mk_move(Reg, Ret, Tail)).

conv_switch(I, Map, Data) ->
  Labels = hipe_rtl:switch_labels(I),
  LMap = [{label,L} || L <- Labels],
  {NewData, JTabLab} =
    case hipe_rtl:switch_sort_order(I) of
      [] ->
	hipe_consttab:insert_block(Data, word, LMap);
      SortOrder ->
	hipe_consttab:insert_sorted_block(
	  Data, word, LMap, SortOrder)
    end,
  %% no immediates allowed here
  {IndexR, Map1} = conv_dst(hipe_rtl:switch_src(I), Map),
  JTabR = new_untagged_temp(),
  I2 =
    hipe_epiphany:mk_movi(
      JTabR, {label, {JTabLab,constant}},
      [hipe_epiphany:mk_pseudo_switch(JTabR, IndexR, Labels)]),
  {I2, Map1, NewData}.

%%% Load an integer constant into a register.

mk_movi(Value, Dst, Tail) ->
  hipe_epiphany:mk_movi(Value, Dst, Tail).

%%% Check if an RTL ALU or ALUB operator commutes.

rtl_aluop_commutes(RtlAluOp) ->
  case RtlAluOp of
    'add' -> true;
    'mul' -> true;
    'or'  -> true;
    'and' -> true;
    'xor' -> true;
    _	  -> false
  end.

%%% Split a list of formal or actual parameters into the
%%% part passed in registers and the part passed on the stack.
%%% The parameters passed in registers are also tagged with
%%% the corresponding registers.

split_args(Args) ->
  split_args(0, hipe_epiphany_registers:nr_args(), Args, []).

split_args(I, N, [Arg|Args], RegArgs) when I < N ->
  Reg = hipe_epiphany_registers:arg(I),
  Temp = hipe_epiphany:mk_temp(Reg, 'tagged'),
  split_args(I+1, N, Args, [{Arg,Temp}|RegArgs]);
split_args(_, _, StkArgs, RegArgs) ->
  {RegArgs, StkArgs}.

%%% Convert a list of actual parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_actuals([{Src,Dst}|Actuals], Rest) ->
  move_actuals(Actuals, mk_move(Dst, Src, Rest));
move_actuals([], Rest) ->
  Rest.

%%% Convert a list of formal parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_formals([{Dst,Src}|Formals], Rest) ->
  move_formals(Formals, [hipe_epiphany:mk_pseudo_move(Dst, Src) | Rest]);
move_formals([], Rest) ->
  Rest.

%%% Convert a 'fun' operand (MFA, prim, or temp)

conv_fun(Fun, Map) ->
  case hipe_rtl:is_var(Fun) of
    true ->
      conv_dst(Fun, Map);
    false ->
      case hipe_rtl:is_reg(Fun) of
	true ->
	  conv_dst(Fun, Map);
	false ->
	  if is_atom(Fun) ->
	      {hipe_epiphany:mk_prim(Fun), Map};
	     true ->
	      {conv_mfa(Fun), Map}
	  end
      end
  end.

%%% Convert an MFA operand.

conv_mfa({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
  hipe_epiphany:mk_mfa(M, F, A).

%%% Convert an RTL source operand (imm/var/reg).
%%% Returns a temp or a naked integer.

conv_src(Opnd, Map) ->
  case hipe_rtl:is_imm(Opnd) of
    true ->
      Value = hipe_rtl:imm_value(Opnd),
      if is_integer(Value) ->
	  {Value, Map}
      end;
    false ->
      conv_dst(Opnd, Map)
  end.

conv_src_list([O|Os], Map) ->
  {V, Map1} = conv_src(O, Map),
  {Vs, Map2} = conv_src_list(Os, Map1),
  {[V|Vs], Map2};
conv_src_list([], Map) ->
  {[], Map}.

%%% Convert an RTL destination operand (var/reg).

conv_dst(Opnd, Map) ->
  {Name, Type} =
    case hipe_rtl:is_var(Opnd) of
      true ->
	{hipe_rtl:var_index(Opnd), 'tagged'};
      false ->
	case hipe_rtl:is_fpreg(Opnd) of
	  true ->
	    exit({?MODULE, conv_dst, Opnd});
	  false ->
	    {hipe_rtl:reg_index(Opnd), 'untagged'}
	end
    end,
  case hipe_epiphany_registers:is_precoloured(Name) of
    true ->
      {hipe_epiphany:mk_temp(Name, Type), Map};
    false ->
      case vmap_lookup(Map, Opnd) of
	{value, NewTemp} ->
	  {NewTemp, Map};
	_ ->
	  NewTemp = hipe_epiphany:mk_new_temp(Type),
	  {NewTemp, vmap_bind(Map, Opnd, NewTemp)}
      end
  end.

conv_dst_list([O|Os], Map) ->
  {Dst, Map1} = conv_dst(O, Map),
  {Dsts, Map2} = conv_dst_list(Os, Map1),
  {[Dst|Dsts], Map2};
conv_dst_list([], Map) ->
  {[], Map}.

conv_formals(Os, Map) ->
  conv_formals(hipe_epiphany_registers:nr_args(), Os, Map, []).

conv_formals(N, [O|Os], Map, Res) ->
  Type =
    case hipe_rtl:is_var(O) of
      true -> 'tagged';
      _ -> 'untagged'
    end,
  Dst =
    if N > 0 -> hipe_epiphany:mk_new_temp(Type);	% allocatable
       true -> hipe_epiphany:mk_new_nonallocatable_temp(Type)
    end,
  Map1 = vmap_bind(Map, O, Dst),
  conv_formals(N-1, Os, Map1, [Dst|Res]);
conv_formals(_, [], Map, Res) ->
  {lists:reverse(Res), Map}.

mk_sp() ->
  hipe_epiphany:mk_temp(hipe_epiphany_registers:stack_pointer(), 'untagged').

%%% new_untagged_temp -- conjure up an untagged scratch reg

new_untagged_temp() ->
  hipe_epiphany:mk_new_temp('untagged').

%%% new_tagged_temp -- conjure up an tagged scratch reg

new_tagged_temp() ->
  hipe_epiphany:mk_new_temp('tagged').

%%% Map from RTL var/reg operands to temps.

vmap_empty() ->
  gb_trees:empty().

vmap_lookup(Map, Key) ->
  gb_trees:lookup(Key, Map).

vmap_bind(Map, Key, Val) ->
  gb_trees:insert(Key, Val, Map).
