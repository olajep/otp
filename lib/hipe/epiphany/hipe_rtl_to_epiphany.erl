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
%%%   integer multiplications to outnumber floating point operations, we tweak
%%%   this convention by requiring that the mode is "integer" at both function
%%%   entry and exit.
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
  io:format("~w \n  ==>\n ~w\n- - - - - - - - -\n",[H,NewH]),
  {NewT, NewData2} = conv_insn_list(T, NewMap, NewData1),
  {NewH ++ NewT, NewData2};
conv_insn_list([], _, Data) ->
  {[], Data}.

conv_insn(I, Map, Data) ->
  case I of
    #alu{} -> conv_alu(I, Map, Data);
    %% #alub{} -> conv_alub(I, Map, Data);
    %% #branch{} -> conv_branch(I, Map, Data);
    %% #call{} -> conv_call(I, Map, Data);
    %% #comment{} -> conv_comment(I, Map, Data);
    %% #enter{} -> conv_enter(I, Map, Data);
    %% #goto{} -> conv_goto(I, Map, Data);
    %% #label{} -> conv_label(I, Map, Data);
    %% #load{} -> conv_load(I, Map, Data);
    %% #load_address{} -> conv_load_address(I, Map, Data);
    %% #load_atom{} -> conv_load_atom(I, Map, Data);
    #move{} -> conv_move(I, Map, Data);
    %% #return{} -> conv_return(I, Map, Data);
    %% #store{} -> conv_store(I, Map, Data);
    %% #switch{} -> conv_switch(I, Map, Data);
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
    none ->
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

%%% Convert a list of formal parameters passed in
%%% registers (from split_args/1) to a list of moves.

move_formals([{Dst,Src}|Formals], Rest) ->
  move_formals(Formals, [hipe_epiphany:mk_pseudo_move(Src, Dst) | Rest]);
move_formals([], Rest) ->
  Rest.

%%% Convert an MFA operand.

%% conv_mfa({M,F,A}) when is_atom(M), is_atom(F), is_integer(A) ->
%%   hipe_epiphany:mk_mfa(M, F, A).

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

%% conv_src_list([O|Os], Map) ->
%%   {V, Map1} = conv_src(O, Map),
%%   {Vs, Map2} = conv_src_list(Os, Map1),
%%   {[V|Vs], Map2};
%% conv_src_list([], Map) ->
%%   {[], Map}.

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

%% conv_dst_list([O|Os], Map) ->
%%   {Dst, Map1} = conv_dst(O, Map),
%%   {Dsts, Map2} = conv_dst_list(Os, Map1),
%%   {[Dst|Dsts], Map2};
%% conv_dst_list([], Map) ->
%%   {[], Map}.

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

%%% new_untagged_temp -- conjure up an untagged scratch reg

new_untagged_temp() ->
  hipe_epiphany:mk_new_temp('untagged').

%%% Map from RTL var/reg operands to temps.

vmap_empty() ->
  gb_trees:empty().

vmap_lookup(Map, Key) ->
  gb_trees:lookup(Key, Map).

vmap_bind(Map, Key, Val) ->
  gb_trees:insert(Key, Val, Map).
