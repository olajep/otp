%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(hipe_epiphany_pp).
-export([pp/1, pp/2, pp_insn/1]).

-include("hipe_epiphany.hrl").

pp(Defun) ->
  pp(standard_io, Defun).

pp(Dev, #defun{mfa={M,F,A}, code=Code, data=Data}) ->
  Fname = atom_to_list(M)++"_"++atom_to_list(F)++"_"++integer_to_list(A),
  io:format(Dev, "\t.text\n", []),
  io:format(Dev, "\t.align 4\n", []),
  io:format(Dev, "\t.global ~s\n", [Fname]),
  io:format(Dev, "~s:\n", [Fname]),
  pp_insns(Dev, Code, Fname),
  io:format(Dev, "\t.rodata\n", []),
  io:format(Dev, "\t.align 4\n", []),
  hipe_data_pp:pp(Dev, Data, arm, Fname),
  io:format(Dev, "\n", []).

pp_insns(Dev, [I|Is], Fname) ->
  pp_insn(Dev, I, Fname),
  pp_insns(Dev, Is, Fname);
pp_insns(_, [], _) ->
  [].

pp_insn(I) ->
  pp_insn(standard_io, I, "").

pp_insn(Dev, I, Pre) ->
  case I of
    #alu{aluop=AluOp, dst=Dst, src1=Src1, src2=Src2} ->
      io:format(Dev, "\t~s ", [alu_op_name(AluOp)]),
      pp_operand(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_operand(Dev, Src1),
      io:format(Dev, ", ", []),
      pp_operand(Dev, Src2),
      io:format(Dev, "\n", []);
    #bcc{'cond'=Cond, label=Label} ->
      io:format(Dev, "\tb~s .~s_~w\n", [cond_name(Cond), Pre, Label]);
    #label{label=Label} ->
      io:format(Dev, ".~s_~w:~n", [Pre, Label]);
    #ldr{size=Size, dst=Dst, base=Base, sign=Sign, offset=Offset} ->
      io:format(Dev, "\tldr~s ", [mem_size_name(Size)]),
      pp_operand(Dev, Dst),
      io:format(Dev, ", [", []),
      pp_operand(Dev, Base),
      io:format(Dev, ", ~s", [addr_sign_name(Sign)]),
      pp_operand(Dev, Offset),
      io:format(Dev, "]\n", []);
    #mov{dst=Dst, src=Src} ->
      io:format(Dev, "\tmov ", []),
      pp_operand(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_operand(Dev, Src),
      io:format(Dev, "\n", []);
    #movt{dst=Dst, src=Src} ->
      io:format(Dev, "\tmovt ", []),
      pp_operand(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_operand(Dev, Src),
      io:format(Dev, "\n", []);
    #movfs{dst=Dst, src=Src} ->
      io:format(Dev, "\tmovfs ", []),
      pp_operand(Dev, Dst),
      io:format(Dev, ", ~s\n", [atom_to_list(Src)]);
    #pseudo_bcc{'cond'=Cond, true_label=TrueLab, false_label=FalseLab, pred=Pred} ->
      io:format(Dev, "\tpseudo_b~s .~s_~w # .~s_~w ~.2f\n",
		[cond_name(Cond), Pre, TrueLab, Pre, FalseLab, Pred]);
    #pseudo_call{funv=FunV, sdesc=SDesc, contlab=ContLab, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_call ", []),
      pp_funv(Dev, FunV),
      io:format(Dev, " # contlab .~s_~w", [Pre, ContLab]),
      pp_sdesc(Dev, Pre, SDesc),
      io:format(Dev, " ~w\n", [Linkage]);
    #pseudo_move{dst=Dst, src=Src} ->
      io:format(Dev, "\tpseudo_move ", []),
      pp_operand(Dev, Dst),
      io:format(Dev, ", ", []),
      pp_operand(Dev, Src),
      io:format(Dev, "\n", []);
    #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_tailcall ", []),
      pp_funv(Dev, FunV),
      io:format(Dev, "/~w (", [Arity]),
      pp_args(Dev, StkArgs),
      io:format(Dev, ") ~w\n", [Linkage]);
    #pseudo_tailcall_prepare{} ->
      io:format(Dev, "\tpseudo_tailcall_prepare\n", []);
    #rts{} ->
      io:format(Dev, "\trts\n", []);
    _ -> exit({?MODULE, pp_insn, I})
  end.

alu_op_name(AluOp) -> atom_to_list(AluOp).

pp_operand(Dev, {lo16, LTImm}) ->
      io:format(Dev, "%low(", []),
      pp_ltimm(Dev, LTImm),
      io:format(Dev, ")", []);
pp_operand(Dev, {hi16, LTImm}) ->
      io:format(Dev, "%high(", []),
      pp_ltimm(Dev, LTImm),
      io:format(Dev, ")", []);
pp_operand(Dev, #epiphany_simm11{value=Value}) -> io:format(Dev, "#~w", [Value]);
%%pp_operand(Dev, #epiphany_simm24{value=Value}) -> io:format(Dev, "#~w", [Value]);
pp_operand(Dev, #epiphany_uimm5{value=Value}) -> io:format(Dev, "#~w", [Value]);
pp_operand(Dev, #epiphany_uimm11{value=Value}) -> io:format(Dev, "#~w", [Value]);
pp_operand(Dev, #epiphany_uimm16{value=Value}) -> io:format(Dev, "#~w", [Value]);
pp_operand(Dev, Temp=#epiphany_temp{reg=Reg, type=Type}) ->
  case hipe_epiphany:temp_is_precoloured(Temp) of
    true ->
      Name = hipe_epiphany_registers:reg_name(Reg),
      io:format(Dev, "~s", [Name]);
    false ->
      Tag =
	case Type of
	  tagged -> "t";
	  untagged -> "u"
	end,
      io:format(Dev, "~s~w", [Tag, Reg])
  end.

pp_ltimm(Dev, {label, Label}) ->
  io:format(Dev, "~w", [Label]);
pp_ltimm(Dev, Atom) when is_atom(Atom) ->
  io:format(Dev, "%atom(~w)", [Atom]).

to_hex(N) ->
  io_lib:format("~.16x", [N, "0x"]).

pp_sdesc(Dev, Pre, #epiphany_sdesc{
		      exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live}) ->
  pp_sdesc_exnlab(Dev, Pre, ExnLab),
  io:format(Dev, " ~s ~w [", [to_hex(FSize), Arity]),
  pp_sdesc_live(Dev, Live),
  io:format(Dev, "]", []).

pp_sdesc_exnlab(Dev, _, []) -> io:format(Dev, " []", []);
pp_sdesc_exnlab(Dev, Pre, ExnLab) -> io:format(Dev, " .~s_~w", [Pre, ExnLab]).

pp_sdesc_live(_, {}) -> [];
pp_sdesc_live(Dev, Live) -> pp_sdesc_live(Dev, Live, 1).

pp_sdesc_live(Dev, Live, I) ->
  io:format(Dev, "~s", [to_hex(element(I, Live))]),
  if I < tuple_size(Live) ->
      io:format(Dev, ",", []),
      pp_sdesc_live(Dev, Live, I+1);
     true -> []
  end.

pp_fun(Dev, Fun) ->
  case Fun of
    #epiphany_mfa{m=M, f=F, a=A} ->
      io:format(Dev, "~w:~w/~w", [M, F, A]);
    #epiphany_prim{prim=Prim} ->
      io:format(Dev, "~w", [Prim])
  end.

pp_funv(Dev, FunV) ->
  case FunV of
    #epiphany_temp{} ->
      pp_operand(Dev, FunV);
    Fun ->
      pp_fun(Dev, Fun)
  end.

cond_name(always) -> "";
cond_name(Cond) -> atom_to_list(Cond).

mem_size_name('w') -> "";
mem_size_name(Size) -> atom_to_list(Size).

addr_sign_name('+') -> "";
addr_sign_name('-') -> "-".

pp_args(Dev, [A|As]) ->
  pp_operand(Dev, A),
  pp_comma_args(Dev, As);
pp_args(_, []) ->
  [].

pp_comma_args(Dev, [A|As]) ->
  io:format(Dev, ", ", []),
  pp_operand(Dev, A),
  pp_comma_args(Dev, As);
pp_comma_args(_, []) ->
  [].
