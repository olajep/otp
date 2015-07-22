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
      io:format(Dev, "\t~s ~s, ~s, ~s\n",
		[alu_op(AluOp), operand(Dst), operand(Src1), operand(Src2)]);
    #b{funv=FunV, linkage=Linkage} ->
      io:format(Dev, "\tb ~s # ~w\n",
		[funv(FunV), Linkage]);
    #bcc{'cond'=Cond, label=Label} ->
      io:format(Dev, "\tb~s .~s_~w\n",
		[cond_name(Cond), Pre, Label]);
    #bl{funv=FunV, sdesc=SDesc, linkage=Linkage} ->
      io:format(Dev, "\tbl ~s #~s ~w\n",
		[funv(FunV), sdesc(Pre, SDesc), Linkage]);
    #comment{term=Term} ->
      io:format(Dev, "\t# ~p\n",
		[Term]);
    #jalr{funv=FunV, sdesc=SDesc} ->
      io:format(Dev, "\tjalr ~s #~s\n",
		[funv(FunV), sdesc(Pre, SDesc)]);
    #jr{funv=FunV} ->
      io:format(Dev, "\tjr ~s\n",
		[funv(FunV)]);
    #label{label=Label} ->
      io:format(Dev, ".~s_~w:~n",
		[Pre, Label]);
    #ldr{size=Size, dst=Dst, base=Base, sign=Sign, offset=Offset} ->
      io:format(Dev, "\tldr~s ~s, [~s, ~s~s]\n",
		[mem_size(Size), operand(Dst), operand(Base), addr_sign(Sign),
		 operand(Offset)]);
    #movcc{'cond'=Cond, dst=Dst, src=Src} ->
      io:format(Dev, "\tmov~s ~s, ~s\n",
		[cond_name(Cond), operand(Dst), operand(Src)]);
    #mov{dst=Dst, src=Src} ->
      io:format(Dev, "\tmov ~s, ~s\n",
		[operand(Dst), operand(Src)]);
    #movt{dst=Dst, src=Src} ->
      io:format(Dev, "\tmovt ~s, ~s\n",
		[operand(Dst), operand(Src)]);
    #movfs{dst=Dst, src=Src} ->
      io:format(Dev, "\tmovfs ~s, ~s\n",
		[operand(Dst), atom_to_list(Src)]);
    #pseudo_bcc{'cond'=Cond, true_label=TrueLab, false_label=FalseLab,
		pred=Pred} ->
      io:format(Dev, "\tpseudo_b~s .~s_~w # .~s_~w ~.2f\n",
		[cond_name(Cond), Pre, TrueLab, Pre, FalseLab, Pred]);
    #pseudo_call{funv=FunV, sdesc=SDesc, contlab=ContLab, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_call ~s # contlab .~s_~w~s ~w\n",
		[funv(FunV), Pre, ContLab, sdesc(Pre, SDesc), Linkage]);
    #pseudo_move{dst=Dst, src=Src} ->
      io:format(Dev, "\tpseudo_move ~s, ~s\n",
		[operand(Dst), operand(Src)]);
    #pseudo_switch{jtab=JTab, index=Index, labels=Labels} ->
      io:format(Dev, "\tpseudo_switch ~s[~s] #~s\n",
		[operand(JTab), operand(Index), labels(Labels, Pre)]);
    #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage} ->
      io:format(Dev, "\tpseudo_tailcall ~s/~w (~s) ~w\n",
		[funv(FunV), Arity, pp_args(StkArgs), Linkage]);
    #pseudo_tailcall_prepare{} ->
      io:format(Dev, "\tpseudo_tailcall_prepare\n",
		[]);
    #rts{} ->
      io:format(Dev, "\trts\n",
		[]);
    #str{size=Size, src=Src, base=Base, sign=Sign, offset=Offset} ->
      io:format(Dev, "\tstr~s ~s, [~s, ~s~s]\n",
		[mem_size(Size), operand(Src), operand(Base),
		 addr_sign(Sign), operand(Offset)]);
    _ -> exit({?MODULE, pp_insn, I})
  end.

alu_op(AluOp) -> atom_to_list(AluOp).

operand({lo16, LTImm}) ->
      io_lib:format("%low(~s)", [pp_ltimm(LTImm)]);
operand({hi16, LTImm}) ->
      io_lib:format("%high(~s)", [pp_ltimm(LTImm)]);
operand(#epiphany_simm11{value=Value}) -> io_lib:format("#~w", [Value]);
%%pp_operand(#epiphany_simm24{value=Value}) -> io_lib:format("#~w", [Value]);
operand(#epiphany_uimm5{value=Value}) -> io_lib:format("#~w", [Value]);
operand(#epiphany_uimm11{value=Value}) -> io_lib:format("#~w", [Value]);
operand(#epiphany_uimm16{value=Value}) -> io_lib:format("#~w", [Value]);
operand(Temp=#epiphany_temp{reg=Reg, type=Type}) ->
  case hipe_epiphany:temp_is_precoloured(Temp) of
    true ->
      hipe_epiphany_registers:reg_name(Reg);
    false ->
      Tag =
	case Type of
	  tagged -> "t";
	  untagged -> "u"
	end,
      io_lib:format("~s~w", [Tag, Reg])
  end.

pp_ltimm({label, Label}) ->
  io_lib:format("~w", [Label]);
pp_ltimm(Atom) when is_atom(Atom) ->
  io_lib:format("%atom(~w)", [Atom]).

to_hex(N) ->
  io_lib:format("~.16x", [N, "0x"]).

sdesc(Pre, #epiphany_sdesc{
	      exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live}) ->
  io_lib:format("~s ~s ~w [~s]",
		[pp_sdesc_exnlab(Pre, ExnLab), to_hex(FSize), Arity,
		 pp_sdesc_live(Live)]).

pp_sdesc_exnlab(_, []) -> " []";
pp_sdesc_exnlab(Pre, ExnLab) -> io_lib:format(" .~s_~w", [Pre, ExnLab]).

pp_sdesc_live({}) -> [];
pp_sdesc_live(Live) -> pp_sdesc_live(Live, 1).

pp_sdesc_live(Live, I) ->
  [to_hex(element(I, Live)) |
    if I < tuple_size(Live) ->
	"," ++
	  pp_sdesc_live(Live, I+1);
       true -> ""
    end].

labels([Label|Labels], Pre) ->
  [io_lib:format(" .~s_~w", [Pre, Label]) |
   labels(Labels, Pre)];
labels([], _Pre) -> "".

pp_fun(Fun) ->
  case Fun of
    #epiphany_mfa{m=M, f=F, a=A} ->
      io_lib:format("~w:~w/~w", [M, F, A]);
    #epiphany_prim{prim=Prim} ->
      io_lib:format("~w", [Prim])
  end.

funv(FunV) ->
  case FunV of
    #epiphany_temp{} ->
      operand(FunV);
    Fun ->
      pp_fun(Fun)
  end.

cond_name(always) -> "";
cond_name(Cond) -> atom_to_list(Cond).

mem_size('w') -> "";
mem_size(Size) -> atom_to_list(Size).

addr_sign('+') -> "";
addr_sign('-') -> "-".

pp_args([A|As]) -> [operand(A) | pp_comma_args(As)];
pp_args([]) -> "".

pp_comma_args([A|As]) ->
  [", ",
   operand(A),
   pp_comma_args(As)];
pp_comma_args([]) ->
  "".
