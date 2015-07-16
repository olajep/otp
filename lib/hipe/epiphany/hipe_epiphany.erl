%%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2015. All Rights Reserved.
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

-module(hipe_epiphany).
-export([
	 mk_temp/2,
	 mk_new_temp/1,
	 mk_new_nonallocatable_temp/1,
	 is_temp/1,
	 temp_reg/1,
	 temp_type/1,
	 temp_is_allocatable/1,
	 temp_is_precoloured/1,

	 mk_simm11/1,
	 mk_uimm5/1,
	 mk_uimm11/1,
	 mk_uimm16/1,

	 mk_mfa/3,

	 mk_alu/4,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_movi/3,

	 mk_pseudo_move/2,
	 is_pseudo_move/1,
	 pseudo_move_dst/1,
	 pseudo_move_src/1,

	 mk_defun/8,
	 defun_mfa/1,
	 defun_formals/1,
	 defun_is_closure/1,
	 defun_is_leaf/1,
	 defun_code/1,
	 defun_data/1,
	 defun_var_range/1
	]).

-include("hipe_epiphany.hrl").

mk_temp(Reg, Type, Allocatable) ->
  #epiphany_temp{reg=Reg, type=Type, allocatable=Allocatable}.
mk_temp(Reg, Type) -> mk_temp(Reg, Type, true).
mk_new_temp(Type, Allocatable) ->
  mk_temp(hipe_gensym:get_next_var(epiphany), Type, Allocatable).
mk_new_temp(Type) -> mk_new_temp(Type, true).
mk_new_nonallocatable_temp(Type) -> mk_new_temp(Type, false).
is_temp(X) -> case X of #epiphany_temp{} -> true; _ -> false end.
temp_reg(#epiphany_temp{reg=Reg}) -> Reg.
temp_type(#epiphany_temp{type=Type}) -> Type.
temp_is_allocatable(#epiphany_temp{allocatable=A}) -> A.
temp_is_precoloured(#epiphany_temp{reg=Reg}) ->
    hipe_epiphany_registers:is_precoloured(Reg).

mk_simm11(Value) when -16#400 =< Value, Value < 16#400 ->
  #epiphany_simm11{value=Value}.

mk_uimm5(Value) when 0 =< Value, Value < 16#20 ->
  #epiphany_uimm5{value=Value}.

mk_uimm11(Value) when 0 =< Value, Value < 16#800 ->
  #epiphany_uimm11{value=Value}.

mk_uimm16(Value) when 0 =< Value, Value < 16#10000 ->
  #epiphany_uimm16{value=Value}.

mk_mfa(M, F, A) -> #epiphany_mfa{m=M, f=F, a=A}.

mk_alu(AluOp, Dst, Src1, Src2) ->
  #alu{aluop=AluOp, dst=Dst, src1=Src1, src2=Src2}.

mk_label(Label) -> #label{label=Label}.
is_label(I) -> case I of #label{} -> true; _ -> false end.
label_label(#label{label=Label}) -> Label.

%% mk_movcc(Cond, Dst, Src) ->
%%   #movcc{'cond'=Cond, dst=Dst, src=Src}.

mk_mov(Dst=#epiphany_temp{allocatable=true}, Imm=#epiphany_uimm16{}) ->
  #mov{dst=Dst, src=Imm}%% ;
%% mk_mov(Dst=#epiphany_temp{allocatable=true}, Src=#epiphany_temp{allocatable=true}) ->
%%   mk_movcc('always', Dst, Src)
    .

mk_movt(Dst=#epiphany_temp{allocatable=true}, Src=#epiphany_uimm16{}) ->
  #movt{dst=Dst, src=Src}.

mk_movi(Dst, Value, Tail) ->
  if 0 =< Value, Value < 16#10000 ->
      [mk_mov(Dst, mk_uimm16(Value)) | Tail];
     true ->
      Hi16 = mk_uimm16((Value bsr 16) band 16#FFFF),
      Lo16 = mk_uimm16(Value band 16#FFFF),
      [mk_mov(Dst, Lo16),
       mk_movt(Dst, Hi16) |
       Tail]
  end.

mk_pseudo_move(Dst, Src) -> #pseudo_move{dst=Dst, src=Src}.
is_pseudo_move(I) -> case I of #pseudo_move{} -> true; _ -> false end.
pseudo_move_dst(#pseudo_move{dst=Dst}) -> Dst.
pseudo_move_src(#pseudo_move{src=Src}) -> Src.

mk_defun(MFA, Formals, IsClosure, IsLeaf, Code, Data, VarRange, LabelRange) ->
  #defun{mfa=MFA, formals=Formals, code=Code, data=Data,
	 isclosure=IsClosure, isleaf=IsLeaf,
	 var_range=VarRange, label_range=LabelRange}.
defun_mfa(#defun{mfa=MFA}) -> MFA.
defun_formals(#defun{formals=Formals}) -> Formals.
defun_is_closure(#defun{isclosure=IsClosure}) -> IsClosure.
defun_is_leaf(#defun{isleaf=IsLeaf}) -> IsLeaf.
defun_code(#defun{code=Code}) -> Code.
defun_data(#defun{data=Data}) -> Data.
defun_var_range(#defun{var_range=VarRange}) -> VarRange.
