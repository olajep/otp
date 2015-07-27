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
	 mk_simm24/1,

	 mk_mfa/3,

	 mk_prim/1,
	 is_prim/1,
	 prim_prim/1,

	 mk_sdesc/4,

	 mk_alu/4,

	 mk_b/2,

	 mk_bcc/2,

	 mk_bl/3,

	 mk_comment/1,

	 mk_jalr/2,

	 mk_jr/1,

	 mk_label/1,
	 is_label/1,
	 label_label/1,

	 mk_ldr/5,

	 mk_movcc/3,

	 mk_movi/2,
	 mk_movi/3,

	 mk_movfs/2,

	 mk_rts/1,

	 mk_str/5,

	 mk_pseudo_bcc/4,

	 mk_pseudo_call/4,
	 pseudo_call_contlab/1,
	 pseudo_call_funv/1,
	 pseudo_call_sdesc/1,
	 pseudo_call_linkage/1,

	 mk_pseudo_move/2,
	 is_pseudo_move/1,
	 pseudo_move_dst/1,
	 pseudo_move_src/1,

	 mk_pseudo_switch/3,

	 mk_pseudo_tailcall/4,
	 pseudo_tailcall_funv/1,
	 pseudo_tailcall_stkargs/1,
	 pseudo_tailcall_linkage/1,

	 mk_pseudo_tailcall_prepare/0,

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

-export_type([instr/0]).

-spec mk_temp(reg(), type(), boolean()) -> pseudo_temp().
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

mk_simm24(Value) when -16#800000 =< Value, Value < 16#800000 ->
  #epiphany_simm24{value=Value}.

mk_mfa(M, F, A) -> #epiphany_mfa{m=M, f=F, a=A}.

mk_prim(Prim) when is_atom(Prim) -> #epiphany_prim{prim=Prim}.
is_prim(X) -> case X of #epiphany_prim{} -> true; _ -> false end.
prim_prim(#epiphany_prim{prim=Prim}) -> Prim.

mk_sdesc(ExnLab, FSize, Arity, Live)
  when (([] == ExnLab) or (is_integer(ExnLab) and (ExnLab >= 0))),
       is_integer(FSize), FSize >= 0, is_integer(Arity), Arity >= 0,
       is_tuple(Live) ->
  #epiphany_sdesc{exnlab=ExnLab, fsize=FSize, arity=Arity, live=Live}.

-spec mk_alu(aluop(), temp(), temp(),
	     temp() | #epiphany_simm11{} | #epiphany_uimm5{}) -> alu().
mk_alu(AluOp, Dst, Src1, Src2) ->
  #alu{aluop=AluOp, dst=Dst, src1=Src1, src2=Src2}.

-spec mk_b(#epiphany_mfa{} | #epiphany_prim{}, linkage()) -> b().
mk_b(FunLit, Linkage) ->
  #b{funv=FunLit, linkage=Linkage}.

-spec mk_bcc('cond'(), non_neg_integer()) -> bcc().
mk_bcc(Cond, Label) ->
  #bcc{'cond'=Cond, label=Label}.

-spec mk_comment(term()) -> comment().
mk_comment(Term) ->
  #comment{term=Term}.

-spec mk_bl(#epiphany_mfa{} | #epiphany_prim{}, #epiphany_sdesc{}, linkage())
	   -> bl().
mk_bl(FunLit, SDesc,Linkage) ->
  #bl{funv=FunLit, sdesc=SDesc, linkage=Linkage}.

-spec mk_jalr(temp(), #epiphany_sdesc{}) -> jalr().
mk_jalr(FunV, SDesc) ->
  #jalr{funv=FunV, sdesc=SDesc}.

-spec mk_jr(temp()) -> jr().
mk_jr(FunV) ->
  #jr{funv=FunV}.

-spec mk_label(non_neg_integer()) -> label().
mk_label(Label) when is_integer(Label), Label >= 0 -> #label{label=Label}.
is_label(I) -> case I of #label{} -> true; _ -> false end.
label_label(#label{label=Label}) -> Label.

-spec mk_movcc('cond'(), temp(), temp()) -> movcc().
mk_movcc(Cond, Dst, Src) ->
  #movcc{'cond'=Cond, dst=Dst, src=Src}.

-spec mk_mov(temp(), #epiphany_uimm16{} | {lo16, link_time_immediate()})
	    -> mov().
mk_mov(Dst=#epiphany_temp{allocatable=true}, Imm=#epiphany_uimm16{}) ->
  #mov{dst=Dst, src=Imm};
mk_mov(Dst=#epiphany_temp{allocatable=true}, Imm={lo16, _}) ->
  #mov{dst=Dst, src=Imm}.

-spec mk_movt(temp(), #epiphany_uimm16{} | {hi16, link_time_immediate()})
	     -> movt().
mk_movt(Dst=#epiphany_temp{allocatable=true}, Src=#epiphany_uimm16{}) ->
  #movt{dst=Dst, src=Src};
mk_movt(Dst=#epiphany_temp{allocatable=true}, Src={hi16, _}) ->
  #movt{dst=Dst, src=Src}.

-spec mk_ldr(mem_size(), temp(), temp(), addr_sign(),
	     temp() | #epiphany_uimm11{}) -> ldr().
mk_ldr(Size, Dst, Base, Sign, Offset) ->
  #ldr{size=Size, dst=Dst, base=Base, sign=Sign, offset=Offset}.

-spec mk_movi(temp(), integer() | link_time_immediate()) -> [mov() | movt()].
mk_movi(Dst, Value) -> mk_movi(Dst, Value, []).

-spec mk_movi(temp(), integer() | link_time_immediate(), [T])
	     -> [mov() | movt() | T].
mk_movi(Dst, Value, Tail) when is_integer(Value) ->
  if 0 =< Value, Value < 16#10000 ->
      [mk_mov(Dst, mk_uimm16(Value)) | Tail];
     true ->
      Hi16 = mk_uimm16((Value bsr 16) band 16#FFFF),
      Lo16 = mk_uimm16(Value band 16#FFFF),
      [mk_mov(Dst, Lo16),
       mk_movt(Dst, Hi16) |
       Tail]
  end;
mk_movi(Dst, Value, Tail) ->
  ok = case Value of
	 Atom when is_atom(Atom) -> ok;
	 {label, _} -> ok
       end,
  [mk_mov(Dst, {lo16, Value}),
   mk_movt(Dst, {hi16, Value}) |
   Tail].

-spec mk_movfs(temp(), spec_reg()) -> movfs().
mk_movfs(Dst, Src) ->
  #movfs{dst=Dst, src=Src}.

-spec mk_rts(non_neg_integer()) -> rts().
mk_rts(NrRets) when is_integer(NrRets) -> #rts{nr_rets=NrRets}.

-spec mk_str(mem_size(), temp(), temp(), addr_sign(),
	     temp() | #epiphany_uimm11{}) -> str().
mk_str(Size, Src=#epiphany_temp{allocatable=true},
       Base=#epiphany_temp{allocatable=true}, Sign, Offset) ->
  case Offset of
    #epiphany_temp{allocatable=true} -> ok;
    #epiphany_uimm11{} -> ok
  end,
  #str{size=Size, src=Src, base=Base, sign=Sign, offset=Offset}.

-spec mk_pseudo_bcc('cond'(), non_neg_integer(), non_neg_integer(), number())
		   -> pseudo_bcc().
mk_pseudo_bcc(Cond, TrueLab, FalseLab, Pred) ->
  if Pred >= 0.5 ->
      mk_pseudo_bcc_simple(negate_cond(Cond), FalseLab,
			  TrueLab, 1.0-Pred);
     true ->
      mk_pseudo_bcc_simple(Cond, TrueLab, FalseLab, Pred)
  end.

mk_pseudo_bcc_simple(Cond, TrueLab, FalseLab, Pred) when Pred =< 0.5 ->
  #pseudo_bcc{'cond'=Cond, true_label=TrueLab,
	      false_label=FalseLab, pred=Pred}.

negate_cond(Cond) ->
  case Cond of
    'eq'   -> 'ne';	% ==, !=
    'ne'   -> 'eq';	% !=, ==
    'lt'   -> 'gte';	% <, >=
    'gte'  -> 'lt';	% >=, <
    'gt'   -> 'lte';	% >, <=
    'lte'  -> 'gt';	% <=, >
    'ltu'  -> 'gteu';	% <u, >=u
    'gteu' -> 'ltu';	% >=u, <u
    'gtu'  -> 'lteu';	% >u, <=u
    'lteu' -> 'gtu';	% <=u, >u
    'beq'  -> 'bne';	% == (FPU), != (FPU)
    'bne'  -> 'beq'	% =! (FPU), == (FPU)
  end.

-spec mk_pseudo_call(#epiphany_mfa{} | #epiphany_prim{} | temp(),
		     #epiphany_sdesc{}, non_neg_integer(), linkage())
		    -> pseudo_call().
mk_pseudo_call(FunV, SDesc, ContLab, Linkage) ->
  #pseudo_call{funv=FunV, sdesc=SDesc, contlab=ContLab, linkage=Linkage}.
pseudo_call_funv(#pseudo_call{funv=FunV}) -> FunV.
pseudo_call_sdesc(#pseudo_call{sdesc=SDesc}) -> SDesc.
pseudo_call_contlab(#pseudo_call{contlab=ContLab}) -> ContLab.
pseudo_call_linkage(#pseudo_call{linkage=Linkage}) -> Linkage.

-spec mk_pseudo_move(#epiphany_temp{allocatable::false}, temp()) -> pseudo_move();
		    (temp(), #epiphany_temp{allocatable::false}) -> pseudo_move();
		    (temp(), temp()) -> pseudo_move().
mk_pseudo_move(Dst, Src) -> #pseudo_move{dst=Dst, src=Src}.
is_pseudo_move(I) -> case I of #pseudo_move{} -> true; _ -> false end.
pseudo_move_dst(#pseudo_move{dst=Dst}) -> Dst.
pseudo_move_src(#pseudo_move{src=Src}) -> Src.

-spec mk_pseudo_switch(temp(), temp(), [non_neg_integer()])
		      -> pseudo_switch().
mk_pseudo_switch(JTab, Index, Labels) ->
  #pseudo_switch{jtab=JTab, index=Index, labels=Labels}.

-spec mk_pseudo_tailcall(#epiphany_mfa{} | #epiphany_prim{} | temp(),
			 arity(), [temp()], linkage()) -> pseudo_tailcall().
mk_pseudo_tailcall(FunV, Arity, StkArgs, Linkage=remote) ->
  #pseudo_tailcall{funv=FunV, arity=Arity, stkargs=StkArgs, linkage=Linkage}.
pseudo_tailcall_funv(#pseudo_tailcall{funv=FunV}) -> FunV.
pseudo_tailcall_stkargs(#pseudo_tailcall{stkargs=StkArgs}) -> StkArgs.
pseudo_tailcall_linkage(#pseudo_tailcall{linkage=Linkage}) -> Linkage.

mk_pseudo_tailcall_prepare() -> #pseudo_tailcall_prepare{}.

-spec mk_defun(mfa(), [pseudo_temp()], boolean(), boolean(), [instr()],
	       hipe_consttab(),
	       [] | {non_neg_integer(), non_neg_integer()},
	       [] | {non_neg_integer(), non_neg_integer()}) -> #defun{}.
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
