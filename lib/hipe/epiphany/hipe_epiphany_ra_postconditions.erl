%% -*- erlang-indent-level: 2 -*-
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

-module(hipe_epiphany_ra_postconditions).

-export([check_and_rewrite/3, check_and_rewrite2/3]).

-include("hipe_epiphany.hrl").

check_and_rewrite(Defun, Coloring, Allocator) ->
  TempMap = hipe_temp_map:cols2tuple(Coloring, hipe_epiphany_specific),
  check_and_rewrite2(Defun, TempMap, Allocator).

check_and_rewrite2(Defun, TempMap, Allocator) ->
  Strategy = strategy(Allocator),
  #defun{code=Code0} = Defun,
  {Code1,DidSpill} = do_insns(Code0, TempMap, Strategy, [], false),
  VarRange = {0, hipe_gensym:get_var(epiphany)},
  {Defun#defun{code=Code1, var_range=VarRange},
   DidSpill}.

strategy(Allocator) ->
  case Allocator of
    'normal' -> 'new';
    'linearscan' -> 'fixed';
    'naive' -> 'fixed'
  end.

do_insns([I|Insns], TempMap, Strategy, Accum, DidSpill0) ->
  {NewIs, DidSpill1} = do_insn(I, TempMap, Strategy),
  do_insns(Insns, TempMap, Strategy, lists:reverse(NewIs, Accum), DidSpill0 or DidSpill1);
do_insns([], _TempMap, _Strategy, Accum, DidSpill) ->
  {lists:reverse(Accum), DidSpill}.

do_insn(I, TempMap, Strategy) ->
  case I of
    #alu{} -> do_alu(I, TempMap, Strategy);
    #ldr{} -> do_ldr(I, TempMap, Strategy);
    %% #movcc{} -> do_movcc(I, TempMap, Strategy);
    #mov{} -> do_mov(I, TempMap, Strategy);
    #movt{} -> do_movt(I, TempMap, Strategy);
    #movfs{} -> do_movfs(I, TempMap, Strategy);
    #pseudo_call{} -> do_pseudo_call(I, TempMap, Strategy);
    #pseudo_move{} -> do_pseudo_move(I, TempMap, Strategy);
    %%#pseudo_switch{} -> do_pseudo_switch(I, TempMap, Strategy);
    #pseudo_tailcall{} -> do_pseudo_tailcall(I, TempMap, Strategy);
    %% rts should not be needed -- it only reads precoloured registers
    #str{} -> do_str(I, TempMap, Strategy);
    _ -> {[I], false}
  end.

%%% Fix relevant instruction types.

do_alu(I=#alu{dst=Dst,src1=Src1,src2=Src2}, TempMap, Strategy) ->
  {FixDst, NewDst, DidSpill1} = fix_dst (Dst,  TempMap, Strategy),
  {FixSrc1,NewSrc1,DidSpill2} = fix_src1(Src1, TempMap, Strategy),
  {FixSrc2,NewSrc2,DidSpill3} = fix_src2(Src2, TempMap, Strategy),
  NewI = I#alu{dst=NewDst,src1=NewSrc1,src2=NewSrc2},
  {FixSrc1 ++ FixSrc2 ++ [NewI | FixDst], DidSpill1 or DidSpill2 or DidSpill3}.

do_ldr(I=#ldr{dst=Dst,base=Base,offset=Offset}, TempMap, Strategy) ->
  {FixDst,   NewDst,   DidSpill1} = fix_dst (Dst,    TempMap, Strategy),
  {FixBase,  NewBase,  DidSpill2} = fix_src1(Base,   TempMap, Strategy),
  {FixOffset,NewOffset,DidSpill3} = fix_src2(Offset, TempMap, Strategy),
  NewI = I#ldr{dst=NewDst,base=NewBase,offset=NewOffset},
  {FixBase ++ FixOffset ++ [NewI | FixDst], DidSpill1 or DidSpill2 or DidSpill3}.

do_mov(I=#mov{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#mov{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_movt(I=#movt{dst=Dst}, TempMap, Strategy) ->
  %% Since movt reads the destination register, it is essentially a 2-address
  %% form instruction.
  {FixSrc,FixDst,NewDst,DidSpill} = fix_srcdst(Dst, TempMap, Strategy),
  NewI = I#movt{dst=NewDst},
  {FixSrc ++ [NewI | FixDst], DidSpill}.

do_movfs(I=#movfs{dst=Dst}, TempMap, Strategy) ->
  {FixDst,NewDst,DidSpill} = fix_dst(Dst, TempMap, Strategy),
  NewI = I#movfs{dst=NewDst},
  {[NewI | FixDst], DidSpill}.

do_pseudo_call(I=#pseudo_call{funv=FunV}, TempMap, Strategy) ->
  {FixFunV,NewFunV,DidSpill} = fix_funv(FunV, TempMap, Strategy),
  NewI = I#pseudo_call{funv=NewFunV},
  {FixFunV ++ [NewI], DidSpill}.

do_pseudo_move(I=#pseudo_move{dst=Dst,src=Src}, TempMap, Strategy) ->
  %% Either Dst or Src (but not both) may be a pseudo temp.
  %% pseudo_move and pseudo_tailcall are special cases: in
  %% all other instructions, all temps must be non-pseudos
  %% after register allocation.
  case temp_is_spilled(Dst, TempMap) of
    true -> % Src must not be a pseudo
      {FixSrc,NewSrc,DidSpill} = fix_src1(Src, TempMap, Strategy),
      NewI = I#pseudo_move{src=NewSrc},
      {FixSrc ++ [NewI], DidSpill};
    _ ->
      {[I], false}
  end.

do_pseudo_tailcall(I=#pseudo_tailcall{funv=FunV}, TempMap, Strategy) ->
  {FixFunV,NewFunV,DidSpill} = fix_funv(FunV, TempMap, Strategy),
  NewI = I#pseudo_tailcall{funv=NewFunV},
  {FixFunV ++ [NewI], DidSpill}.

do_str(I=#str{src=Src,base=Base,offset=Offset}, TempMap, Strategy) ->
  {FixSrc,   NewSrc,   DidSpill1} = fix_src1(Src,    TempMap, Strategy),
  {FixBase,  NewBase,  DidSpill2} = fix_src2(Base,   TempMap, Strategy),
  {FixOffset,NewOffset,DidSpill3} = fix_src3(Offset, TempMap, Strategy),
  NewI = I#str{src=NewSrc,base=NewBase,offset=NewOffset},
  {FixSrc ++ FixBase ++ FixOffset ++ [NewI], DidSpill1 or DidSpill2 or DidSpill3}.

%%% Fix Dst and Src operands.

fix_funv(FunV, TempMap, Strategy) ->
  case FunV of
    #epiphany_temp{} -> fix_src1(FunV, TempMap, Strategy);
    _ -> {[], FunV, false}
  end.

fix_src1(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp1(Strategy)).

temp1('new') -> [];
temp1('fixed') -> hipe_epiphany_registers:temp1().

fix_src2(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp2(Strategy)).

temp2('new') -> [];
temp2('fixed') -> hipe_epiphany_registers:temp2().

fix_src3(Src, TempMap, Strategy) ->
  fix_src(Src, TempMap, temp3(Strategy)).

temp3('new') -> [];
temp3('fixed') -> hipe_epiphany_registers:temp3().

fix_src(Src, TempMap, RegOpt) ->
  case Src of
    #epiphany_temp{} ->
      case temp_is_spilled(Src, TempMap) of
	true ->
	  NewSrc = clone(Src, RegOpt),
	  {[hipe_epiphany:mk_pseudo_move(NewSrc, Src)],
	   NewSrc,
	   true};
	_ ->
	  {[], Src, false}
      end;
    _Immediate ->
      {[], Src, false}
  end.

fix_dst(Dst, TempMap, Strategy) ->
  fix_dst_common(Dst, TempMap, temp1(Strategy)).

fix_dst_common(Dst, TempMap, RegOpt) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst, RegOpt),
      {[hipe_epiphany:mk_pseudo_move(Dst, NewDst)], NewDst, true};
    _ ->
      {[], Dst, false}
  end.

fix_srcdst(Dst, TempMap, Strategy) ->
  case temp_is_spilled(Dst, TempMap) of
    true ->
      NewDst = clone(Dst, temp1(Strategy)),
      {[hipe_epiphany:mk_pseudo_move(NewDst, Dst)],
       [hipe_epiphany:mk_pseudo_move(Dst, NewDst)], NewDst, true};
    _ ->
      {[], [], Dst, false}
  end.

%%% Check if an operand is a pseudo-temp.

temp_is_spilled(Temp, []) -> % special case for naive regalloc
  not(hipe_epiphany:temp_is_precoloured(Temp));
temp_is_spilled(Temp, TempMap) ->
  case hipe_epiphany:temp_is_allocatable(Temp) of
    true ->
      Reg = hipe_epiphany:temp_reg(Temp),
      tuple_size(TempMap) > Reg andalso hipe_temp_map:is_spilled(Reg, TempMap);
    false -> true
  end.

%%% Make a certain reg into a clone of Temp.

clone(Temp, RegOpt) ->
  Type = hipe_epiphany:temp_type(Temp),
  case RegOpt of
    [] -> hipe_epiphany:mk_new_temp(Type);
    Reg -> hipe_epiphany:mk_temp(Reg, Type)
  end.
