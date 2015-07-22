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

-module(hipe_epiphany_ra_finalise).
-export([finalise/2]).
-include("hipe_epiphany.hrl").

finalise(Defun, TempMap) ->
  Code = hipe_epiphany:defun_code(Defun),
  {_, SpillLimit} = hipe_epiphany:defun_var_range(Defun),
  Map = mk_ra_map(TempMap, SpillLimit),
  NewCode = ra_code(Code, Map, []),
  Defun#defun{code=NewCode}.

ra_code([I|Insns], Map, Accum) ->
  ra_code(Insns, Map, [ra_insn(I, Map) | Accum]);
ra_code([], _Map, Accum) ->
  lists:reverse(Accum).

ra_insn(I, Map) ->
  case I of
    #alu{dst=Dst,src1=Src1,src2=Src2} ->
      I#alu{dst=opnd(Dst, Map),src1=opnd(Src1, Map),src2=opnd(Src2, Map)};
    #ldr{dst=Dst,base=Base,offset=Offset} ->
      I#ldr{dst=opnd(Dst, Map),base=opnd(Base, Map),offset=opnd(Offset, Map)};
    #mov  {dst=Dst} -> I#mov  {dst=opnd(Dst, Map)};
    #movt {dst=Dst} -> I#movt {dst=opnd(Dst, Map)};
    #movfs{dst=Dst} -> I#movfs{dst=opnd(Dst, Map)};
    #pseudo_call{funv=FunV} -> I#pseudo_call{funv=funv(FunV, Map)};
    #pseudo_move{dst=Dst,src=Src} ->
      I#pseudo_move{dst=opnd(Dst, Map),src=opnd(Src, Map)};
    #pseudo_switch{jtab=JTab,index=Index} ->
      I#pseudo_switch{jtab=opnd(JTab, Map),index=opnd(Index, Map)};
    #pseudo_tailcall{funv=FunV,stkargs=StkArgs} ->
      I#pseudo_tailcall{funv=funv(FunV, Map),stkargs=args(StkArgs, Map)};
    #str{src=Src,base=Base,offset=Offset} ->
      I#str{src=opnd(Src, Map),base=opnd(Base, Map),offset=opnd(Offset, Map)};
    %% Instructions that should only be introduced after RA
    #b{}     -> exit({?MODULE, ra_insn, I});
    #bl{}    -> exit({?MODULE, ra_insn, I});
    #jalr{}  -> exit({?MODULE, ra_insn, I});
    #jr{}    -> exit({?MODULE, ra_insn, I});
    #movcc{} -> exit({?MODULE, ra_insn, I});
    _ -> I
    end.

opnd(T=#epiphany_temp{}, Map) -> temp(T, Map);
opnd(Imm, _) -> Imm.

temp(Temp=#epiphany_temp{reg=Reg}, Map) ->
  case hipe_epiphany_registers:is_precoloured(Reg) of
    true -> Temp;
    false ->
      case gb_trees:lookup(Reg, Map) of
	{value,NewReg} -> Temp#epiphany_temp{reg=NewReg};
	none -> Temp
      end
  end.

funv(FunV, Map) -> opnd(FunV, Map).

args(Args, Map) -> [opnd(Arg, Map) || Arg <- Args].

mk_ra_map(TempMap, SpillLimit) ->
  %% Build a partial map from pseudo to reg or spill.
  %% Spills are represented as pseudos with indices above SpillLimit.
  %% The frame mapping proper is unchanged, since spills look just like
  %% ordinary (un-allocated) pseudos.
  lists:foldl(fun(MapLet, Map) ->
		  {Key,Val} = conv_ra_maplet(MapLet, SpillLimit),
		  gb_trees:insert(Key, Val, Map)
	      end,
	      gb_trees:empty(),
	      TempMap).

conv_ra_maplet(MapLet = {From,To}, SpillLimit) ->
  %% From should be a pseudo, or a hard reg mapped to itself.
  if is_integer(From), From =< SpillLimit ->
      case hipe_epiphany_registers:is_precoloured(From) of
	false -> [];
	_ ->
	  case To of
	    {reg, From} -> [];
	    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
	  end
      end;
     true -> exit({?MODULE,conv_ra_maplet,MapLet})
  end,
  case To of
    {reg, NewReg} ->
      %% NewReg should be a hard reg, or a pseudo mapped
      %% to itself (formals are handled this way).
      if is_integer(NewReg) ->
	  case hipe_epiphany_registers:is_precoloured(NewReg) of
	    true -> [];
	    _ -> if From =:= NewReg -> [];
		    true ->
		     exit({?MODULE,conv_ra_maplet,MapLet})
		 end
	  end;
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      {From, NewReg};
    {spill, SpillIndex} ->
      %% SpillIndex should be >= 0.
      if is_integer(SpillIndex), SpillIndex >= 0 -> [];
	 true -> exit({?MODULE,conv_ra_maplet,MapLet})
      end,
      ToTempNum = SpillLimit+SpillIndex+1,
      MaxTempNum = hipe_gensym:get_var(epiphany),
      if MaxTempNum >= ToTempNum -> ok;
	 true -> hipe_gensym:set_var(epiphany, ToTempNum)
      end,
      {From, ToTempNum};
    _ -> exit({?MODULE,conv_ra_maplet,MapLet})
  end.
