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

-module(hipe_epiphany_defuse).
-export([insn_def/1, insn_use/1]).
-include("hipe_epiphany.hrl").

%%%
%%% insn_def(Insn) -- Return set of temps defined by an instruction.
%%%

insn_def(I) ->
  case I of
    #alu{dst=Dst} -> [Dst];
    #ldr{dst=Dst} -> [Dst];
    #mov{dst=Dst} -> [Dst];
    #movcc{dst=Dst} -> [Dst];
    #movt{dst=Dst} -> [Dst];
    #movfs{dst=Dst} -> [Dst];
    #pseudo_call{} -> call_clobbered();
    #pseudo_move{dst=Dst} -> [Dst];
    #pseudo_tailcall_prepare{} -> tailcall_clobbered();
    %% Instructions introduced after RA (not allowed)
    #b{}    -> exit({?MODULE, insn_def, I});
    #bl{}   -> exit({?MODULE, insn_def, I});
    #jalr{} -> exit({?MODULE, insn_def, I});
    #jr{}   -> exit({?MODULE, insn_def, I});
    _ -> []
  end.

call_clobbered() ->
  [hipe_epiphany:mk_temp(R, T)
   || {R,T} <- hipe_epiphany_registers:call_clobbered()].

tailcall_clobbered() ->
  [hipe_epiphany:mk_temp(R, T)
   || {R,T} <- hipe_epiphany_registers:tailcall_clobbered()].

%%%
%%% insn_use(Insn) -- Return set of temps used by an instruction.
%%%

insn_use(I) ->
  case I of
    #alu{src1=Src1,src2=Src2} -> operand_use(Src2, [Src1]);
    #ldr{base=Base,offset=Offset} -> operand_use(Offset, [Base]);
    #movcc{'cond'='always',src=Src} -> [Src];
    #movcc{dst=Dst,src=Src} -> addtemp(Dst, [Src]);
    #movt{dst=Dst} -> [Dst]; %% sic!
    #pseudo_call{funv=FunV,sdesc=#epiphany_sdesc{arity=Arity}} ->
      funv_use(FunV, arity_use(Arity));
    #pseudo_move{src=Src} -> [Src];
    #pseudo_switch{jtab=JTabR,index=IndexR} ->
      addtemps([temp3(), JTabR], [IndexR]);
    #pseudo_tailcall{funv=FunV,arity=Arity,stkargs=StkArgs} ->
      addargs(StkArgs, addtemps(tailcall_clobbered(),
				funv_use(FunV, arity_use(Arity))));
    #rts{} ->
      [hipe_epiphany:mk_temp(hipe_epiphany_registers:lr(), 'untagged') |
       rets()];
    #str{src=Src,base=Base,offset=Offset} ->
      operand_use(Offset, addtemp(Base, [Src]));
    %% Instructions introduced after RA (not allowed)
    #b{}    -> exit({?MODULE, insn_use, I});
    #bl{}   -> exit({?MODULE, insn_use, I});
    #jalr{} -> exit({?MODULE, insn_use, I});
    #jr{}   -> exit({?MODULE, insn_use, I});
    _ -> []
  end.

addargs([Arg|Args], Set) ->
  addargs(Args, operand_use(Arg, Set));
addargs([], Set) ->
  Set.

arity_use(Arity) ->
  [hipe_epiphany:mk_temp(R, 'tagged')
   || R <- hipe_epiphany_registers:args(Arity)].

funv_use(FunV, Set) ->
  case FunV of
    #epiphany_temp{} -> addtemp(FunV, Set);
    _ -> Set
  end.

operand_use(Opd, Set) ->
  case Opd of
    #epiphany_temp{} -> addtemp(Opd, Set);
    _Immediate -> Set
  end.

rets() ->
  [hipe_epiphany:mk_temp(hipe_epiphany_registers:ret(N), 'tagged')
   || N <- lists:seq(0, hipe_epiphany_registers:nr_rets() - 1)].

temp3() ->
  hipe_epiphany:mk_temp(hipe_epiphany_registers:temp3(), 'untagged').

%%%
%%% Auxiliary operations on sets of temps
%%%

addtemps([Arg|Args], Set) ->
  addtemps(Args, addtemp(Arg, Set));
addtemps([], Set) ->
  Set.

addtemp(Temp, Set) ->
  case lists:member(Temp, Set) of
    false -> [Temp|Set];
    _ -> Set
  end.
