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

-module(hipe_epiphany_finalise).
-export([finalise/1]).
-include("hipe_epiphany.hrl").

finalise(Defun) ->
  #defun{code=Code0} = Defun,
  Code1 = peep(expand(Code0)),
  Defun#defun{code=Code1}.

expand(Insns) ->
  expand_list(Insns, []).

expand_list([I|Insns], Accum) ->
  expand_list(Insns, expand_insn(I, Accum));
expand_list([], Accum) ->
  lists:reverse(Accum).

expand_insn(I, Accum) ->
  case I of
    #pseudo_bcc{'cond'=Cond,true_label=TrueLab,false_label=FalseLab} ->
      [hipe_epiphany:mk_bcc('always', FalseLab),
       hipe_epiphany:mk_bcc(Cond,     TrueLab) |
       Accum];
    #pseudo_call{funv=FunV,sdesc=SDesc,contlab=ContLab,linkage=Linkage} ->
      [hipe_epiphany:mk_bcc('always', ContLab),
       case FunV of
	 #epiphany_temp{} -> hipe_epiphany:mk_jalr(FunV, SDesc);
	 _ -> hipe_epiphany:mk_bl(FunV, SDesc, Linkage)
       end |
       Accum];
    #pseudo_switch{jtab=JTab,index=Index=#epiphany_temp{}} ->
      %% We assume temp3 is free; we know temp2 and 1 might not be (used by
      %% ra_postconditions to load spills).
      %% XXX: Since temp3 isn't in the low register numbers, we are forcing all
      %% three instructions to be word-sized. Really, we'd like to expand the
      %% lsl /before/ RA. Indeed, if it was expanded before we even lower to
      %% epiphany, the computaion could be optimised away, potentially.
      Temp3 =
	case hipe_epiphany:mk_temp(hipe_epiphany_registers:temp3(), 'untagged') of
	  JTab -> exit({?MODULE, expand_insn, I});
	  Index -> exit({?MODULE, expand_insn, I});
	  Free -> Free
	end,
      [hipe_epiphany:mk_jr(Temp3),
       hipe_epiphany:mk_ldr('w', Temp3, JTab, '+', Temp3),
       hipe_epiphany:mk_alu('lsl', Temp3, Index, hipe_epiphany:mk_uimm5(2))
       | Accum];
    #pseudo_tailcall_prepare{} ->
      Accum;
    %% #rts{} -> ; %% We choose to expand rts during encode instead
    %% Should already have been expanded by hipe_epiphany_frame
    #pseudo_move{}     -> exit({?MODULE, expand_insn, I});
    #pseudo_tailcall{} -> exit({?MODULE, expand_insn, I});
    _ ->
      [I|Accum]
  end.

peep(Insns) ->
  peep_list(Insns, []).

peep_list([#bcc{label=Label} | (Insns = [#label{label=Label}|_])], Accum) ->
  peep_list(Insns, Accum);
peep_list([#movcc{dst=Dst,src=Dst} | Insns], Accum) ->
  peep_list(Insns, Accum);
peep_list([#str{size=Size,src=Src,base=Base,sign=Sign,offset=Offset}=StrI,
	   #ldr{size=Size,dst=Dst,base=Base,sign=Sign,offset=Offset} | Insns],
	  Accum) ->
  peep_list([StrI, #movcc{'cond'='always',dst=Dst,src=Src} | Insns], Accum);
peep_list([I|Insns], Accum) ->
  peep_list(Insns, [I|Accum]);
peep_list([], Accum) ->
  lists:reverse(Accum).
