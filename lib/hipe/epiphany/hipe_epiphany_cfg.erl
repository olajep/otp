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

-module(hipe_epiphany_cfg).

-export([init/1,
         labels/1, start_label/1,
         succ/2,
         bb/2, bb_add/3]).
-export([postorder/1]).
-export([linearise/1]).
-export([params/1, reverse_postorder/1]).
-export([arity/1]). % for linear scan
%%-export([redirect_jmp/3]).

%%% these tell cfg.inc what to define (ugly as hell)
-define(BREADTH_ORDER,true).  % for linear scan
-define(PARAMS_NEEDED,true).
-define(START_LABEL_UPDATE_NEEDED,true).

-include("hipe_epiphany.hrl").
-include("../flow/cfg.hrl").
-include("../flow/cfg.inc").

init(Defun) ->
  Code = hipe_epiphany:defun_code(Defun),
  StartLab = hipe_epiphany:label_label(hd(Code)),
  Data = hipe_epiphany:defun_data(Defun),
  IsClosure = hipe_epiphany:defun_is_closure(Defun),
  Name = hipe_epiphany:defun_mfa(Defun),
  IsLeaf = hipe_epiphany:defun_is_leaf(Defun),
  Formals = hipe_epiphany:defun_formals(Defun),
  CFG0 = mk_empty_cfg(Name, StartLab, Data, IsClosure, IsLeaf, Formals),
  take_bbs(Code, CFG0).

is_branch(I) ->
  case I of
    #bcc{'cond'=Cond} -> 'always' = Cond, true;
    #pseudo_call{} -> true;
    %% #pseudo_switch{} -> true;
    #pseudo_tailcall{} -> true;
    _ -> false
  end.

branch_successors(Branch) ->
  case Branch of
    #bcc{'cond'='always',label=Label} -> [Label];
    #pseudo_call{contlab=ContLab, sdesc=#epiphany_sdesc{exnlab=ExnLab}} ->
      case ExnLab of
	[] -> [ContLab];
	_ -> [ContLab,ExnLab]
      end;
    %% #pseudo_switch{labels=Labels} -> Labels;
    #pseudo_tailcall{} -> []
  end.

-ifdef(REMOVE_TRIVIAL_BBS_NEEDED).
fails_to(_Instr) -> [].
-endif.

mk_goto(Label) ->
  hipe_epiphany:mk_bcc('always', Label).

is_label(I) ->
  hipe_epiphany:is_label(I).

label_name(Label) ->
  hipe_epiphany:label_label(Label).

mk_label(Name) ->
  hipe_epiphany:mk_label(Name).

linearise(CFG) ->	% -> defun, not insn list
  MFA = function(CFG),
  Formals = params(CFG),
  Code = linearize_cfg(CFG),
  Data = data(CFG),
  VarRange = hipe_gensym:var_range(epiphany),
  LabelRange = hipe_gensym:label_range(epiphany),
  IsClosure = is_closure(CFG),
  IsLeaf = is_leaf(CFG),
  hipe_epiphany:mk_defun(MFA, Formals, IsClosure, IsLeaf,
			 Code, Data, VarRange, LabelRange).

arity(CFG) ->
  {_M, _F, A} = function(CFG),
  A.
