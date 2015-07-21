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

-module(hipe_epiphany_specific).

%% for hipe_coalescing_regalloc:
-export([number_of_temporaries/1
	,analyze/1
	,labels/1
	,all_precoloured/0
	,bb/2
	,liveout/2
	,reg_nr/1
	,def_use/1
	,is_move/1
	,is_precoloured/1
	,var_range/1
	,allocatable/0
	,non_alloc/1
	,physical_name/1
	,reverse_postorder/1
	,livein/2
	,uses/1
	,defines/1
	]).

%% for hipe_graph_coloring_regalloc:
-export([is_fixed/1]).

%% for hipe_ls_regalloc:
-export([args/1, is_arg/1, is_global/1, new_spill_index/1]).
-export([breadthorder/1, postorder/1]).

%% callbacks for hipe_regalloc_loop
-export([defun_to_cfg/1,
	 check_and_rewrite/2]).

defun_to_cfg(Defun) ->
  hipe_epiphany_cfg:init(Defun).

check_and_rewrite(Defun, Coloring) ->
  hipe_epiphany_ra_postconditions:check_and_rewrite(Defun, Coloring, 'normal').

reverse_postorder(CFG) ->
  hipe_epiphany_cfg:reverse_postorder(CFG).

non_alloc(CFG) ->
  non_alloc(hipe_epiphany_registers:nr_args(), hipe_epiphany_cfg:params(CFG)).

%% same as hipe_epiphany_frame:fix_formals/2
non_alloc(0, Rest) -> Rest;
non_alloc(N, [_|Rest]) -> non_alloc(N-1, Rest);
non_alloc(_, []) -> [].

%% Liveness stuff

analyze(CFG) ->
  hipe_epiphany_liveness:analyse(CFG).

livein(Liveness,L) ->
  [X || X <- hipe_epiphany_liveness:livein(Liveness,L),
	hipe_epiphany:temp_is_allocatable(X)].

liveout(BB_in_out_liveness,Label) ->
  [X || X <- hipe_epiphany_liveness:liveout(BB_in_out_liveness,Label),
	hipe_epiphany:temp_is_allocatable(X)].

%% Registers stuff

allocatable() ->
  hipe_epiphany_registers:allocatable().

all_precoloured() ->
  hipe_epiphany_registers:all_precoloured().

is_precoloured(Reg) ->
  hipe_epiphany_registers:is_precoloured(Reg).

is_fixed(R) ->
  hipe_epiphany_registers:is_fixed(R).

physical_name(Reg) ->
  Reg.

%% CFG stuff

labels(CFG) ->
  hipe_epiphany_cfg:labels(CFG).

var_range(_CFG) ->
  hipe_gensym:var_range(epiphany).

number_of_temporaries(_CFG) ->
  Highest_temporary = hipe_gensym:get_var(epiphany),
  %% Since we can have temps from 0 to Max adjust by +1.
  Highest_temporary + 1.

bb(CFG,L) ->
  hipe_epiphany_cfg:bb(CFG,L).

%% Epiphany stuff

def_use(Instruction) ->
  {defines(Instruction), uses(Instruction)}.

uses(I) ->
  [X || X <- hipe_epiphany_defuse:insn_use(I),
	hipe_epiphany:temp_is_allocatable(X)].

defines(I) ->
  [X || X <- hipe_epiphany_defuse:insn_def(I),
	hipe_epiphany:temp_is_allocatable(X)].

is_move(Instruction) ->
  case hipe_epiphany:is_pseudo_move(Instruction) of
    true ->
      Dst = hipe_epiphany:pseudo_move_dst(Instruction),
      case hipe_epiphany:temp_is_allocatable(Dst) of
	false -> false;
	_ ->
	  Src = hipe_epiphany:pseudo_move_src(Instruction),
	  hipe_epiphany:temp_is_allocatable(Src)
      end;
    false -> false
      %% hipe_epiphany:is_movcc(Instruction) andalso
      %% 	hipe_epiphany:movcc_cond(Instruction) =:= 'always'
  end.

reg_nr(Reg) ->
  hipe_epiphany:temp_reg(Reg).

%%% Linear Scan stuff

new_spill_index(SpillIndex) when is_integer(SpillIndex) ->
  SpillIndex+1.

breadthorder(CFG) ->
  hipe_epiphany_cfg:breadthorder(CFG).

postorder(CFG) ->
  hipe_epiphany_cfg:postorder(CFG).

is_global(R) ->
  R =:= hipe_epiphany_registers:temp1() orelse
    R =:= hipe_epiphany_registers:temp2() orelse
    R =:= hipe_epiphany_registers:temp3() orelse
    hipe_epiphany_registers:is_fixed(R).

is_arg(R) ->
  hipe_epiphany_registers:is_arg(R).

args(CFG) ->
  hipe_epiphany_registers:args(hipe_epiphany_cfg:arity(CFG)).
