%%% -*- erlang-indent-level: 2 -*-
%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2004-2016. All Rights Reserved.
%%% 
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% 
%%% %CopyrightEnd%
%%%
%%% Common wrapper for graph_coloring and coalescing regallocs.

-module(hipe_regalloc_loop).
-export([ra/7, ra_fp/6]).

%%-define(HIPE_INSTRUMENT_COMPILER, true). %% Turn on instrumentation.
-include("../main/hipe.hrl").

ra(CFG, Liveness0, SpillIndex, Options, RegAllocMod, TargetMod, TargetCtx) ->
  {NewCFG, Liveness, Coloring, _NewSpillIndex} =
    ra_common(CFG, Liveness0, SpillIndex, Options, RegAllocMod, TargetMod,
	      TargetCtx),
  {NewCFG, Liveness, Coloring}.

ra_fp(CFG, Liveness, Options, RegAllocMod, TargetMod, TargetCtx) ->
  ra_common(CFG, Liveness, 0, Options, RegAllocMod, TargetMod, TargetCtx).

ra_common(CFG0, Liveness0, SpillIndex, Options, RegAllocMod, TargetMod,
	  TargetCtx) ->
  ?inc_counter(ra_calls_counter, 1),
  {CFG1, Liveness1, SpillGrouping} =
    do_range_split(CFG0, Liveness0, TargetMod, TargetCtx, Options),
  SpillLimit0 = TargetMod:number_of_temporaries(CFG1, TargetCtx),
  {Coloring, _, CFG, Liveness} =
    call_allocator_initial(CFG1, Liveness1, SpillLimit0, SpillIndex, Options,
			   RegAllocMod, TargetMod, TargetCtx),
  %% The first iteration, the hipe_regalloc_prepass may create new temps, these
  %% should not end up above SpillLimit.
  SpillLimit = TargetMod:number_of_temporaries(CFG, TargetCtx),
  alloc(Coloring, CFG, Liveness, SpillLimit, SpillIndex, SpillGrouping, Options,
	RegAllocMod, TargetMod, TargetCtx).

alloc(Coloring0, CFG0, Liveness, SpillLimit, SpillIndex, SpillGrouping, Options,
      RegAllocMod, TargetMod, TargetCtx) ->
  ?inc_counter(ra_iteration_counter, 1),
  Coloring = combine_spills(Coloring0, SpillGrouping),
  {CFG, DidSpill} = TargetMod:check_and_rewrite(CFG0, Coloring, TargetCtx),
  case DidSpill of
    false -> %% No new temps, we are done.
      ?add_spills(Options, _NewSpillIndex),
      TempMap = hipe_temp_map:cols2tuple(Coloring, TargetMod, TargetCtx),
      {TempMap2, NewSpillIndex2} =
	hipe_spillmin:stackalloc(CFG0, Liveness, [], SpillIndex, Options,
				 TargetMod, TargetCtx, TempMap),
      Coloring2 = 
	hipe_spillmin:mapmerge(hipe_temp_map:to_substlist(TempMap), TempMap2),
      %% case proplists:get_bool(verbose_spills, Options) of
      %%   true ->
      %%     ?msg("Num spill slots used: ~p~n", [NewSpillIndex2-SpillIndex]);
      %%   false ->
      %%     ok
      %% end,
      {CFG, Liveness, Coloring2, NewSpillIndex2};
    _ ->
      %% Since SpillLimit is used as a low-water-mark
      %% the list of temps not to spill is uninteresting.
      {NewColoring, _NewSpillIndex} =
	call_allocator(CFG, Liveness, SpillLimit, SpillIndex, Options,
		       RegAllocMod, TargetMod, TargetCtx),
      alloc(NewColoring, CFG, Liveness, SpillLimit, SpillIndex, SpillGrouping,
	    Options, RegAllocMod, TargetMod, TargetCtx)
  end.

call_allocator_initial(CFG, Liveness, SpillLimit, SpillIndex, Options,
		       RegAllocMod, TargetMod, TargetCtx) ->
  case proplists:get_bool(ra_prespill, Options) of
    true ->
      hipe_regalloc_prepass:regalloc_initial(
	RegAllocMod, CFG, Liveness, SpillIndex, SpillLimit, TargetMod,
	TargetCtx, Options);
    false ->
      {C, SI} = RegAllocMod:regalloc(CFG, Liveness, SpillIndex, SpillLimit,
				     TargetMod, TargetCtx, Options),
      {C, SI, CFG, Liveness}
  end.

call_allocator(CFG, Liveness, SpillLimit, SpillIndex, Options, RegAllocMod,
	       TargetMod, TargetCtx) ->
  case proplists:get_bool(ra_prespill, Options) of
    true ->
      hipe_regalloc_prepass:regalloc(
	RegAllocMod, CFG, Liveness, SpillIndex, SpillLimit, TargetMod,
	TargetCtx, Options);
    false ->
      RegAllocMod:regalloc(CFG, Liveness, SpillIndex, SpillLimit, TargetMod,
			   TargetCtx, Options)
  end.

do_range_split(CFG0, Liveness0, TgtMod, TgtCtx, Options) ->
  Grouping0 = hipe_spill_grouping:new(),
  {CFG2, Liveness1, Grouping1} =
    case proplists:get_bool(ra_restore_reuse, Options) of
      true ->
	{CFG1, GList0} = hipe_restore_reuse:split(CFG0, Liveness0, TgtMod,
						  TgtCtx),
	{CFG1, TgtMod:analyze(CFG1, TgtCtx),
	 hipe_spill_grouping:add(GList0, Grouping0)};
      false ->
	{CFG0, Liveness0, Grouping0}
    end,
  case proplists:get_bool(ra_range_split, Options) of
    true ->
      {CFG3, GList1} = hipe_range_split:split(CFG2, Liveness1, TgtMod, TgtCtx),
      {CFG3, TgtMod:analyze(CFG3, TgtCtx),
	 hipe_spill_grouping:add(GList1, Grouping1)};
    false ->
      {CFG2, Liveness1, Grouping1}
  end.

combine_spills(Alloc, Grouping) ->
  hipe_spill_grouping:combine_spills(Alloc, Grouping).
