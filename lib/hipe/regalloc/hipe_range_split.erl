%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%@doc
%%	       TEMPORARY LIVE RANGE SPLITTING PASS
%%
%% Live range splitting is useful to allow a register allocator to allocate a
%% temporary to register for a part of its lifetime, even if it cannot be for
%% the entirety. This improves register allocation quality, at the cost of
%% making the allocation problem more time and memory intensive to solve.
%%
%% Optimal allocation can be achieved if all temporaries are split at every
%% program point (between all instructions), but this makes register allocation
%% infeasably slow in practice. Instead, this module uses heuristics to choose
%% which temporaries should have their live ranges split, and at which points.
%%
%% Candidate heuristic: For every temp alive over a killall
%%
%% Moreover, since the spill slot allocator does not perform move coalescing,
%% this module returns a grouping of temporaries, that can later be used with
%% combine_spills/2 to make sure all the ranges of a temporary are allocated to
%% the same spill slot.
%% TODO: How do we make sure these spill slot move-to-selfs are omitted from the
%% machine code? Is it best to alter every check_and_rewrite/2, as well as
%%_frame or _ra_finalise? Can it be done in a simpler way?
-module(hipe_range_split).

-export([split/4, combine_spills/2]).
-export_type([spill_grouping/0]).

%% -compile(inline).
-define(USE_BB_WEIGHTS,1).

-define(DO_ASSERT, 1).
-include("../main/hipe.hrl").

-opaque spill_grouping() :: #{temp() => temp()}. % member => witness (like dset)
-type cfg()              :: any().
-type liveness()         :: any().
-type target_module()    :: module().
-type target_context()   :: any().
-type target()           :: {target_module(), target_context()}.
-type liveset()          :: ordsets:ordset(temp()).
-type temp()             :: non_neg_integer().
-type label()            :: non_neg_integer().

%% Tasks:
%%  * DSETS partitioning of program space at clobber points.
%%
%%  **? Collect partition edges (clobber points) with liveset and/or clobberset?
%%
%%  ** Count uses and definitions of each temp, grouped by partition. (Will
%%     possibly require some cleaverness to be efficient if done during the
%%     partitioning pass)
%%
%% * P({DEF}) lattice fwd dataflow (for eliding stores at SPILL splits)
%%
%% Q: Do we need a partition refinement data structure? No, it does not do quite
%% what we need. We want something akin to a graph, where we delete edges (by
%% inserting splits) to partition the space. Can 'sofs' do that?
%%
%% Idea: Second "spill mode," where temps are spilled at definitions rather than
%% exit edges. Heuristic decision based on similar "cost" as first spill mode.
%% Q: Can we do the same to restores? Is there any benefit?

-spec split(cfg(), liveness(), target_module(), target_context())
	   -> {cfg(), spill_grouping()}.
split(CFG0, Liveness, TargetMod, TargetContext) ->
  {M,F,A} = element(2, element(3, CFG0)),
  %% io:fwrite(standard_error, "Splitting ~w:~w/~w~n", [M,F,A]),
  TStart = erlang:monotonic_time(milli_seconds),

  Wts = compute_weights(CFG0, TargetMod, TargetContext),
  TWt = erlang:monotonic_time(milli_seconds),

  Target = {TargetMod, TargetContext},
  Defs = def_analyse(CFG0, Target),

  TDefs = erlang:monotonic_time(milli_seconds),

  %% io:fwrite(standard_error, "Defs: ~p~n",
  %% 	    [maps:map(fun(_,V)->maps:keys(V)end,Defs)]),
  {DUCounts, Edges, DSets0, Temps} = scan(CFG0, Liveness, Wts, Defs, Target),
  {DSets, _} = dsets_to_map(DSets0),

  TScan = erlang:monotonic_time(milli_seconds),
  put(renames, 0),
  put(introduced, 0.0),
  put(saved, 0.0),

  Renames = decide(DUCounts, Edges, Target),
  SpillGroup = mk_spillgroup(Renames),

  TDecide = erlang:monotonic_time(milli_seconds),
  %% io:fwrite(standard_error, "Renames ~p~n", [Renames]),

  CFG = rewrite(CFG0, Target, Liveness, Defs, DSets, Renames, Temps),

  TEnd = erlang:monotonic_time(milli_seconds),
  Time = TEnd - TStart,
  case (Time > 100) or ((RenC=erase(renames))>0) of false -> ok; true ->
      io:fwrite(standard_error,
		%% "Split~4w pts,~4w ren,~5w intro,~5w sav,~4w ms"
		"Split~4w,~4w,~6.2f,~6.2f,~4w ms"
		" (~3w+~3w+~3w+~3w+~3w)"
		", ~w:~w/~w~n",
		[erase(partitions),RenC,erase(introduced),erase(saved),Time,
		 %% Weight+ Defs+ Scan+ Decide+ Rewrite
		 TWt-TStart, TDefs-TWt, TScan-TDefs, TDecide-TScan, TEnd-TDecide,
		 M,F,A])

	%% ,io:fwrite(standard_error, "Wts: ~p~n", [Wts])
  end,
  %%error(notimpl).
  {CFG, SpillGroup}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Zeroth pass
%%
%% P({DEF}) lattice fwd dataflow (for eliding stores at SPILL splits)
-type defs() :: #{label() => defset() | {call, defset(), defset()}}.

-spec def_analyse(cfg(), target()) -> defs().
def_analyse(CFG, Target) ->
  Defs0 = def_init(CFG, Target),
  RPO = reverse_postorder(CFG, Target),
  def_dataf(RPO, CFG, Defs0).

-spec def_init(cfg(), target()) -> defs().
def_init(CFG, Target) ->
  Labels = labels(CFG, Target),
  maps:from_list(
    [begin
       Last = hipe_bb:last(BB=bb(CFG, L, Target)),
       {L, case defines_all_alloc(Last, Target) of
	     false -> def_init_scan(hipe_bb:code(BB), Target, defset_new());
	     true ->
	       {call, def_init_scan(hipe_bb:butlast(BB), Target, defset_new()),
		defset_from_list(reg_defines(Last, Target))}
	   end}
     end || L <- Labels]).

def_init_scan([], _Target, Defset) -> Defset;
def_init_scan([I|Is], Target, Defset0) ->
  ?ASSERT(not defines_all_alloc(I, Target)),
  Defset = defset_add_list(reg_defines(I, Target), Defset0),
  def_init_scan(Is, Target, Defset).

reg_defines(I, Target) ->
  reg_names(defines(I,Target), Target).

def_dataf(Labels, CFG, Defs0) ->
  case def_dataf_once(Labels, CFG, Defs0, 0) of
    {Defs, 0} -> Defs;
    {Defs, _Changed} ->
      def_dataf(Labels, CFG, Defs)
  end.

def_dataf_once([], _CFG, Defs, Changed) -> {Defs, Changed};
def_dataf_once([L|Ls], CFG, Defs0, Changed0) ->
  AddPreds =
    fun(Defset1) ->
	lists:foldl(fun(P, Defset2) ->
			defset_union(defout(P, Defs0), Defset2)
		    end, Defset1, hipe_gen_cfg:pred(CFG, L))
    end,
  Defset =
    case Defset0 = maps:get(L, Defs0) of
      {call, Butlast, Defout} -> {call, AddPreds(Butlast), Defout};
      _ -> AddPreds(Defset0)
    end,
  Changed = case Defset =:= Defset0 of
	      true  -> Changed0;
	      false -> Changed0+1
	    end,
  def_dataf_once(Ls, CFG, Defs0#{L := Defset}, Changed).

defout(L, Defs) ->
  case maps:get(L, Defs) of
    {call, _DefButLast, Defout} -> Defout;
    Defout -> Defout
  end.

defbutlast(L, Defs) ->
  case maps:get(L, Defs) of
    {call, DefButLast, _Defout} -> DefButLast
   ;_ -> error(badarg, [L, Defs])
  end.


%% Maps seem to be faster than ordsets for defset()
-type defset() :: #{temp() => []}.
defset_new() -> #{}.
defset_union(A, B) -> maps:merge(A, B).
defset_member(E, D) -> maps:is_key(E, D).

defset_add_list([],     D) -> D;
defset_add_list([E|Es], D) -> defset_add_list(Es, D#{E => []}).

defset_intersect_ordset([], _D) -> [];
defset_intersect_ordset([E|Es], D) ->
  case maps:is_key(E,D) of
    true -> [E|defset_intersect_ordset(Es,D)];
    false ->   defset_intersect_ordset(Es,D)
  end.

-ifdef(NOTDEF).
-type defset() :: ordsets:ordset(temp()).
defset_new() -> ordsets:new().
defset_union(A, B) -> ordsets:union(A, B).
defset_member(E, D) -> lists:member(E, D).
defset_add_list(L, F) -> defset_union(ordsets:from_list(L), F).
defset_intersect_ordset(O, D) -> ordsets:intersection(D, O).
-endif.

defset_from_list(L) -> defset_add_list(L, defset_new()).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% First pass
%%
%% Compute program space partitioning, collect information required by the
%% heuristic.
-type part_key() :: label().
-type part_dsets() :: dsets(part_key()).
%%-type part_dsets_map() :: #{part_key() => part_key()}.
-type ducounts() :: #{part_key() => ducount()}.

scan(CFG, Liveness, Weights, Defs, Target) ->
  Labels = labels(CFG, Target),
  DSets0 = dsets_new(Labels),
  Edges0 = edges_new(),
  {DUCounts0, Edges1, DSets1, Temps} =
    scan_bbs(Labels, CFG, Liveness, Weights, Defs, Target, #{}, Edges0, DSets0,
	     #{}),
  {RLList, DSets2} = dsets_to_rllist(DSets1),
  put(partitions, length(RLList)),
  %% io:fwrite(standard_error, "Partitioning: ~p~n", [RLList]),
  {Edges, DSets} = edges_map_roots(DSets2, Edges1),
  DUCounts = collect_ducounts(RLList, DUCounts0, #{}),
  {DUCounts, Edges, DSets, Temps}.

collect_ducounts([], _, Acc) -> Acc;
collect_ducounts([{R,Ls}|RLs], DUCounts, Acc) ->
  DUCount = lists:foldl(fun(Key, FAcc) ->
			    ducount_merge(maps:get(Key, DUCounts, ducount_new()), FAcc)
			end, ducount_new(), Ls),
  collect_ducounts(RLs, DUCounts, Acc#{R => DUCount}).

scan_bbs([], _CFG, _Liveness, _Weights, _Defs, _Target, DUCounts, Edges, DSets,
	 Temps) ->
  {DUCounts, Edges, DSets, Temps};
scan_bbs([L|Ls], CFG, Liveness, Weights, Defs, Target, DUCounts0, Edges0, DSets0,
	 Temps0) ->
  Code = hipe_bb:code(BB = bb(CFG, L, Target)),
  Wt = weight(L, Weights),
  Temps = collect_temps(Code, Target, Temps0),
  LastI = hipe_bb:last(BB),
  {DSets, Edges, EntryCode} =
    case defines_all_alloc(LastI, Target) of
      false ->
	DSets1 = lists:foldl(fun(S, DS) -> dsets_union(L, S, DS) end,
			     DSets0, hipe_gen_cfg:succ(CFG, L)),
	{DSets1, Edges0, Code};
      true ->
	LiveBefore = liveness_step(LastI, Target, liveout(Liveness, L, Target)),
	%% We can omit the spill of a temp that has not been defined since the
	%% last time it was spilled
	SpillSet = defset_intersect_ordset(LiveBefore, defbutlast(L, Defs)),
	Edges1 = edges_insert(L, L, Wt, SpillSet, Edges0),
	Edges3 = lists:foldl(fun({S, BranchWt}, Edges2) ->
				 SLivein = livein(Liveness, S, Target),
				 SWt = weight_scaled(L, BranchWt, Weights),
				 edges_insert(S, L, SWt, SLivein, Edges2)
			     end, Edges1, branch_preds(LastI, Target)),
	{DSets0, Edges3, hipe_bb:butlast(BB)}
    end,
  DUCount = scan_bb(EntryCode, Target, Wt, ducount_new()),
  DUCounts = DUCounts0#{L => DUCount},
  scan_bbs(Ls, CFG, Liveness, Weights, Defs, Target, DUCounts, Edges, DSets, Temps).

collect_temps([], _Target, Temps) -> Temps;
collect_temps([I|Is], Target, Temps0) ->
  {TDef, TUse} = def_use(I, Target),
  Fun = fun(Temp, Temps1) ->
	    Temps1#{reg_nr(Temp, Target) => Temp}
	end,
  Temps = lists:foldl(Fun, lists:foldl(Fun, Temps0, TDef), TUse),
  collect_temps(Is, Target, Temps).

%% Scans the code forwards, collecting def/use counts
scan_bb([], _Target, _Wt, DUCount) -> DUCount;
scan_bb([I|Is], Target, Wt, DUCount0) ->
  {Def, Use} = reg_def_use(I, Target),
  DUCount = ducount_add(Use, Wt, ducount_add(Def, Wt, DUCount0)),
  scan_bb(Is, Target, Wt, DUCount).

liveness_step(I, Target, Liveout) ->
  {Def, Use} = reg_def_use(I, Target),
  ordsets:union(Use, ordsets:subtract(Liveout, Def)).

reg_def_use(I, Target) ->
  {TDef, TUse} = def_use(I, Target),
  {reg_names(TDef,Target), reg_names(TUse,Target)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
weight(L, Weights) -> weight_scaled(L, 1.0, Weights).
-ifndef(USE_BB_WEIGHTS).
compute_weights(_, _, _) -> [].
weight_scaled(_L, _Scale, _Weights) -> 1.0.

-else.
compute_weights(CFG, TargetMod, TargetContext) ->
  %% try
    hipe_bb_weights:compute(CFG, TargetMod, TargetContext)
  %% catch error:E ->
  %%     io:fwrite(standard_error, "BB weighting failed: ~p:~n~p~n",
  %% 		[E, erlang:get_stacktrace()]),
  %%     []
  %% end
    .

%% weight_scaled(_L, _Scale, []) -> 1.0;
weight_scaled(L, Scale, Weights) ->
  Wt0 = hipe_bb_weights:weight(L, Weights) * Scale,
  %% true = Wt > 0.0000000000000000001,
  Wt = erlang:min(erlang:max(Wt0, 0.0000000000000000001), 10000.0),
  %% math:sqrt(Wt).
  math:pow(Wt, math:log(4*1.1*1.1)/math:log(100)).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Second pass (not really pass :/)
%%
%% Decide which temps to split, in which parts, and pick new names for them.
-type ren() :: #{temp() => temp()}.
-type renames() :: #{label() => ren()}.

-spec decide(ducounts(), edges(), target()) -> renames().
decide(DUCounts, Edges, Target) ->
  %% io:fwrite(standard_error, "Deciding~n", []),
  decide_parts(maps:to_list(DUCounts), Edges, Target, #{}).

decide_parts([], _Edges, _Target, Acc) -> Acc;
decide_parts([{Part,DUCount}|Ps], Edges, Target, Acc) ->
  Spills = decide_temps(ducount_to_list(DUCount), Part, Edges, Target, #{}),
  decide_parts(Ps, Edges, Target, Acc#{Part => Spills}).

decide_temps([], _Part, _Edges, _Target, Acc) -> Acc;
decide_temps([{Temp, SpillGain}|Ts], Part, Edges, Target, Acc0) ->
  Es = edges_query(Temp, Part, Edges),
  SpillCost = lists:sum(Es),
  Acc =
    case not is_precoloured(Temp, Target)
      andalso 1.1*SpillCost < SpillGain
      %% Es = [] usually means the temp is local to the partition; hence no need
      %% to split it
      andalso Es =/= []
    of
      false -> Acc0;
      true ->
	%% io:fwrite(standard_error, "Splitting range of temp ~w in part ~w, "
	%% 	  "SpillGain: ~w, Edges: ~w~n",
	%% 	  [Temp, Part, SpillGain, Edges]),
	put(renames, get(renames)+1),
	put(introduced, get(introduced)+SpillCost),
	put(saved, get(saved)+SpillGain),
	Acc0#{Temp => new_reg_nr(Target)}
  end,
  decide_temps(Ts, Part, Edges, Target, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Actual second pass
rewrite(CFG, Target, Liveness, Defs, DSets, Renames, Temps) ->
  rewrite_bbs(labels(CFG, Target), Target, Liveness, Defs, DSets, Renames,
	      Temps, CFG).

rewrite_bbs([], _Target, _Liveness, _Defs, _DSets, _Renames, _Temps, CFG) ->
  CFG;
rewrite_bbs([L|Ls], Target, Liveness, Defs, DSets, Renames, Temps, CFG0) ->
  Code0Rev = lists:reverse(Code0=hipe_bb:code(BB = bb(CFG0, L, Target))),
  EntryRen = maps:get(maps:get(L,DSets), Renames),
  {Code, CFG} =
    case defines_all_alloc(hd(Code0Rev), Target) of
      false ->
	Fun = rewrite_subst_fun(Target, EntryRen),
	Code1 = lists:map(fun(I) -> subst_temps(Fun, I, Target) end, Code0),
	{Code1, CFG0};
      true ->
	CallI0 = hd(Code0Rev),
	Succ = hipe_gen_cfg:succ(CFG0, L),
	{CallI, CFG1} = inject_restores(Succ, Target, Liveness, DSets, Renames,
					Temps, CallI0, CFG0),
	Liveout1 = liveness_step(CallI, Target, liveout(Liveness, L, Target)),
	Defout = defbutlast(L, Defs),
	SpillMap = mk_spillmap(EntryRen, Liveout1, Defout, Temps, Target),
	Fun = rewrite_subst_fun(Target, EntryRen),
	Code1Rev = lists:map(fun(I) -> subst_temps(Fun, I, Target) end,
			     tl(Code0Rev)),
	Code2 = lift_spills(Code1Rev, Target, SpillMap, [CallI]),
	{Code2, CFG1}
    end,
  rewrite_bbs(Ls, Target, Liveness, Defs, DSets, Renames, Temps,
	      update_bb(CFG, L, hipe_bb:code_update(BB, Code), Target)).

rewrite_subst_fun(Target, Ren) ->
  fun(Temp) ->
      Reg = reg_nr(Temp, Target),
      case Ren of
	#{Reg := NewName} -> update_reg_nr(NewName, Temp, Target);
	#{} -> Temp
      end
  end.

mk_spillmap(Ren, Livein, Defout, Temps, Target) ->
  [begin
     Temp = maps:get(Reg, Temps),
     {NewName, mk_move(update_reg_nr(NewName, Temp, Target), Temp, Target)}
   end || {Reg, NewName} <- maps:to_list(Ren), lists:member(Reg, Livein),
	  defset_member(Reg, Defout)].

mk_restores(Ren, Liveout, Temps, Target) ->
  [begin
     Temp = maps:get(Reg, Temps),
     mk_move(Temp, update_reg_nr(NewName, Temp, Target), Target)
   end || {Reg, NewName} <- maps:to_list(Ren), lists:member(Reg, Liveout)].

inject_restores([], _Target, _Liveness, _DSets, _Renames, _Temps, CFI, CFG) ->
  {CFI, CFG};
inject_restores([L|Ls], Target, Liveness, DSets, Renames, Temps, CFI0, CFG0) ->
  Ren = maps:get(maps:get(L,DSets), Renames),
  Livein = livein(Liveness, L, Target),
  {CFI, CFG} =
    case mk_restores(Ren, Livein, Temps, Target) of
      [] -> {CFI0, CFG0}; % optimisation
      Restores ->
	RestBBLbl = new_label(Target),
	Code = Restores ++ [mk_goto(L, Target)],
	%% io:fwrite(standard_error, "Injecting restore block ~w before ~w~n",
	%% 	    [RestBBLbl, L]),
	%% io:fwrite(standard_error, "~p~n", [Code]),
	CFI1 = redirect_jmp(CFI0, L, RestBBLbl, Target),
	CFG1 = update_bb(CFG0, RestBBLbl, hipe_bb:mk_bb(Code), Target),
	{CFI1, CFG1}
    end,
  inject_restores(Ls, Target, Liveness, DSets, Renames, Temps, CFI, CFG).

%% Heuristic. Move spills up until we meet the edge of the BB or a definition of
%% that temp.
lift_spills([], _Target, SpillMap, Acc) ->
  [SpillI || {_, SpillI} <- SpillMap] ++ Acc;
lift_spills([I|Is], Target, SpillMap0, Acc) ->
  Def = reg_defines(I, Target),
  {Spills0, SpillMap} =
    lists:partition(fun({Reg,_}) -> lists:member(Reg, Def) end, SpillMap0),
  Spills = [SpillI || {_, SpillI} <- Spills0],
  lift_spills(Is, Target, SpillMap, [I|Spills ++ Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec mk_spillgroup(renames()) -> spill_grouping().
mk_spillgroup(Renames) ->
  maps:fold(fun(_, Ren, Acc0) ->
		maps:fold(fun(Orig, New, Acc1) ->
			      Acc1#{Orig => Orig, New => Orig}
			  end, Acc0, Ren)
	    end, #{}, Renames).

-spec combine_spills(hipe_map(), spill_grouping()) -> hipe_map().
combine_spills(Alloc, Grouping) ->
  combine_spills_1(Alloc, Grouping, #{}).

combine_spills_1([], _Grouping, _Subst) -> [];
combine_spills_1([{_, {RegTag, _}}=A|As], Grouping, Subst)
  when RegTag =:= 'reg'; RegTag =:= 'fp_reg' ->
  [A|combine_spills_1(As, Grouping, Subst)];
combine_spills_1([{Temp, {spill, Slot0}}=A0|As], Grouping, Subst0) ->
  {A, Subst} =
    case Grouping of
      #{Temp := Witness} ->
	case Subst0 of
	  #{Witness := Slot} -> {{Temp, {spill, Slot}}, Subst0};
	  #{} -> {A0, Subst0#{Witness => Slot0}}
	end;
      #{} -> {A0, Subst0}
    end,
  [A|combine_spills_1(As, Grouping, Subst)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Edges ADT
%%
%% Keeps track of the edges between partitions and the sets of temps live at
%% that edge.

%% The liveset type needs to provide logarithmic membership query; thus it
%% cannot be the usual sorted list.
-type edge_liveset() :: #{temp() => []}. % set
-type edges() :: #{part_key() => [{part_key(), float(), edge_liveset()}]}.

-spec edges_new() -> edges().
edges_new() -> #{}.

-spec edges_insert(part_key(), part_key(), float(), liveset(), edges())
		  -> edges().
edges_insert(A, B, Weight, OLiveset, Edges) ->
  Liveset = maps:from_list([{T, []} || T <- OLiveset]),
  map_append(A, {B, Weight, Liveset}, Edges).

-spec edges_map_roots(part_dsets(), edges()) -> {edges(), part_dsets()}.
edges_map_roots(DSets0, Edges) ->
  %% Can just as well be mapfoldl, but I think mapfoldr is more efficient
  {NewEs, DSets} = lists:mapfoldr(fun edges_map_roots_1/2, DSets0,
				  maps:to_list(Edges)),
  {maps_from_list_merge(NewEs, fun erlang:'++'/2, #{}), DSets}.

maps_from_list_merge([], _MF, Acc) -> Acc;
maps_from_list_merge([{K,V}|Ps], MF, Acc) ->
  maps_from_list_merge(Ps, MF, case Acc of
				 #{K := OV} -> Acc#{K := MF(V, OV)};
				 #{}        -> Acc#{K => V}
			       end).

edges_map_roots_1({A, Es}, DSets0) ->
  {AR, DSets1} = dsets_find(A, DSets0),
  %% dito about mapfoldl
  {EsR, DSets} = lists:mapfoldr(fun({B, Wt, Live}, DSets2) ->
				    {BR, DSets3} = dsets_find(B, DSets2),
				    {{BR, Wt, Live}, DSets3}
				end, DSets1, Es),
  {{AR, EsR}, DSets}.

-spec edges_query(temp(), part_key(), edges()) -> [part_key()].
edges_query(Temp, Part, Edges) ->
  Es = maps:get(Part, Edges, []),
  [Wt || {_B, Wt, Live} <- Es, maps:is_key(Temp, Live)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% The disjoint set forests data structure, for elements of arbitrary types.
%% Note that the find operation mutates the set.
-type dsets(X) :: #{X => {node, X} | {root, non_neg_integer()}}.

-spec dsets_new([E]) -> dsets(E).
dsets_new(Elems) -> maps:from_list([{E,{root,0}} || E <- Elems]).

-spec dsets_find(E, dsets(E)) -> {E, dsets(E)}.
dsets_find(E, DS0) ->
  case DS0 of
    #{E := {root,_}} -> {E, DS0};
    #{E := {node,N}} ->
      case dsets_find(N, DS0) of
	{N, _}=T -> T;
	{R, DS1} -> {R, DS1#{E := {node,R}}}
      end
   ;_ -> error(badarg, [E, DS0])
  end.

-spec dsets_union(E, E, dsets(E)) -> dsets(E).
dsets_union(X, Y, DS0) ->
  {XRoot, DS1} = dsets_find(X, DS0),
  case dsets_find(Y, DS1) of
    {XRoot, DS2} -> DS2;
    {YRoot, DS2} ->
      #{XRoot := {root,XRR}, YRoot := {root,YRR}} = DS2,
      if XRR < YRR -> DS2#{XRoot := {node,YRoot}};
	 XRR > YRR -> DS2#{YRoot := {node,XRoot}};
	 true -> DS2#{YRoot := {node,XRoot}, XRoot := {root,XRR+1}}
      end
  end.

-spec dsets_to_map(dsets(E)) -> {#{Elem::E => Root::E}, dsets(E)}.
dsets_to_map(DS) ->
  dsets_to_map(maps:keys(DS), DS, #{}).

dsets_to_map([], DS, Acc) -> {Acc, DS};
dsets_to_map([K|Ks], DS0, Acc) ->
  {KR, DS} = dsets_find(K, DS0),
  dsets_to_map(Ks, DS, Acc#{K => KR}).

-spec dsets_to_rllist(dsets(E)) -> {[{Root::E, Elems::[E]}], dsets(E)}.
dsets_to_rllist(DS0) ->
  {Lists, DS} = dsets_to_rllist(maps:keys(DS0), #{}, DS0),
  {maps:to_list(Lists), DS}.

dsets_to_rllist([], Acc, DS) -> {Acc, DS};
dsets_to_rllist([E|Es], Acc, DS0) ->
  {ERoot, DS} = dsets_find(E, DS0),
  dsets_to_rllist(Es, map_append(ERoot, E, Acc), DS).

map_append(Key, Elem, Map) ->
  case Map of
    #{Key := List} -> Map#{Key := [Elem|List]};
    #{} -> Map#{Key => [Elem]}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Def and use counting ADT
-type ducount() :: #{temp() => float()}.

-spec ducount_new() -> ducount().
ducount_new() -> #{}.

-spec ducount_add([temp()], float(), ducount()) -> ducount().
ducount_add([], _Weight, DUCount) -> DUCount;
ducount_add([T|Ts], Weight, DUCount0) ->
  DUCount =
    case DUCount0 of
      #{T := Count} -> DUCount0#{T := Count + Weight};
      #{}           -> DUCount0#{T => Weight}
    end,
  ducount_add(Ts, Weight, DUCount).

ducount_to_list(DUCount) -> maps:to_list(DUCount).

-spec ducount_merge(ducount(), ducount()) -> ducount().
ducount_merge(DCA, DCB) when map_size(DCA) < map_size(DCB) ->
  ducount_merge_1(ducount_to_list(DCA), DCB);
ducount_merge(DCA, DCB) when map_size(DCA) >= map_size(DCB) ->
  ducount_merge_1(ducount_to_list(DCB), DCA).

ducount_merge_1([], DUCount) -> DUCount;
ducount_merge_1([{T,AC}|Ts], DUCount0) ->
  DUCount =
    case DUCount0 of
      #{T := BC} -> DUCount0#{T := AC + BC};
      #{}        -> DUCount0#{T => AC}
    end,
  ducount_merge_1(Ts, DUCount).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Target module interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TGT_IFACE_0(N), N(         {M,C}) -> M:N(         C)).
-define(TGT_IFACE_1(N), N(A1,      {M,C}) -> M:N(A1,      C)).
-define(TGT_IFACE_2(N), N(A1,A2,   {M,C}) -> M:N(A1,A2,   C)).
-define(TGT_IFACE_3(N), N(A1,A2,A3,{M,C}) -> M:N(A1,A2,A3,C)).

?TGT_IFACE_2(bb).
?TGT_IFACE_1(def_use).
?TGT_IFACE_1(defines).
?TGT_IFACE_1(defines_all_alloc).
?TGT_IFACE_1(is_precoloured).
?TGT_IFACE_1(labels).
?TGT_IFACE_1(mk_goto).
?TGT_IFACE_2(mk_move).
?TGT_IFACE_0(new_label).
?TGT_IFACE_0(new_reg_nr).
?TGT_IFACE_3(redirect_jmp).
?TGT_IFACE_1(reg_nr).
?TGT_IFACE_1(reverse_postorder).
?TGT_IFACE_2(subst_temps).
?TGT_IFACE_3(update_bb).
?TGT_IFACE_2(update_reg_nr).

branch_preds(Instr, {TgtMod,TgtCtx}) ->
  merge_sorted_preds(lists:keysort(1, TgtMod:branch_preds(Instr, TgtCtx))).

livein(Liveness, L, Target={TgtMod,TgtCtx}) ->
  ordsets:from_list(reg_names(TgtMod:livein(Liveness, L, TgtCtx), Target)).

liveout(Liveness, L, Target={TgtMod,TgtCtx}) ->
  ordsets:from_list(reg_names(TgtMod:liveout(Liveness, L, TgtCtx), Target)).

merge_sorted_preds([]) -> [];
merge_sorted_preds([{L, P1}, {L, P2}|LPs]) ->
  merge_sorted_preds([{L, P1+P2}|LPs]);
merge_sorted_preds([LP|LPs]) -> [LP|merge_sorted_preds(LPs)].

reg_names(Regs, {TgtMod,TgtCtx}) ->
  [TgtMod:reg_nr(X,TgtCtx) || X <- Regs].
