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

-define(DO_ASSERT, 1).
-include("../main/hipe.hrl").

-opaque spill_grouping() :: [].
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
%%    Partitions: {entry, Lb} | {exit, Lb}
%%    Internal parts are not included in DSETS (they will never be unioned with
%%    any other part, and they are not known in the beginning. dsets_find/2
%%    could be extended to understand them and return them as their own
%%    witnesses if need be.
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

-spec split(cfg(), liveness(), target_module(), target_context())
	   -> {cfg(), spill_grouping()}.
split(CFG0, Liveness, TargetMod, TargetContext) ->
  {M,F,A} = element(2, element(3, CFG0)),
  %% io:fwrite(standard_error, "Splitting ~w:~w/~w~n", [M,F,A]),
  TStart = erlang:monotonic_time(milli_seconds),

  Target = {TargetMod, TargetContext},
  Defs = def_analyse(CFG0, Target),
  %% io:fwrite(standard_error, "Defs: ~p~n",
  %% 	    [maps:map(fun(_,V)->maps:keys(V)end,Defs)]),
  {DUCounts, Edges, DSets0, Temps} = scan(CFG0, Liveness, Defs, Target),
  {DSets, _} = dsets_to_map(DSets0),

  put(renames, 0),
  put(introduced, 0),
  put(saved, 0),

  Renames = decide(DUCounts, Edges, Target),
  %% io:fwrite(standard_error, "Renames ~p~n", [Renames]),
  CFG = rewrite(CFG0, Target, Liveness, Defs, DSets, Renames, Temps),

  Time = erlang:monotonic_time(milli_seconds) - TStart,
  case (Time > 100) or ((RenC=erase(renames))>0) of false -> ok; true ->
      io:fwrite(standard_error,
		"Split~5w ren,~5w intro,~5w sav,~5w ms, ~w:~w/~w~n",
		[RenC,erase(introduced),erase(saved),Time,M,F,A])
  end,
  %%error(notimpl).
  {CFG, []}.

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
			defset_union(
			  case maps:get(P, Defs0) of
			    {call, _PButlast, PDefset} -> PDefset;
			    PDefset -> PDefset
			  end, Defset2)
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

%% defout(L, Defs) ->
%%   case maps:get(L, Defs) of
%%     {call, _DefButLast, Defout} -> Defout;
%%     Defout -> Defout
%%   end.

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
defset_intersect_ordset([E|Es],D) ->
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
-type part_key() :: {entry | exit, label()}.
-type part_dsets() :: dsets(part_key()).
%%-type part_dsets_map() :: #{part_key() => part_key()}.
%%-type ducounts() :: #{part_key() => ducount()}.

scan(CFG, Liveness, Defs, Target) ->
  Labels = labels(CFG, Target),
  DSets0 = initial_dsets(CFG, Labels),
  Edges0 = edges_new(),
  {DUCounts0, Edges1, DSets1, Temps} =
    scan_bbs(Labels, CFG, Liveness, Defs, Target, #{}, Edges0, DSets0, #{}),
  {RLList, DSets2} = dsets_to_rllist(DSets1),
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

initial_dsets(CFG, Labels) ->
  DSets0 = dsets_new([{Half, L} || L <- Labels, Half <- [entry,exit]]),
  Edges = lists:append([[{L, S} || S <- hipe_gen_cfg:succ(CFG, L)]
			|| L <- Labels]),
  lists:foldl(fun({X, Y}, DS) -> dsets_union({exit,X}, {entry,Y}, DS) end,
	      DSets0, Edges).

scan_bbs([], _CFG, _Liveness, _Defs, _Target, DUCounts, Edges, DSets, Temps) ->
  {DUCounts, Edges, DSets, Temps};
scan_bbs([L|Ls], CFG, Liveness, Defs, Target, DUCounts0, Edges0, DSets0,
	 Temps0) ->
  Code = hipe_bb:code(BB = bb(CFG, L, Target)),
  Temps = collect_temps(Code, Target, Temps0),
  LastI = hipe_bb:last(BB),
  {DSets, Edges, EntryCode} =
    case defines_all_alloc(LastI, Target) of
      false ->
	{dsets_union({entry,L}, {exit,L}, DSets0), Edges0, Code};
      true ->
	Liveout = liveout(Liveness, L, Target),
	LiveBefore = liveness_step(LastI, Target, Liveout),
	%% We can omit the spill of a temp that has not been defined since the
	%% last time it was spilled
	SpillSet = defset_intersect_ordset(LiveBefore, defbutlast(L, Defs)),
	Edges1 = edges_insert({entry,L}, {exit,L}, SpillSet,
			      edges_insert({exit,L}, {entry,L}, Liveout,
					   Edges0)),
	{DSets0, Edges1, hipe_bb:butlast(BB)}
    end,
  DUCount = scan_bb(EntryCode, Target, ducount_new()),
  DUCounts = DUCounts0#{{entry,L} => DUCount},
  scan_bbs(Ls, CFG, Liveness, Defs, Target, DUCounts, Edges, DSets, Temps).

collect_temps([], _Target, Temps) -> Temps;
collect_temps([I|Is], Target, Temps0) ->
  {TDef, TUse} = def_use(I, Target),
  Fun = fun(Temp, Temps1) ->
	    Temps1#{reg_nr(Temp, Target) => Temp}
	end,
  Temps = lists:foldl(Fun, lists:foldl(Fun, Temps0, TDef), TUse),
  collect_temps(Is, Target, Temps).

%% Scans the code forwards, collecting def/use counts
scan_bb([], _Target, DUCount) -> DUCount;
scan_bb([I|Is], Target, DUCount0) ->
  {Def, Use} = reg_def_use(I, Target),
  DUCount = ducount_add(Use, ducount_add(Def, DUCount0)),
  scan_bb(Is, Target, DUCount).

liveness_step(I, Target, Liveout) ->
  {Def, Use} = reg_def_use(I, Target),
  ordsets:union(Use, ordsets:subtract(Liveout, Def)).

reg_def_use(I, Target) ->
  {TDef, TUse} = def_use(I, Target),
  {reg_names(TDef,Target), reg_names(TUse,Target)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Second pass (not really pass :/)
%%
%% Decide which temps to split, in which parts, and pick new names for them.

decide(DUCounts, Edges, Target) ->
  %% io:fwrite(standard_error, "Deciding~n", []),
  decide_parts(maps:to_list(DUCounts), Edges, Target, #{}).

decide_parts([], _Edges, _Target, Acc) -> Acc;
decide_parts([{Part,DUCount}|Ps], Edges, Target, Acc) ->
  Spills = decide_temps(ducount_to_list(DUCount), Part, Edges, Target, #{}),
  decide_parts(Ps, Edges, Target, Acc#{Part => Spills}).

decide_temps([], _Part, _Edges, _Target, Acc) -> Acc;
decide_temps([{Temp, Count}|Ts], Part, Edges, Target, Acc0) ->
  Es = edges_query(Temp, Part, Edges),
  Acc =
    case not is_precoloured(Temp, Target)
      andalso length(Es) =< Count % Probably < is better; at least when we have
				  % BB weighting
      andalso Es =/= [] % Es = [] usually means the temp is local to the
			% partition; hence no need to split it
    of
      false -> Acc0;
      true ->
	%% io:fwrite(standard_error, "Splitting range of temp ~w in part ~w, "
	%% 	  "Count: ~w, Edges: ~w~n",
	%% 	  [Temp, Part, Count, Es]),
	put(renames, get(renames)+1),
	put(introduced, get(introduced)+length(Es)),
	put(saved, get(saved)+Count),
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
  Liveout0 = liveout(Liveness, L, Target),
  EntryRen = maps:get(maps:get({entry,L},DSets), Renames),
  {Code, CFG} =
    case defines_all_alloc(hd(Code0Rev), Target) of
      false ->
	Fun = rewrite_subst_fun(Target, EntryRen),
	Code1 = lists:map(fun(I) -> subst_temps(Fun, I, Target) end, Code0),
	{Code1, CFG0};
      true ->
	ExitRen = maps:get(maps:get({exit,L},DSets), Renames),
	CallI0 = hd(Code0Rev),
	Restores = mk_restores(ExitRen, Liveout0, Temps, Target),
	Succ = hipe_gen_cfg:succ(CFG0, L),
	{CallI, CFG1} = inject_restores(Succ, Restores, Target, CallI0, CFG0),
	Liveout1 = liveness_step(CallI, Target, Liveout0),
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

inject_restores(_Succs, [], _Target, CFI, CFG) -> {CFI, CFG}; % optimisation
inject_restores([], _Restores, _Target, CFI, CFG) -> {CFI, CFG};
inject_restores([L|Ls], Restores, Target, CFI0, CFG0) ->
  RestBBLbl = new_label(Target),
  Code = Restores ++ [mk_goto(L, Target)],
  %% io:fwrite(standard_error, "Injecting restore block ~w before ~w~n",
  %% 	    [RestBBLbl, L]),
  %% io:fwrite(standard_error, "~p~n", [Code]),
  CFI = redirect_jmp(CFI0, L, RestBBLbl, Target),
  CFG = update_bb(CFG0, RestBBLbl, hipe_bb:mk_bb(Code), Target),
  inject_restores(Ls, Restores, Target, CFI, CFG).

%% Heuristic. Move spills up until we meet the edge of the BB or a definition or
%% use of that temp.
lift_spills([], _Target, SpillMap, Acc) ->
  [SpillI || {_, SpillI} <- SpillMap] ++ Acc;
lift_spills([I|Is], Target, SpillMap0, Acc) ->
  {Def, Use} = reg_def_use(I, Target),
  DefUse = Def ++ Use,
  {Spills0, SpillMap} =
    lists:partition(fun({Reg,_}) -> lists:member(Reg, DefUse) end, SpillMap0),
  Spills = [SpillI || {_, SpillI} <- Spills0],
  lift_spills(Is, Target, SpillMap, [I|Spills ++ Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec combine_spills(hipe_map(), spill_grouping()) -> hipe_map().
combine_spills(_Alloc, _Grouping) ->
  %%error(notimpl).
  _Alloc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Edges ADT
%%
%% Keeps track of the edges between partitions and the sets of temps live at
%% that edge.

%% The liveset type needs to provide logarithmic membership query; thus it
%% cannot be the usual sorted list.
-type edge_liveset() :: #{temp() => []}. % set
-type edges() :: #{part_key() => [{part_key(), edge_liveset()}]}.

-spec edges_new() -> edges().
edges_new() -> #{}.

-spec edges_insert(part_key(), part_key(), liveset(), edges()) -> edges().
edges_insert(A, B, OLiveset, Edges) ->
  Liveset = maps:from_list([{T, []} || T <- OLiveset]),
  map_append(A, {B, Liveset}, Edges).

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
  {EsR, DSets} = lists:mapfoldr(fun({B, Live}, DSets2) ->
				    {BR, DSets3} = dsets_find(B, DSets2),
				    {{BR, Live}, DSets3}
				end, DSets1, Es),
  {{AR, EsR}, DSets}.

-spec edges_query(temp(), part_key(), edges()) -> [part_key()].
edges_query(Temp, Part, Edges) ->
  Es = maps:get(Part, Edges, []), % All parts have edges
  [B || {B, Live} <- Es, maps:is_key(Temp, Live)].

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
-type ducount() :: #{temp() => non_neg_integer()}.

-spec ducount_new() -> ducount().
ducount_new() -> #{}.

-spec ducount_add([temp()], ducount()) -> ducount().
ducount_add([], DUCount) -> DUCount;
ducount_add([T|Ts], DUCount0) ->
  DUCount =
    case DUCount0 of
      #{T := Count} -> DUCount0#{T := Count + 1};
      #{}           -> DUCount0#{T => 1}
    end,
  ducount_add(Ts, DUCount).

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
%% bb(CFG, L, {TgtMod,TgtCtx}) ->
%%   TgtMod:bb(CFG, L, TgtCtx).

%% def(I, {TgtMod,TgtCtx}) ->
%%   TgtMod:def(I, TgtCtx).

%% def_use(I, {TgtMod,TgtCtx}) ->
%%   TgtMod:def_use(I, TgtCtx).

%% defines_all_alloc(I, {TgtMod,TgtCtx}) ->
%%   TgtMod:defines_all_alloc(I, TgtCtx).

%% is_precoloured(Reg, {TgtMod,TgtCtx}) ->
%%   TgtMod:is_precoloured(Reg, TgtCtx).

%% labels(CFG, {TgtMod,TgtCtx}) ->
%%   TgtMod:labels(CFG, TgtCtx).

liveout(Liveness, L, Target={TgtMod,TgtCtx}) ->
  ordsets:from_list(reg_names(TgtMod:liveout(Liveness, L, TgtCtx), Target)).

?TGT_IFACE_1(mk_goto).
?TGT_IFACE_2(mk_move).
?TGT_IFACE_0(new_label).
?TGT_IFACE_0(new_reg_nr).
?TGT_IFACE_3(redirect_jmp).

%% mk_goto(Label, {TgtMod,TgtCtx}) ->
%%   TgtMod:mk_goto(Label, TgtCtx).

%% mk_move(Src, Dst, {TgtMod,TgtCtx}) ->
%%   TgtMod:mk_move(Src, Dst, TgtCtx).

%% new_label({TgtMod,TgtCtx}) ->
%%   TgtMod:new_label(TgtCtx).

%% new_reg_nr({TgtMod,TgtCtx}) ->
%%   TgtMod:new_reg_nr(TgtCtx).

%% redirect_jmp(JmpInsn, ToOld, ToNew, {TgtMod,TgtCtx}) ->
%%   TgtMod:redirect_jmp(JmpInsn, ToOld, ToNew, TgtCtx).

reg_names(Regs, {TgtMod,TgtCtx}) ->
  [TgtMod:reg_nr(X,TgtCtx) || X <- Regs].

?TGT_IFACE_1(reg_nr).
?TGT_IFACE_1(reverse_postorder).
?TGT_IFACE_2(subst_temps).
?TGT_IFACE_3(update_bb).
?TGT_IFACE_2(update_reg_nr).

%% reg_nr(Temp, {TgtMod,TgtCtx}) ->
%%   TgtMod:reg_nr(Temp, TgtCtx).

%% reverse_postorder(CFG, {TgtMod,TgtCtx}) ->
%%   TgtMod:reverse_postorder(CFG, TgtCtx).

%% subst_temps(SubstFun, Instr, {TgtMod,TgtCtx}) ->
%%   TgtMod:subst_temps(SubstFun, Instr, TgtCtx).

%% update_bb(CFG, L, BB, {TgtMod,TgtCtx}) ->
%%   TgtMod:update_bb(CFG, L, BB, TgtCtx).

%% update_reg_nr(Nr, Temp, {TgtMod,TgtCtx}) ->
%%   TgtMod:update_reg_nr(Nr, Temp, TgtCtx).
