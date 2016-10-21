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

%% Heuristic tuning constants
-define(GAIN_FACTOR_THRESH, 1.1).
-define(MODE2_PREFERENCE, 1.1).
-define(WEIGHT_FUN(Wt), math:pow(Wt, math:log(4*1.1*1.1)/math:log(100))).

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
%%
%%   Idea: Backward P({DEF}) analysis can elide redundant mode2 spills stores.
%%
%%   Idea: Custom liveness analysis, killing liveness at call instructions can
%%   be used to elide restores in second spill mode
%%
%% Edge case not handled: Call instructions directly defining a pseudo. In that
%% case, if that pseudo has been selected for mode2 spills, no spill is inserted
%% after the call.

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
  RDefs = rdef_analyse(CFG0, Target),
  PLive = plive_analyse(CFG0, Target),

  TDefs = erlang:monotonic_time(milli_seconds),

  %% io:fwrite(standard_error, "Defs: ~p~n",
  %% 	    [maps:map(fun(_,V)->maps:keys(V)end,Defs)]),
  {DUCounts, Costs, DSets0, Temps} =
    scan(CFG0, Liveness, PLive, Wts, Defs, RDefs, Target),
  {DSets, _} = dsets_to_map(DSets0),

  TScan = erlang:monotonic_time(milli_seconds),
  put(renames, 0),
  put(introduced, 0.0),
  put(saved, 0.0),

  Renames = decide(DUCounts, Costs, Target),
  SpillGroup = mk_spillgroup(Renames),

  TDecide = erlang:monotonic_time(milli_seconds),
  %% io:fwrite(standard_error, "Renames ~p~n", [Renames]),

  CFG = rewrite(CFG0, Target, Liveness, PLive, Defs, RDefs, DSets, Renames,
		Temps),

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
-type defs_inp() :: #{label() =>
			defset_inp() | {call, defset_inp(), defset_inp()}}.
-type defs()     :: #{label() => defset()}.

-spec def_analyse(cfg(), target()) -> defs().
def_analyse(CFG, Target) ->
  Defs0 = def_init(CFG, Target),
  RPO = reverse_postorder(CFG, Target),
  def_dataf(RPO, CFG, Defs0).

-spec def_init(cfg(), target()) -> defs_inp().
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
    {Defs, 0} ->
      def_finalise(Defs);
    {Defs, _Changed} ->
      def_dataf(Labels, CFG, Defs)
  end.

def_finalise(Defs) ->
  maps:from_list([{K, defset_finalise(BL)}
		  || {K, {call, BL, _}} <- maps:to_list(Defs)]).

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

-spec defout(label(), defs_inp()) -> defset_inp().
defout(L, Defs) ->
  case maps:get(L, Defs) of
    {call, _DefButLast, Defout} -> Defout;
    Defout -> Defout
  end.

-spec defbutlast(label(), defs()) -> defset().
defbutlast(L, Defs) -> maps:get(L, Defs).

-type defset_inp() :: bitord().
defset_new() -> bitord_new().
defset_union(A, B) -> bitord_union(A, B).
defset_add_list(L, D) -> defset_union(defset_from_list(L), D).
defset_from_list(L) -> bitord_from_ordset(ordsets:from_list(L)).
defset_finalise(D) -> bitarr_from_bitord(D).

-type defset() :: bitarr().
defset_member(E, D) -> bitarr_get(E, D).

defset_intersect_ordset([], _D) -> [];
defset_intersect_ordset([E|Es], D) ->
  case bitarr_get(E, D) of
    true  -> [E|defset_intersect_ordset(Es,D)];
    false ->    defset_intersect_ordset(Es,D)
  end.

-ifdef(NOTDEF).
%% Maps seem to be faster than ordsets for defset()
-type defset() :: #{temp() => []}.
defset_new() -> #{}.
defset_union(A, B) -> maps:merge(A, B).
defset_member(E, D) -> maps:is_key(E, D).

defset_add_list([],     D) -> D;
defset_add_list([E|Es], D) -> defset_add_list(Es, D#{E => []}).

defset_intersect_ordset([], _D) -> [];
defset_intersect_ordset([E|Es], D) ->
  case D of
    #{E := _} -> [E|defset_intersect_ordset(Es,D)];
    #{}       ->    defset_intersect_ordset(Es,D)
  end.

defset_from_list(L) -> defset_add_list(L, defset_new()).
-endif.

-ifdef(NOTDEF).
-type defset() :: ordsets:ordset(temp()).
defset_new() -> ordsets:new().
defset_union(A, B) -> ordsets:union(A, B).
defset_member(E, D) -> lists:member(E, D).
defset_add_list(L, F) -> defset_union(defset_from_list(L), F).
defset_intersect_ordset(O, D) -> ordsets:intersection(D, O).
defset_from_list(L) -> ordsets:from_list(L).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% P({DEF}) lattice reverse dataflow (for eliding stores at defines in mode2)
-type rdefs() :: #{label() =>
		     {call, rdefset(), [label()]}
		   | {nocall, rdefset(), rdefset(), [label()]}}.

-spec rdef_analyse(cfg(), target()) -> rdefs().
rdef_analyse(CFG, Target) ->
  Defs0 = rdef_init(CFG, Target),
  PO = postorder(CFG, Target),
  rdef_dataf(PO, Defs0).

-spec rdef_init(cfg(), target()) -> rdefs().
rdef_init(CFG, Target) ->
  Labels = labels(CFG, Target),
  maps:from_list(
    [begin
       Last = hipe_bb:last(BB=bb(CFG, L, Target)),
       Succs = hipe_gen_cfg:succ(CFG, L),
       {L, case defines_all_alloc(Last, Target) of
	     true ->
	       Defin = rdef_init_scan(hipe_bb:butlast(BB), Target, rdefset_empty()),
	       {call, Defin, Succs};
	     false ->
	       Gen = rdef_init_scan(hipe_bb:code(BB), Target, rdefset_empty()),
	       {nocall, Gen, rdefset_top(), Succs}
	   end}
     end || L <- Labels]).

rdef_init_scan([], _Target, Defset) -> Defset;
rdef_init_scan([I|Is], Target, Defset) ->
  rdef_init_scan(Is, Target, rdef_step(I, Target, Defset)).

rdef_step(I, Target, Defset) ->
  ?ASSERT(not defines_all_alloc(I, Target)),
  rdefset_add_list(reg_defines(I, Target), Defset).

rdef_dataf(Labels, Defs0) ->
  case rdef_dataf_once(Labels, Defs0, 0) of
    {Defs, 0} -> Defs;
    {Defs, _Changed} ->
      rdef_dataf(Labels, Defs)
  end.

rdef_dataf_once([], Defs, Changed) -> {Defs, Changed};
rdef_dataf_once([L|Ls], Defs0, Changed0) ->
  Defset =
    case Defset0 = maps:get(L, Defs0) of
      {call, Defin, Succs} ->
	%% XXX: This is right, right?
	{call, Defin, Succs};
	%% {call, rdefout_intersect(L, Defs0, Defin), Succs};
      {nocall, Gen, Defin0, Succs} ->
	Defin = rdefset_union(Gen, rdefout_intersect(L, Defs0, Defin0)),
	{nocall, Gen, Defin, Succs}
    end,
  Changed = case Defset =:= Defset0 of
	      true  -> Changed0;
	      false -> Changed0+1
	    end,
  rdef_dataf_once(Ls, Defs0#{L := Defset}, Changed).

-spec rdefin(label(), rdefs()) -> rdefset().
rdefin(L, Defs) -> rdefin_val(maps:get(L, Defs)).
rdefin_val({nocall, _Gen, Defin, _Succs}) -> Defin;
rdefin_val({call, Defin, _Succs}) -> Defin.

rsuccs(L, Defs) -> rsuccs_val(maps:get(L, Defs)).
rsuccs_val({nocall, _Gen, _Defin, Succs}) -> Succs;
rsuccs_val({call, _Defin, Succs}) -> Succs.

%% -spec rdefbutlast(label(), rdefs()) -> rdefset().
%% rdefbutlast(L, Defs) -> error(notimpl).

rdefout(L, Defs) ->
  rdefout_intersect(L, Defs, rdefset_top()).

rdefout_intersect(L, Defs, Init) ->
  lists:foldl(fun(S, Acc) ->
		  rdefset_intersect(rdefin(S, Defs), Acc)
	      end, Init, rsuccs(L, Defs)).

-type rdefset() :: bitord() | top.
rdefset_top() -> top.
rdefset_empty() -> bitord_new().
rdefset_from_list(L) -> bitord_from_ordset(ordsets:from_list(L)).

rdefset_add_list(_, top) -> top; % Should never happen in rdef_dataf
rdefset_add_list(L, D) -> rdefset_union(rdefset_from_list(L), D).

rdefset_union(top, _) -> top;
rdefset_union(_, top) -> top;
rdefset_union(A, B) -> bitord_union(A, B).

rdefset_intersect(top, D) -> D;
rdefset_intersect(D, top) -> D;
rdefset_intersect(A, B) -> bitord_intersect(A, B).

rdefset_member(_, top) -> true;
rdefset_member(E, D) -> bitord_get(E, D).

ordset_subtract_rdefset(_, top) -> [];
ordset_subtract_rdefset(OS, D) ->
  %% Lazy implementation; could do better if OS can grow
  lists:filter(fun(E) -> not rdefset_member(E, D) end, OS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Integer sets represented as bit sets
%%
%% Two representations; bitord() and bitarr()
-define(LIMB_IX_BITS,    11).
-define(LIMB_BITS,       (1 bsl ?LIMB_IX_BITS)).
-define(LIMB_IX(Index),  (Index bsr ?LIMB_IX_BITS)).
-define(BIT_IX(Index),   (Index band (?LIMB_BITS - 1))).
-define(BIT_MASK(Index), (1 bsl ?BIT_IX(Index))).

%% bitord(): fast at union/2 and can be compared for equality with '=:='
-type bitord() :: orddict:orddict(non_neg_integer(), 0..((1 bsl ?LIMB_BITS)-1)).

-spec bitord_new() -> bitord().
bitord_new() -> [].

-spec bitord_get(non_neg_integer(), bitord()) -> boolean().
bitord_get(Index, Bitord) ->
  case orddict:find(?LIMB_IX(Index), Bitord) of
    error -> false;
    {ok, Limb}  ->
      0 =/= (Limb band ?BIT_MASK(Index))
  end.

-spec bitord_union(bitord(), bitord()) -> bitord().
bitord_union(Lhs, Rhs) ->
  orddict:merge(fun(_, L, R) -> L bor R end, Lhs, Rhs).

-spec bitord_intersect(bitord(), bitord()) -> bitord().
bitord_intersect([], _) -> [];
bitord_intersect(_, []) -> [];
bitord_intersect([{K, L}|Ls], [{K, R}|Rs]) ->
  [{K, L band R} | bitord_intersect(Ls, Rs)];
bitord_intersect([{LK, _}|Ls], [{RK, _}|_]=Rs) when LK < RK ->
  bitord_intersect(Ls, Rs);
bitord_intersect([{LK, _}|_]=Ls, [{RK, _}|Rs]) when LK > RK ->
  bitord_intersect(Ls, Rs).

-spec bitord_from_ordset(ordsets:ordset(non_neg_integer())) -> bitord().
bitord_from_ordset([]) -> [];
bitord_from_ordset([B|Bs]) ->
  bitord_from_ordset_1(Bs, ?LIMB_IX(B), ?BIT_MASK(B)).

bitord_from_ordset_1([B|Bs], Key, Val) when Key =:= ?LIMB_IX(B) ->
  bitord_from_ordset_1(Bs, Key, Val bor ?BIT_MASK(B));
bitord_from_ordset_1([B|Bs], Key, Val) ->
  [{Key,Val} | bitord_from_ordset_1(Bs, ?LIMB_IX(B), ?BIT_MASK(B))];
bitord_from_ordset_1([], Key, Val) -> [{Key, Val}].

%% bitarr(): fast (enough) at get/2
-type bitarr() :: array:array(0..((1 bsl ?LIMB_BITS)-1)).

-spec bitarr_get(non_neg_integer(), bitarr()) -> boolean().
bitarr_get(Index, Array) ->
  Limb = array:get(?LIMB_IX(Index), Array),
  0 =/= (Limb band ?BIT_MASK(Index)).

-spec bitarr_from_bitord(bitord()) -> bitarr().
bitarr_from_bitord(Ord) ->
  array:from_orddict(Ord, 0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Partition-local liveness analysis
%%
%% As temps are not spilled when exiting a partition in mode2, only
%% partition-local uses need to be considered when deciding which temps need
%% restoring at partition entry.

-type plive() :: #{label() =>
		     {call, liveset(), [label()]}
		   | {nocall, {liveset(), liveset()}, liveset(), [label()]}}.

-spec plive_analyse(cfg(), target()) -> plive().
plive_analyse(CFG, Target) ->
  Defs0 = plive_init(CFG, Target),
  PO = postorder(CFG, Target),
  plive_dataf(PO, Defs0).

-spec plive_init(cfg(), target()) -> plive().
plive_init(CFG, Target) ->
  Labels = labels(CFG, Target),
  maps:from_list(
    [begin
       Last = hipe_bb:last(BB=bb(CFG, L, Target)),
       Succs = hipe_gen_cfg:succ(CFG, L),
       {L, case defines_all_alloc(Last, Target) of
	     true ->
	       %% XXX: It should be code and not butlast, right?
	       %% Otherwise we'll forget call uses, right?
	       {Gen, _} = plive_init_scan(hipe_bb:code(BB), Target),
	       {call, Gen, Succs};
	     false ->
	       GenKill = plive_init_scan(hipe_bb:code(BB), Target),
	       {nocall, GenKill, liveset_empty(), Succs}
	   end}
     end || L <- Labels]).

plive_init_scan([], _Target) -> {liveset_empty(), liveset_empty()};
plive_init_scan([I|Is], Target) ->
  {Gen0, Kill0} = plive_init_scan(Is, Target),
  {InstrKillLst, InstrGenLst} = reg_def_use(I, Target),
  InstrGen = liveset_from_list(InstrGenLst),
  InstrKill = liveset_from_list(InstrKillLst),
  Gen1 = liveset_subtract(Gen0, InstrKill),
  Gen = liveset_union(Gen1, InstrGen),
  Kill1 = liveset_union(Kill0, InstrKill),
  Kill = liveset_subtract(Kill1, InstrGen),
  {Gen, Kill}.

%% plive_step(I, Target, Liveset) ->
%%   ?ASSERT(not defines_all_alloc(I, Target)),
%%   liveness_step(I, Target, Liveset).

plive_dataf(Labels, PLive0) ->
  case plive_dataf_once(Labels, PLive0, 0) of
    {PLive, 0} -> PLive;
    {PLive, _Changed} ->
      plive_dataf(Labels, PLive)
  end.

plive_dataf_once([], PLive, Changed) -> {PLive, Changed};
plive_dataf_once([L|Ls], PLive0, Changed0) ->
  Liveset =
    case Liveset0 = maps:get(L, PLive0) of
      {call, Livein, Succs} ->
	%% XXX: This is right, right?
	{call, Livein, Succs};
	%% {call, rdefout_intersect(L, PLive0, Defin), Succs};
      {nocall, {Gen, Kill} = GenKill, _OldLivein, Succs} ->
	Liveout = pliveout(L, PLive0),
	Livein = liveset_union(Gen, liveset_subtract(Liveout, Kill)),
	{nocall, GenKill, Livein, Succs}
    end,
  Changed = case Liveset =:= Liveset0 of
	      true  -> Changed0;
	      false -> Changed0+1
	    end,
  plive_dataf_once(Ls, PLive0#{L := Liveset}, Changed).

pliveout(L, PLive) ->
  liveset_union([plivein(S, PLive) || S <- psuccs(L, PLive)]).

psuccs(L, PLive) -> psuccs_val(maps:get(L, PLive)).
psuccs_val({call, _Livein, Succs}) -> Succs;
psuccs_val({nocall, _GenKill, _Livein, Succs}) -> Succs.

plivein(L, PLive) -> plivein_val(maps:get(L, PLive)).
plivein_val({call, Livein, _Succs}) -> Livein;
plivein_val({nocall, _GenKill, Livein, _Succs}) ->  Livein.

liveset_empty() -> ordsets:new().
liveset_subtract(A, B) -> ordsets:subtract(A, B).
liveset_union(A, B) -> ordsets:union(A, B).
liveset_union(LivesetList) -> ordsets:union(LivesetList).
liveset_from_list(L) -> ordsets:from_list(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% First pass
%%
%% Compute program space partitioning, collect information required by the
%% heuristic.
-type part_key() :: label().
-type part_dsets() :: dsets(part_key()).
%%-type part_dsets_map() :: #{part_key() => part_key()}.
-type ducounts() :: #{part_key() => ducount()}.

scan(CFG, Liveness, PLive, Weights, Defs, RDefs, Target) ->
  Labels = labels(CFG, Target),
  DSets0 = dsets_new(Labels),
  Costs0 = costs_new(),
  {DUCounts0, Costs1, DSets1, Temps} =
    scan_bbs(Labels, CFG, Liveness, PLive, Weights, Defs, RDefs, Target, #{},
	     Costs0, DSets0, #{}),
  {RLList, DSets2} = dsets_to_rllist(DSets1),
  put(partitions, length(RLList)),
  %% io:fwrite(standard_error, "Partitioning: ~p~n", [RLList]),
  {Costs, DSets} = costs_map_roots(DSets2, Costs1),
  DUCounts = collect_ducounts(RLList, DUCounts0, #{}),
  {DUCounts, Costs, DSets, Temps}.

collect_ducounts([], _, Acc) -> Acc;
collect_ducounts([{R,Ls}|RLs], DUCounts, Acc) ->
  DUCount = lists:foldl(
	      fun(Key, FAcc) ->
		  ducount_merge(maps:get(Key, DUCounts, ducount_new()), FAcc)
	      end, ducount_new(), Ls),
  collect_ducounts(RLs, DUCounts, Acc#{R => DUCount}).

scan_bbs([], _CFG, _Liveness, _PLive, _Weights, _Defs, _RDefs, _Target,
	 DUCounts, Costs, DSets, Temps) ->
  {DUCounts, Costs, DSets, Temps};
scan_bbs([L|Ls], CFG, Liveness, PLive, Weights, Defs, RDefs, Target, DUCounts0,
	 Costs0, DSets0, Temps0) ->
  Code = hipe_bb:code(BB = bb(CFG, L, Target)),
  Wt = weight(L, Weights),
  Temps = collect_temps(Code, Target, Temps0),
  LastI = hipe_bb:last(BB),
  {DSets, Costs5, EntryCode, RDefout, Liveout} =
    case defines_all_alloc(LastI, Target) of
      false ->
	DSets1 = lists:foldl(fun(S, DS) -> dsets_union(L, S, DS) end,
			     DSets0, hipe_gen_cfg:succ(CFG, L)),
	{DSets1, Costs0, Code, rdefout(L, RDefs), liveout(Liveness, L, Target)};
      true ->
	LiveBefore = liveness_step(LastI, Target, liveout(Liveness, L, Target)),
	%% We can omit the spill of a temp that has not been defined since the
	%% last time it was spilled
	SpillSet = defset_intersect_ordset(LiveBefore, defbutlast(L, Defs)),
	Costs1 = costs_insert(exit, L, Wt, SpillSet, Costs0),
	Costs4 = lists:foldl(fun({S, BranchWt}, Costs2) ->
				 SLivein = livein(Liveness, S, Target),
				 SPLivein = plivein(S, PLive),
				 SWt = weight_scaled(L, BranchWt, Weights),
				 Costs3 = costs_insert(entry1, S, SWt, SLivein, Costs2),
				 costs_insert(entry2, S, SWt, SPLivein, Costs3)
			     end, Costs1, branch_preds(LastI, Target)),
	{DSets0, Costs4, hipe_bb:butlast(BB), rdefset_empty(), LiveBefore}
    end,
  {DUCount, Mode2Spills} =
    scan_bb(lists:reverse(EntryCode), Target, Wt, RDefout, Liveout,
	    ducount_new(), []),
  DUCounts = DUCounts0#{L => DUCount},
  Costs = costs_insert(spill, L, Wt, Mode2Spills, Costs5),
  scan_bbs(Ls, CFG, Liveness, PLive, Weights, Defs, RDefs, Target, DUCounts,
	   Costs, DSets, Temps).

collect_temps([], _Target, Temps) -> Temps;
collect_temps([I|Is], Target, Temps0) ->
  {TDef, TUse} = def_use(I, Target),
  Fun = fun(Temp, Temps1) ->
	    Temps1#{reg_nr(Temp, Target) => Temp}
	end,
  Temps = lists:foldl(Fun, lists:foldl(Fun, Temps0, TDef), TUse),
  collect_temps(Is, Target, Temps).

%% Scans the code backwards, collecting def/use counts and mode2 spills
scan_bb([], _Target, _Wt, _RDefout, _Liveout, DUCount, Spills) ->
  {DUCount, Spills};
scan_bb([I|Is], Target, Wt, RDefout, Liveout, DUCount0, Spills0) ->
  {Def, Use} = reg_def_use(I, Target),
  DUCount = ducount_add(Use, Wt, ducount_add(Def, Wt, DUCount0)),
  Livein = liveness_step(I, Target, Liveout),
  RDefin = rdef_step(I, Target, RDefout),
  %% The temps that would be spilled after I in mode 2
  NewSpills = ordset_subtract_rdefset(
		ordsets:intersection(ordsets:from_list(Def), Liveout),
		RDefout),
  Spills = ordsets:union(NewSpills, Spills0),
  scan_bb(Is, Target, Wt, RDefin, Livein, DUCount, Spills).

liveness_step(I, Target, Liveout) ->
  {Def, Use} = reg_def_use(I, Target),
  ordsets:union(ordsets:from_list(Use),
		ordsets:subtract(Liveout, ordsets:from_list(Def))).

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
  ?WEIGHT_FUN(Wt).
-endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Second pass (not really pass :/)
%%
%% Decide which temps to split, in which parts, and pick new names for them.
-type spill_mode() :: mode1 % Spill temps at partition exits
		    | mode2.% Spill temps at definitions
-type ren() :: #{temp() => {spill_mode(), temp()}}.
-type renames() :: #{label() => ren()}.

-spec decide(ducounts(), costs(), target()) -> renames().
decide(DUCounts, Costs, Target) ->
  decide_parts(maps:to_list(DUCounts), Costs, Target, #{}).

decide_parts([], _Costs, _Target, Acc) -> Acc;
decide_parts([{Part,DUCount}|Ps], Costs, Target, Acc) ->
  Spills = decide_temps(ducount_to_list(DUCount), Part, Costs, Target, #{}),
  decide_parts(Ps, Costs, Target, Acc#{Part => Spills}).

decide_temps([], _Part, _Costs, _Target, Acc) -> Acc;
decide_temps([{Temp, SpillGain}|Ts], Part, Costs, Target, Acc0) ->
  SpillCost1 = costs_query(Temp, entry1, Part, Costs)
    + costs_query(Temp, exit, Part, Costs),
  SpillCost2 = costs_query(Temp, entry2, Part, Costs)
    + costs_query(Temp, spill, Part, Costs),
  Acc =
    %% SpillCost1 =:= 0.0 usually means the temp is local to the partition;
    %% hence no need to split it
    case SpillCost1 =/= 0.0
      andalso not is_precoloured(Temp, Target)
      andalso (?GAIN_FACTOR_THRESH*SpillCost1 < SpillGain
	       orelse ?GAIN_FACTOR_THRESH*SpillCost2 < SpillGain)
    of
      false -> Acc0;
      true ->
	{Mode, SpillCost} =
	  case ?MODE2_PREFERENCE*SpillCost1 < SpillCost2 of
	    true  -> {mode1, SpillCost1};
	    false -> {mode2, SpillCost2}
	  end,
	%% io:fwrite(standard_error, "Splitting range of temp ~w in part ~w, "
	%% 	  "SpillGain: ~w, Edges: ~w~n",
	%% 	  [Temp, Part, SpillGain, Edges]),
	put(renames, get(renames)+1),
	put(introduced, get(introduced)+SpillCost),
	put(saved, get(saved)+SpillGain),
	Acc0#{Temp => {Mode, new_reg_nr(Target)}}
  end,
  decide_temps(Ts, Part, Costs, Target, Acc).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Actual second pass
rewrite(CFG, Target, Liveness, PLive, Defs, RDefs, DSets, Renames, Temps) ->
  rewrite_bbs(labels(CFG, Target), Target, Liveness, PLive, Defs, RDefs, DSets,
	      Renames, Temps, CFG).

rewrite_bbs([], _Target, _Liveness, _PLive, _Defs, _RDefs, _DSets, _Renames,
	    _Temps, CFG) ->
  CFG;
rewrite_bbs([L|Ls], Target, Liveness, PLive, Defs, RDefs, DSets, Renames, Temps,
	    CFG0) ->
  Code0Rev = lists:reverse(hipe_bb:code(BB = bb(CFG0, L, Target))),
  EntryRen = maps:get(maps:get(L,DSets), Renames),
  SubstFun = rewrite_subst_fun(Target, EntryRen),
  Fun = fun(I) -> subst_temps(SubstFun, I, Target) end,
  Liveout0 = liveout(Liveness, L, Target),
  {Code, CFG} =
    case defines_all_alloc(hd(Code0Rev), Target) of
      false ->
	RDefout0 = rdefout(L, RDefs),
	Code1 = rewrite_instrs(Code0Rev, Fun, EntryRen, Temps, Target, Liveout0,
			       RDefout0, []),
	{Code1, CFG0};
      true ->
	CallI0 = hd(Code0Rev),
	Succ = hipe_gen_cfg:succ(CFG0, L),
	{CallI, CFG1} = inject_restores(Succ, Target, Liveness, PLive, DSets,
					Renames, Temps, CallI0, CFG0),
	Liveout1 = liveness_step(CallI, Target, Liveout0),
	RDefout1 = rdefset_empty(),
	Defout = defbutlast(L, Defs),
	SpillMap = mk_spillmap(EntryRen, Liveout1, Defout, Temps, Target),
	Code1 = rewrite_instrs(tl(Code0Rev), Fun, EntryRen, Temps, Target,
			       Liveout1, RDefout1, []),
	Code2 = lift_spills(lists:reverse(Code1), Target, SpillMap, [CallI]),
	{Code2, CFG1}
    end,
  rewrite_bbs(Ls, Target, Liveness, PLive, Defs, RDefs, DSets, Renames, Temps,
	      update_bb(CFG, L, hipe_bb:code_update(BB, Code), Target)).

rewrite_instrs([], _Fun, _Ren, _Temps, _Target, _Livein, _RDefin, Acc) -> Acc;
rewrite_instrs([I|Is], Fun, Ren, Temps, Target, Liveout, RDefout, Acc0) ->
  Livein = liveness_step(I, Target, Liveout),
  RDefin = rdef_step(I, Target, RDefout),
  Def = reg_defines(I, Target),
  Mode2Spills = ordset_subtract_rdefset(
		  ordsets:intersection(
		    ordsets:from_list(Def), Liveout),
		  RDefout),
  Acc = add_mode2_spills(Mode2Spills, Target, Ren, Temps, Acc0),
  rewrite_instrs(Is, Fun, Ren, Temps, Target, Livein, RDefin, [Fun(I)|Acc]).

add_mode2_spills([], _Target, _Ren, _Temps, Acc) -> Acc;
add_mode2_spills([R|Rs], Target, Ren, Temps, Acc) ->
  case Ren of
    #{R := {mode2, NewName}} ->
      #{R := T} = Temps,
      SpillInstr = mk_move(update_reg_nr(NewName, T, Target), T, Target),
      add_mode2_spills(Rs, Target, Ren, Temps, [SpillInstr|Acc]);
    #{} ->
      add_mode2_spills(Rs, Target, Ren, Temps, Acc)
  end.

rewrite_subst_fun(Target, Ren) ->
  fun(Temp) ->
      Reg = reg_nr(Temp, Target),
      case Ren of
	#{Reg := {_Mode, NewName}} -> update_reg_nr(NewName, Temp, Target);
	#{} -> Temp
      end
  end.

mk_spillmap(Ren, Livein, Defout, Temps, Target) ->
  [begin
     Temp = maps:get(Reg, Temps),
     {NewName, mk_move(update_reg_nr(NewName, Temp, Target), Temp, Target)}
   end || {Reg, {mode1, NewName}} <- maps:to_list(Ren),
	  lists:member(Reg, Livein), defset_member(Reg, Defout)].

mk_restores(Ren, Livein, PLivein, Temps, Target) ->
  [begin
     Temp = maps:get(Reg, Temps),
     mk_move(Temp, update_reg_nr(NewName, Temp, Target), Target)
   end || {Reg, {Mode, NewName}} <- maps:to_list(Ren),
	  (       (Mode =:= mode1 andalso lists:member(Reg, Livein ))
	   orelse (Mode =:= mode2 andalso lists:member(Reg, PLivein)))].

inject_restores([], _Target, _Liveness, _PLive, _DSets, _Renames, _Temps, CFI,
		CFG) ->
  {CFI, CFG};
inject_restores([L|Ls], Target, Liveness, PLive, DSets, Renames, Temps, CFI0,
		CFG0) ->
  Ren = maps:get(maps:get(L,DSets), Renames),
  Livein = livein(Liveness, L, Target),
  PLivein = plivein(L, PLive),
  {CFI, CFG} =
    case mk_restores(Ren, Livein, PLivein, Temps, Target) of
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
  inject_restores(Ls, Target, Liveness, PLive, DSets, Renames, Temps, CFI, CFG).

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
		maps:fold(fun(Orig, {_Mode, New}, Acc1) ->
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
%% Costs ADT
%%
%% Keeps track of cumulative cost of spilling temps in particular partitions
%% using particular spill modes.
-type cost_map() :: #{[part_key()|temp()] => float()}.
-type cost_key() :: entry1 | entry2 | exit | spill.
-record(costs, {entry1 = #{} :: cost_map()
	       ,entry2 = #{} :: cost_map()
	       ,exit   = #{} :: cost_map()
	       ,spill  = #{} :: cost_map()
	       }).
-type costs() :: #costs{}.

-spec costs_new() -> costs().
costs_new() -> #costs{}.

-spec costs_insert(cost_key(), part_key(), float(), liveset(), costs())
		  -> costs().
costs_insert(entry1, A, Weight, Liveset, Costs=#costs{entry1=Entry1}) ->
  Costs#costs{entry1=costs_insert_1(A, Weight, Liveset, Entry1)};
costs_insert(entry2, A, Weight, Liveset, Costs=#costs{entry2=Entry2}) ->
  Costs#costs{entry2=costs_insert_1(A, Weight, Liveset, Entry2)};
costs_insert(exit, A, Weight, Liveset, Costs=#costs{exit=Exit}) ->
  Costs#costs{exit=costs_insert_1(A, Weight, Liveset, Exit)};
costs_insert(spill, A, Weight, Liveset, Costs=#costs{spill=Spill}) ->
  Costs#costs{spill=costs_insert_1(A, Weight, Liveset, Spill)}.

costs_insert_1(A, Weight, Liveset, CostMap0) when is_float(Weight) ->
  lists:foldl(fun(Live, CostMap1) ->
		  map_update_counter([A|Live], Weight, CostMap1)
	      end, CostMap0, Liveset).

-spec costs_map_roots(part_dsets(), costs()) -> {costs(), part_dsets()}.
costs_map_roots(DSets0, Costs) ->
  {Entry1, DSets1} = costs_map_roots_1(DSets0, Costs#costs.entry1),
  {Entry2, DSets2} = costs_map_roots_1(DSets1, Costs#costs.entry2),
  {Exit,   DSets3} = costs_map_roots_1(DSets2, Costs#costs.exit),
  {Spill,  DSets}  = costs_map_roots_1(DSets3, Costs#costs.spill),
  {#costs{entry1=Entry1,entry2=Entry2,exit=Exit,spill=Spill}, DSets}.

costs_map_roots_1(DSets0, CostMap) ->
  {NewEs, DSets} = lists:mapfoldl(fun({[A|T], Wt}, DSets1) ->
				      {AR, DSets2} = dsets_find(A, DSets1),
				      {{[AR|T], Wt}, DSets2}
				  end, DSets0, maps:to_list(CostMap)),
  {maps_from_list_merge(NewEs, fun erlang:'+'/2, #{}), DSets}.

maps_from_list_merge([], _MF, Acc) -> Acc;
maps_from_list_merge([{K,V}|Ps], MF, Acc) ->
  maps_from_list_merge(Ps, MF, case Acc of
				 #{K := OV} -> Acc#{K := MF(V, OV)};
				 #{}        -> Acc#{K => V}
			       end).

-spec costs_query(temp(), cost_key(), part_key(), costs()) -> float().
costs_query(Temp, entry1, Part, #costs{entry1=Entry1}) ->
  costs_query_1(Temp, Part, Entry1);
costs_query(Temp, entry2, Part, #costs{entry2=Entry2}) ->
  costs_query_1(Temp, Part, Entry2);
costs_query(Temp, exit, Part, #costs{exit=Exit}) ->
  costs_query_1(Temp, Part, Exit);
costs_query(Temp, spill, Part, #costs{spill=Spill}) ->
  costs_query_1(Temp, Part, Spill).

costs_query_1(Temp, Part, CostMap) ->
  Key = [Part|Temp],
  case CostMap of
    #{Key := Wt} -> Wt;
    #{} -> 0.0
  end.

-spec map_update_counter(Key, number(), #{Key => number(), OK => OV})
			-> #{Key := number(), OK => OV}.
map_update_counter(Key, Incr, Map) ->
  case Map of
    #{Key := Orig} -> Map#{Key := Orig + Incr};
    #{}            -> Map#{Key => Incr}
  end.

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
?TGT_IFACE_1(postorder).
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
