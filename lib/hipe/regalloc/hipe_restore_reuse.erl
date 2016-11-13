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
%% This is a simple live range splitter that tries to avoid sequences where a
%% temporary is accessed on stack multiple times by keeping a copy of that temp
%% around in a register.
%%
%% At any point where a temporary that is expected to be spilled (see uses of
%% spills_add_list/2) is defined or used, this pass considers that temporary
%% "available".
%%
%% Limitations:
%%   * If a live range part starts with several different restores, this module
%%     will introduce a new temp number for each of them, and later be forced to
%%     generate phi blocks. It would be more efficient to introduce just a
%%     single temp number. That would also remove the need for the phi blocks.
%%   ** Phi blocks are not inlined into the successor even if that successor has
%%      only the one predecessor.
%%   * If a live range part ends in a definition, that definition should just
%%     define the base temp rather than the substitution, since some CISC
%%     targets might be able to inline the memory access in the instruction.
-module(hipe_restore_reuse).

-export([split/4]).

%% Exports for hipe_range_split, which uses restore_reuse as one possible spill
%% "mode"
-export([analyse/3
	,renamed_in_block/2
	,split_in_block/2
	]).
-export_type([avail/0]).

-compile(inline).

%% -define(DO_ASSERT, 1).
-include("../main/hipe.hrl").

-type spill_grouping()   :: hipe_spill_grouping:grouping_list().
-type target_cfg()       :: any().
-type liveness()         :: any().
-type target_module()    :: module().
-type target_context()   :: any().
-type target()           :: {target_module(), target_context()}.
-type label()            :: non_neg_integer().
-type temp()             :: non_neg_integer().

-spec split(target_cfg(), liveness(), target_module(), target_context())
	   -> {target_cfg(), spill_grouping()}.
split(CFG, Liveness, TargetMod, TargetContext) ->
  Target = {TargetMod, TargetContext},
  Avail = analyse(CFG, Liveness, Target),
  rewrite(CFG, Target, Avail).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-opaque avail() :: #{label() => avail_bb()}.

-record(avail_bb, {
	  has_call :: boolean(),
	  out      :: availset(),
	  %% Gen: AvailOut generated locally
	  gen      :: availset(),
	  %% Want: DemandIn generated locally
	  want     :: ordsets:ordset(temp()),
	  %% Self: Temps with avail-demand pairs locally
	  self     :: ordsets:ordset(temp()),
	  %% DefIn: Temps shadowed
	  defin    :: ordsets:ordset(temp()),
	  pred     :: [label()],
	  succ     :: [label()]
	 }).
-type avail_bb() :: #avail_bb{}.

avail_get(L, Avail) -> maps:get(L, Avail).
avail_set(L, Val, Avail) -> maps:put(L, Val, Avail).
avail_has_call(L, Avail) -> (avail_get(L, Avail))#avail_bb.has_call.
avail_out(L, Avail) -> (avail_get(L, Avail))#avail_bb.out.
avail_self(L, Avail) -> (avail_get(L, Avail))#avail_bb.self.
avail_pred(L, Avail) -> (avail_get(L, Avail))#avail_bb.pred.
avail_succ(L, Avail) -> (avail_get(L, Avail))#avail_bb.succ.

avail_in(L, Avail) ->
  case avail_pred(L, Avail) of
    [] -> availset_empty(); % entry
    Pred ->
      lists:foldl(fun(P, ASet) ->
		      availset_intersect(avail_out(P, Avail), ASet)
		  end, availset_top(), Pred)
  end.

demand_in(L, Avail) -> (avail_get(L, Avail))#avail_bb.want.
demand_out(L, Avail) ->
  lists:foldl(fun(S, Set) ->
		  ordsets:union(demand_in(S, Avail), Set)
	      end, ordsets:new(), avail_succ(L, Avail)).

def_in(L, Avail) -> (avail_get(L, Avail))#avail_bb.defin.
def_out(L, Avail) ->
  case avail_succ(L, Avail) of
    [] -> ordsets:new(); % entry
    Succ ->
      ordsets:intersection([def_in(S, Avail) || S <- Succ])
  end.

-type availset() :: top | ordsets:ordset(temp()).
availset_empty() -> [].
availset_top() -> top.
availset_intersect(top, B) -> B;
availset_intersect(A, top) -> A;
availset_intersect(A, B) -> ordsets:intersection(A, B).
availset_union(top, _) -> top;
availset_union(_, top) -> top;
availset_union(A, B) -> ordsets:union(A, B).
%% availset_add_list(_, top) -> top;
%% availset_add_list(L, AS) -> ordsets:union(ordsets:from_list(L), AS).
%% availset_from_ordset(OS) -> OS.
ordset_intersect_availset(OS, top) -> OS;
ordset_intersect_availset(OS, AS) -> ordsets:intersection(OS, AS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec analyse(target_cfg(), liveness(), target()) -> avail().
analyse(CFG, Liveness, Target) ->
  Avail0 = analyse_init(CFG, Liveness, Target),
  RPO = reverse_postorder(CFG, Target),
  AvailLs = [L || L <- RPO, not avail_has_call(L, Avail0)],
  Avail1 = avail_dataf(AvailLs, Avail0),
  Avail2 = analyse_filter_want(maps:keys(Avail1), Avail1),
  PO = lists:reverse(RPO),
  demand_dataf(PO, Avail2).

analyse_init(CFG, Liveness, Target) ->
  analyse_init(labels(CFG, Target), CFG, Liveness, Target, #{}, []).

analyse_init([], _CFG, _Liveness, _Target, Spills, Acc) ->
  analyse_init_1(Acc, Spills, []);
analyse_init([L|Ls], CFG, Liveness, Target, Spills0, Acc) ->
  {DefIn, Gen, Self, Want, HasCall0} =
    analyse_scan(hipe_bb:code(bb(CFG, L, Target)), Target,
		 ordsets:new(), ordsets:new(), ordsets:new(),
		 ordsets:new()),
  {Spills, Out, HasCall} =
    case HasCall0 of
      false -> {Spills0, availset_top(), false};
      {true, CallDefs} ->
	Spill = ordsets:subtract(liveout(Liveness, L, Target), CallDefs),
	{spills_add_list(Spill, Spills0), Gen, true}
    end,
  Pred = hipe_gen_cfg:pred(CFG, L),
  Succ = hipe_gen_cfg:succ(CFG, L),
  Val = #avail_bb{gen=Gen, want=Want, self=Self, out=Out, has_call=HasCall,
		  pred=Pred, succ=Succ, defin=DefIn},
  analyse_init(Ls, CFG, Liveness, Target, Spills, [{L, Val} | Acc]).

analyse_init_1([], _Spills, Acc) -> maps:from_list(Acc);
analyse_init_1([{L, Val0}|Vs], Spills, Acc) ->
  #avail_bb{out=Out,gen=Gen,want=Want,self=Self} = Val0,
  Val = Val0#avail_bb{
	  out  = spills_filter_availset(Out,  Spills),
	  gen  = spills_filter_availset(Gen,  Spills),
	  want = spills_filter_availset(Want, Spills),
	  self = spills_filter_availset(Self, Spills)},
  analyse_init_1(Vs, Spills, [{L, Val} | Acc]).

spills_add_list([], Spills) -> Spills;
spills_add_list([R|Rs], Spills) -> spills_add_list(Rs, Spills#{R => []}).

spills_filter_availset([E|Es], Spills) ->
  case Spills of
    #{E := _} -> [E|spills_filter_availset(Es, Spills)];
    #{} ->          spills_filter_availset(Es, Spills)
  end;
spills_filter_availset([], _) -> [];
spills_filter_availset(top, _) -> top.

analyse_scan([], _Target, Defs, Gen, Self, Want) ->
  {Defs, Gen, Self, Want, false};
analyse_scan([I|Is], Target, Defs0, Gen0, Self0, Want0) ->
  {DefL, UseL} = reg_def_use(I, Target),
  Use = ordsets:from_list(UseL),
  Def = ordsets:from_list(DefL),
  Self = ordsets:union(ordsets:intersection(Use, Gen0), Self0),
  Want = ordsets:union(ordsets:subtract(Use, Defs0), Want0),
  Defs = ordsets:union(Def, Defs0),
  case defines_all_alloc(I, Target) of
    true ->
      [] = Is, %assertion
      {Defs, ordsets:new(), Self, Want, {true, Def}};
    false ->
      Gen = ordsets:union(ordsets:union(Def, Use), Gen0),
      analyse_scan(Is, Target, Defs, Gen, Self, Want)
  end.

avail_dataf(RPO, Avail0) ->
  case avail_dataf_once(RPO, Avail0, 0) of
    {Avail, 0} -> Avail;
    {Avail, _Changed} ->
      avail_dataf(RPO, Avail)
  end.

avail_dataf_once([], Avail, Changed) -> {Avail, Changed};
avail_dataf_once([L|Ls], Avail0, Changed0) ->
  ABB = #avail_bb{out=OldOut, gen=Gen} = avail_get(L, Avail0),
  In = avail_in(L, Avail0),
  {Changed, Avail} =
    case availset_union(In, Gen) of
      OldOut -> {Changed0, Avail0};
      Out -> {Changed0+1, avail_set(L, ABB#avail_bb{out=Out}, Avail0)}
    end,
  avail_dataf_once(Ls, Avail, Changed).

analyse_filter_want([], Avail) -> Avail;
analyse_filter_want([L|Ls], Avail0) ->
  ABB = #avail_bb{want=Want0, defin=DefIn0} = avail_get(L, Avail0),
  In = avail_in(L, Avail0),
  Want = ordset_intersect_availset(Want0, In),
  DefIn = ordset_intersect_availset(DefIn0, In),
  Avail = avail_set(L, ABB#avail_bb{want=Want, defin=DefIn}, Avail0),
  analyse_filter_want(Ls, Avail).

demand_dataf(PO, Avail0) ->
  case demand_dataf_once(PO, Avail0, 0) of
    {Avail, 0} -> Avail;
    {Avail, _Changed} ->
      demand_dataf(PO, Avail)
  end.

demand_dataf_once([], Avail, Changed) -> {Avail, Changed};
demand_dataf_once([L|Ls], Avail0, Changed0) ->
  ABB0 = #avail_bb{want=OldIn,defin=OldDef} = avail_get(L, Avail0),
  AvailIn = avail_in(L, Avail0),
  Out = demand_out(L, Avail0),
  DefOut = def_out(L, Avail0),
  {Changed, Avail} =
    case {ordsets:union(ordset_intersect_availset(Out,    AvailIn), OldIn),
	  ordsets:union(ordset_intersect_availset(DefOut, AvailIn), OldDef)}
    of
      {OldIn, OldDef} -> {Changed0, Avail0};
      {In, DefIn} ->
	ABB = ABB0#avail_bb{want=In,defin=DefIn},
	{Changed0+1, avail_set(L, ABB, Avail0)}
    end,
  demand_dataf_once(Ls, Avail, Changed).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rewrite(CFG, Target, Avail) ->
  RPO = reverse_postorder(CFG, Target),
  rewrite(RPO, Target, Avail, #{}, [], CFG).

rewrite([], _Target, _Avail, _Input, SpillGroups, CFG) -> {CFG, SpillGroups};
rewrite([L|Ls], Target, Avail, Input0, SpillGroups0, CFG0) ->
  SplitHere = split_in_block(L, Avail),
  {Input1, LInput} =
    case Input0 of
      #{L := LInput0} -> {Input0, LInput0};
      #{} -> {Input0#{L => []}, []} % entry block
    end,
  ?ASSERT([] =:= [X || X <- SplitHere, orddict:is_key(X, LInput)]),
  ?ASSERT(demand_in(L, Avail) =:= orddict:fetch_keys(LInput)),
  {CFG1, LOutput, SpillGroups} =
    case {SplitHere, LInput} of
      {[], []} -> % optimisation (rewrite will do nothing, so skip it)
	{CFG0, LInput, SpillGroups0};
      _ ->
	Code0 = hipe_bb:code(BB=bb(CFG0, L, Target)),
	DefOut = def_out(L, Avail),
	{Code, LOutput0, _DefIn, SpillGroups1} =
	  rewrite_instrs(Code0, Target, LInput, DefOut, SplitHere, SpillGroups0),
	{update_bb(CFG0, L, hipe_bb:code_update(BB, Code), Target),
	 LOutput0, SpillGroups1}
    end,
  {Input, CFG} = rewrite_succs(avail_succ(L, Avail), Target, L, LOutput, Avail,
			       Input1, CFG1),
  rewrite(Ls, Target, Avail, Input, SpillGroups, CFG).

-spec renamed_in_block(label(), avail()) -> ordsets:ordset(temp()).
renamed_in_block(L, Avail) ->
  ordsets:union(avail_self(L, Avail), demand_out(L, Avail)).

-spec split_in_block(label(), avail()) -> ordsets:ordset(temp()).
split_in_block(L, Avail) ->
  ordsets:subtract(renamed_in_block(L, Avail), demand_in(L, Avail)).

rewrite_instrs([], _Target, Output, DefOut, [], SpillGroups) ->
  {[], Output, DefOut, SpillGroups};
rewrite_instrs([I|Is], Target, Input0, BBDefOut, SplitHere0, SpillGroups0) ->
  {TDef, TUse} = def_use(I, Target),
  {Def, Use} = {reg_names(TDef, Target), reg_names(TUse, Target)},
  {ISplits, SplitHere} =
    lists:partition(fun(R) ->
			lists:member(R, Def) orelse lists:member(R, Use)
		    end, SplitHere0),
  {Input, SpillGroups1, Restores} =
    case ISplits of
      [] -> {Input0, SpillGroups0, []};
      _ ->
	make_splits(ISplits, Target, TDef, TUse, Input0, SpillGroups0, [])
    end,
  SubstFun = fun(Temp) ->
		 case orddict:find(reg_nr(Temp, Target), Input) of
		   {ok, NewTemp} -> NewTemp;
		   error -> Temp
		 end
	     end,
  {Acc0, Output, DefOut, SpillGroups} =
    rewrite_instrs(Is, Target, Input, BBDefOut, SplitHere, SpillGroups1),
  Acc1 = insert_spills(TDef, Target, Input, DefOut, Acc0),
  Acc = Restores ++ [subst_temps(SubstFun, I, Target) | Acc1],
  DefIn = ordsets:union(DefOut, Def),
  {Acc, Output, DefIn, SpillGroups}.

make_splits([], _Target, _TDef, _TUse, Input, SpillGroups, Acc) ->
  {Input, SpillGroups, Acc};
make_splits([S|Ss], Target, TDef, TUse, Input0, SpillGroups0, Acc0) ->
  SubstReg = new_reg_nr(Target),
  {Acc, Subst} =
    case find_reg_temp(S, TUse, Target) of
      error ->
	{ok, Temp} = find_reg_temp(S, TDef, Target),
	{Acc0, update_reg_nr(SubstReg, Temp, Target)};
      {ok, Temp} ->
	Subst0 = update_reg_nr(SubstReg, Temp, Target),
	Acc1 = [mk_move(Temp, Subst0, Target) | Acc0],
	{Acc1, Subst0}
    end,
  Input = orddict:store(S, Subst, Input0),
  SpillGroups = [{SubstReg, S} | SpillGroups0],
  make_splits(Ss, Target, TDef, TUse, Input, SpillGroups, Acc).

find_reg_temp(_Reg, [], _Target) -> error;
find_reg_temp(Reg, [T|Ts], Target) ->
  case reg_nr(T, Target) of
    Reg -> {ok, T};
    _ -> find_reg_temp(Reg, Ts, Target)
  end.

insert_spills([], _Target, _Input, _DefOut, Acc) -> Acc;
insert_spills([T|Ts], Target, Input, DefOut, Acc0) ->
  R = reg_nr(T, Target),
  Acc =
    case orddict:find(R, Input) of
      error -> Acc0;
      {ok, Subst} ->
	case lists:member(R, DefOut) of
	  true -> Acc0;
	  false -> [mk_move(Subst, T, Target) | Acc0]
	end
    end,
  insert_spills(Ts, Target, Input, DefOut, Acc).

rewrite_succs([], _Target, _P, _POutput, _Avail, Input, CFG) -> {Input, CFG};
rewrite_succs([L|Ls], Target, P, POutput, Avail, Input0, CFG0) ->
  NewLInput = orddict_intersect_ordset(POutput, demand_in(L, Avail)),
  {Input, CFG} =
    case Input0 of
      #{L := LInput} ->
	CFG2 =
	  case required_phi_moves(LInput, NewLInput) of
	    [] -> CFG0;
	    ReqMovs ->
	      PhiLb = new_label(Target),
	      Code = [mk_move(S,D,Target) || {S,D} <- ReqMovs]
		++ [mk_goto(L, Target)],
	      PhiBB = hipe_bb:mk_bb(Code),
	      CFG1 = update_bb(CFG0, PhiLb, PhiBB, Target),
	      bb_redirect_jmp(L, PhiLb, P, CFG1, Target)
	  end,
	{Input0, CFG2};
      #{} ->

	{Input0#{L => NewLInput}, CFG0}
    end,
  rewrite_succs(Ls, Target, P, POutput, Avail, Input, CFG).

bb_redirect_jmp(From, To, Lb, CFG, Target) ->
  BB0 = bb(CFG, Lb, Target),
  Last = redirect_jmp(hipe_bb:last(BB0), From, To, Target),
  BB = hipe_bb:code_update(BB0, hipe_bb:butlast(BB0) ++ [Last]),
  update_bb(CFG, Lb, BB, Target).

required_phi_moves([], []) -> [];
required_phi_moves([P|Is], [P|Os]) -> required_phi_moves(Is, Os);
required_phi_moves([{K, In}|Is], [{K, Out}|Os]) ->
  [{Out, In}|required_phi_moves(Is, Os)].

orddict_intersect_ordset([], _) -> [];
orddict_intersect_ordset(_, []) -> [];
orddict_intersect_ordset([{K, _}=P|Ds], [K|Ss]) ->
  [P|orddict_intersect_ordset(Ds, Ss)];
orddict_intersect_ordset([{K1, _}|Ds], [K2|_]=Ss) when K1 < K2 ->
  orddict_intersect_ordset(Ds, Ss);
orddict_intersect_ordset([{K1, _}|_]=Ds, [K2|Ss]) when K1 > K2 ->
  orddict_intersect_ordset(Ds, Ss).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Target module interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TGT_IFACE_0(N), N(         {M,C}) -> M:N(         C)).
-define(TGT_IFACE_1(N), N(A1,      {M,C}) -> M:N(A1,      C)).
-define(TGT_IFACE_2(N), N(A1,A2,   {M,C}) -> M:N(A1,A2,   C)).
-define(TGT_IFACE_3(N), N(A1,A2,A3,{M,C}) -> M:N(A1,A2,A3,C)).

?TGT_IFACE_2(bb).
?TGT_IFACE_1(def_use).
%% ?TGT_IFACE_1(defines).
?TGT_IFACE_1(defines_all_alloc).
%% ?TGT_IFACE_1(is_precoloured).
?TGT_IFACE_1(labels).
?TGT_IFACE_1(mk_goto).
?TGT_IFACE_2(mk_move).
?TGT_IFACE_0(new_label).
?TGT_IFACE_0(new_reg_nr).
%% ?TGT_IFACE_1(postorder).
?TGT_IFACE_3(redirect_jmp).
?TGT_IFACE_1(reg_nr).
?TGT_IFACE_1(reverse_postorder).
?TGT_IFACE_2(subst_temps).
?TGT_IFACE_3(update_bb).
?TGT_IFACE_2(update_reg_nr).

%% branch_preds(Instr, {TgtMod,TgtCtx}) ->
%%   merge_sorted_preds(lists:keysort(1, TgtMod:branch_preds(Instr, TgtCtx))).

%% livein(Liveness, L, Target={TgtMod,TgtCtx}) ->
%%   ordsets:from_list(reg_names(TgtMod:livein(Liveness, L, TgtCtx), Target)).

liveout(Liveness, L, Target={TgtMod,TgtCtx}) ->
  ordsets:from_list(reg_names(TgtMod:liveout(Liveness, L, TgtCtx), Target)).

%% merge_sorted_preds([]) -> [];
%% merge_sorted_preds([{L, P1}, {L, P2}|LPs]) ->
%%   merge_sorted_preds([{L, P1+P2}|LPs]);
%% merge_sorted_preds([LP|LPs]) -> [LP|merge_sorted_preds(LPs)].

reg_names(Regs, {TgtMod,TgtCtx}) ->
  [TgtMod:reg_nr(X,TgtCtx) || X <- Regs].

reg_def_use(I, Target) ->
  {TDef, TUse} = def_use(I, Target),
  {reg_names(TDef, Target), reg_names(TUse, Target)}.
