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
%%	                BASIC BLOCK WEIGHTING
%%
%% Computes basic block weights by using branch probabilities as weights in a
%% linear equation system, that is then solved using Gauss-Jordan Elimination.
%%
%% The equation system representation is intentionally sparse, since most blocks
%% have at most two successors.
-module(hipe_bb_weights).
-export([compute/3, weight/2, call_exn_pred/0]).
-export_type([bb_weights/0]).

-define(DO_ASSERT,1).
-include("../main/hipe.hrl").

-opaque bb_weights() :: #{label() => float()}.

-type cfg() :: any().
-type target_module() :: module().
-type target_context() :: any().

%% -type tuple(X)           :: {} | {X} | {X, X} | {X, X, X} | tuple(). % etc...
-type label()            :: integer().
-type var()              :: label().
-type assignment()       :: {var(), float()}.
%% -type partial_solution() :: [assignment()].
-type solution()         :: [assignment()].

%% Constant. Predicted probability of a call resulting in an exception.
-spec call_exn_pred() -> float().
call_exn_pred() -> 0.01.

-spec compute(cfg(), target_module(), target_context()) -> bb_weights().
compute(CFG, TgtMod, TgtCtx) ->
  Target = {TgtMod, TgtCtx},
  EqSys = build_eq_system(CFG, Target),
  Solution = solve(EqSys),
  %% {M,F,A} = element(2, element(3, CFG)),
  %% Max = lists:max([C || {_,C} <- Solution]),
  %% Min = lists:min([C || {_,C} <- Solution]),
  %% io:fwrite("min: ~-22w, max: ~-22w, ~w:~w/~w~n", [Min,Max,M,F,A]),
  maps:from_list(Solution).

build_eq_system(CFG, Target) ->
  StartLb = hipe_gen_cfg:start_label(CFG),
  EQS0 = eqs_new(),
  EQS = eqs_insert(row_new([{StartLb, 1.0}], 1.0), EQS0),
  [] = hipe_gen_cfg:pred(CFG, StartLb),
  build_eq_system(labels(CFG, Target) -- [StartLb], CFG, Target, EQS).

build_eq_system([], _CFG, _Target, EQS) -> EQS;
build_eq_system([L|Ls], CFG, Target, EQS0) ->
  EQS =
    case hipe_gen_cfg:pred(CFG, L) of
      [] -> EQS0;
      %% [Pred] -> eqs_insert(row_new([{Pred, 1.0}, {L, -1.0}], 0.0), EQS0);
      Preds ->
	PredProb = lists:map(fun(Pred) ->
				 BB = bb(CFG, Pred, Target),
				 Ps = branch_preds(hipe_bb:last(BB), Target),
				 ?ASSERT(length(Ps) =:= length(hipe_gen_cfg:succ(CFG, Pred))),
				 {L, Prob} = lists:keyfind(L, 1, Ps),
				 {Pred, Prob}
			     end, Preds),
	eqs_insert(row_new([{L, -1.0}|PredProb], 0.0), EQS0)
    end,
  build_eq_system(Ls, CFG, Target, EQS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec triangelise(eq_system()) -> {[{var(), key()}], eq_system()}.
triangelise(EQS) ->
  triangelise(lists:sort(eqs_vars(EQS)), EQS, #{}).

triangelise([], EQS, Assoc) -> {[{K, V} || {V, K} <- maps:to_list(Assoc)], EQS};
triangelise([V|Vs], EQS0, Assoc0) ->
  %% Pick a row to be only one mentioning V
  %% We pick the smallest unused eq
  [{_, Key}|_] = lists:sort([{row_size(eqs_get(Key, EQS0)), Key}
			     || Key <- eqs_lookup(V, EQS0),
				not maps:is_key(Key, Assoc0)]),
  Row0 = eqs_get(Key, EQS0),
  Row = row_normalise(V, Row0),
  EQS1 = eqs_put(Key, Row, EQS0),
  %% io:fwrite("l~w's row: ", [V]), row_print(Row),
  EQS = eliminate(V, Key, Row, EQS1),
  triangelise(Vs, EQS, Assoc0#{Key => V}).

row_normalise(Var, Row) ->
  %% Normalise v's coef to 1.0
  %% row_set_coef ensures the coef is exactly 1.0 (no rounding errors)
  row_set_coef(Var, 1.0, row_scale(Row, 1.0/row_get(Var, Row))).

  %% {Known, Rows1} = lists:partition(fun row_constant/1, Rows0),
  %% EqSys1 = {Header0, Rows1},
  %% {Sols, EqSys2} = lists:mapfoldl(fun extract_constant/2, EqSys1, Known),
  %% io:fwrite(standard_error, "Sols: ~p~nEqSys1:~p~n", [Sols, EqSys2]),
  %% notimpl:?FUNCTION_NAME().

%% -spec row_constant(row()) -> boolean().
%% row_constant(Coef) -> row_constant(Coef, tuple_size(Coef)-1).
%% row_constant(_Coef, 0) -> false;
%% row_constant(Coef, I) ->
%%   case element(I, Coef) of
%%     0.0 -> row_constant(Coef, I-1);
%%     1.0 -> row_constant_1(Coef, I-1)
%%   end.
%% row_constant_1(_Coef, 0) -> true;
%% row_constant_1(Coef, I) ->
%%   element(I, Coef) =:= 0.0 andalso row_constant_1(Coef, I-1).

%% -spec extract_constant(rowp(), eq_system()) -> {assignment(), eq_system()}.
%% extract_constant({Key, Row}, EQS0) ->
%%   [{Var, 1.0}] = row_coefs(Row),
%%   %% Pos = const_pos(Row),
%%   %% Var = element(Pos, Header0),
%%   %% Header = tuple_delete_pos(Pos, Header0),
%%   %% {ConstRow, Rows1} = list_take_nth(Pos, Rows0),
%%   EQS = eliminate(Var, Key, Row, EQS0),
%%   ?ASSERT([] =:= eqs_lookup(Var, EQS)),
%%   {{Var, row_const(Row)}, EQS}.

%% Precondition: Row must be normalised; i.e. Vars coef must be 1.0 (mod rounding errors)
-spec eliminate(var(), key(), row(), eq_system()) -> eq_system().
eliminate(Var, Key, Row, EQS0) ->
  ?ASSERT(1.0 =:= row_get(Var, Row)),
  EQS =
    lists:foldl(fun(RK, EQS1) when RK =:= Key -> EQS1;
		   (RK, EQS1) ->
		    R = eqs_get(RK, EQS1),
		    eqs_put(RK, row_addmul(R, Row, -row_get(Var, R)), EQS1)
		end, EQS0, eqs_lookup(Var, EQS0)),
  [Key] = eqs_lookup(Var, EQS),
  EQS.


%% const_pos(Row) -> const_pos(Row, tuple_size(Row)-1).
%% const_pos(Row, I) ->
%%   case element(I, Row) of
%%     1.0 -> I;
%%     0.0 -> const_pos(Row, I-1)
%%   end.

%% header_pos(Key, Tuple) -> header_pos(Key, Tuple, tuple_size(Tuple)).
%% header_pos(Key, Tuple, 0) -> error(badarg, [Key, Tuple]);
%% header_pos(Key, Tuple, Index) ->
%%   case element(Index, Tuple) of
%%     Key -> Index;
%%     _Other -> header_pos(Key, Tuple, Index-1)
%%   end.

%% tuple_delete_pos(Pos, Tuple) ->
%%   list_to_tuple(list_delete_pos(Pos, tuple_to_list(Tuple))).

%% list_delete_pos(1, [_|Es]) -> Es;
%% list_delete_pos(P, [E|Es]) -> [E|list_delete_pos(P-1, Es)].

%% list_take_nth(Pos, List) -> {lists:nth(Pos, List), list_drop_nth(Pos, List)}.

%% list_drop_nth(1, [_|Es]) -> Es;
%% list_drop_nth(P, [E|Es]) -> [E|list_drop_nth(P-1, Es)].

-spec solve(eq_system()) -> solution().
solve(EQS0) ->
  {VarEqs, EQS1} = triangelise(EQS0),
  %% lists:foreach(fun({Var, Key}) ->
  %% 		    io:fwrite("l~w: ", [Var]), row_print(eqs_get(Key, EQS1))
  %% 		end, VarEqs),
  %% eqs_print(EQS1),
  %% Sort equations by size
  %% XXX: are they already sorted ?! (due to triangleisation)
  %% VarEqSiz = lists:sort([{row_size(eqs_get(Key, EQS0)), Var, Key}
  %% 			     || {Var, Key} <- VarEqs0]),
  %% VarEqs = [{Var, Key} || {_, Var, Key} <- VarEqSiz],
  solve_1(VarEqs, maps:from_list(VarEqs), EQS1, []).
  %% solve_2([K || {_, K} <- VarEqs0], EQS1, []).

%% solve_1(EQS0, Acc0) ->
%%   case eqs_no_vars(EQS0) of
%%     0 -> Acc0;
%%     _ ->
%%       Trivial = eqs_of_size(1, EQS0),
%%       ?ASSERT(Trivial =/= []),
%%       {EQS, Acc} = solve_2(Trivial, EQS0, Acc0),
%%       solve_1(EQS, Acc)
%%   end.

%% solve_2([], EQS, Acc) -> {EQS, Acc};
%% solve_2([K|Ks], EQS0, Acc0) ->
%%   Row0 = eqs_get(K, EQS0),
%%   [{V,Coef0}] = row_coefs(Row0),
%%   Row = row_normalise(V, Row0),
%%   EQS1 = eliminate(V, K, Row, EQS0),
%%   ?ASSERT([K] =:= eqs_lookup(V, EQS1)),
%%   io:format("Solved l~w = ~w~n", [V, row_const(Row)]),
%%   solve_2(Ks, eqs_remove(K, EQS1), [{V, row_const(Row)}|Acc0]).

solve_1([], _VarEqs, _EQS, Acc) -> Acc;
solve_1([{V,K}|Ps], VarEqs, EQS0, Acc0) ->
  Row0 = eqs_get(K, EQS0),
  %% io:fwrite("Solving l~w, eq: ", [V]), row_print(Row0),
  VarsToKill = [Var || {Var, _} <- row_coefs(Row0), Var =/= V],
  Row1 = kill_vars(VarsToKill, VarEqs, EQS0, Row0),
  [{V,_}] = row_coefs(Row1), % assertion
  Row = row_normalise(V, Row1),
  [{V,1.0}] = row_coefs(Row), % assertion
  EQS = eliminate(V, K, Row, EQS0),
  [K] = eqs_lookup(V, EQS),
  solve_1(Ps, VarEqs, eqs_remove(K, EQS), [{V, row_const(Row)}|Acc0]).

kill_vars([], _VarEqs, _EQS, Row) -> Row;
kill_vars([V|Vs], VarEqs, EQS, Row0) ->
  VRow0 = eqs_get(maps:get(V, VarEqs), EQS),
  VRow = row_normalise(V, VRow0),
  ?ASSERT(1.0 =:= row_get(V, VRow)),
  Row = row_addmul(Row0, VRow, -row_get(V, Row0)),
  ?ASSERT(0.0 =:= row_get(V, Row)), % V has been killed
  kill_vars(Vs, VarEqs, EQS, Row).

-spec weight(label(), bb_weights()) -> float().
weight(Lbl, Weights) ->
  maps:get(Lbl, Weights).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Row datatype
%% Invariant: No 0.0 coefficiets!
-spec row_empty() -> row().
row_empty() -> {orddict:new(), 0.0}.

-spec row_new([{var(), float()}], float()) -> row().
row_new(Coefs, Const) when is_float(Const) ->
  row_ensure_invar({row_squash_multiples(lists:keysort(1, Coefs)), Const}).

row_squash_multiples([{K, C1},{K, C2}|Ps]) ->
  row_squash_multiples([{K,C1+C2}|Ps]);
row_squash_multiples([P|Ps]) -> [P|row_squash_multiples(Ps)];
row_squash_multiples([]) -> [].

row_ensure_invar({Coef, Const}) ->
  {orddict:filter(fun(_, 0.0) -> false; (_, F) when is_float(F) -> true end,
		  Coef), Const}.

row_const({_, Const}) -> Const.
row_coefs({Coefs, _}) -> orddict:to_list(Coefs).
row_size({Coefs, _}) -> orddict:size(Coefs).

row_get(Var, {Coefs, _}) -> %% orddict:fetch(Var, Coefs).
  case lists:keyfind(Var, 1, Coefs) of
    false -> 0.0;
    {_, Coef} -> Coef
  end.

row_set_coef(Var, 0.0, {Coefs, Const}) ->
  {orddict:erase(Var, Coefs), Const};
row_set_coef(Var, Coef, {Coefs, Const}) ->
  {orddict:store(Var, Coef, Coefs), Const}.

%% Lhs + Rhs*Factor
-spec row_addmul(row(), row(), float()) -> row().
row_addmul({LhsCoefs, LhsConst}, {RhsCoefs, RhsConst}, Factor) ->
  Coefs = row_addmul_coefs(LhsCoefs, RhsCoefs, Factor),
  Const = LhsConst + RhsConst * Factor,
  {Coefs, Const}.

row_addmul_coefs(Ls, [], _Factor) -> Ls;
row_addmul_coefs([], Rs, Factor) -> [{V, C*Factor} || {V, C} <- Rs];
row_addmul_coefs([L={LV, _}|Ls], Rs=[{RV,_}|_], Factor) when LV < RV ->
  [L|row_addmul_coefs(Ls, Rs, Factor)];
row_addmul_coefs(Ls=[{LV, _}|_], [{RV, RC}|Rs], Factor) when LV > RV ->
  [{RV, RC*Factor}|row_addmul_coefs(Ls, Rs, Factor)];
row_addmul_coefs([{V, LC}|Ls], [{V, RC}|Rs], Factor) ->
  case LC + RC * Factor of
    0.0 ->      row_addmul_coefs(Ls, Rs, Factor);
    C -> [{V,C}|row_addmul_coefs(Ls, Rs, Factor)]
  end.

%% XXX: Optimise (needed?)
row_scale(Row, Factor) -> row_addmul(row_empty(), Row, Factor).
%% row_add(Lhs, Rhs) -> row_addmul(Lhs, Rhs, 1.0).
%% row_sub(Lhs, Rhs) -> row_addmul(Lhs, Rhs, -1.0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Equation system ADT
%%
%% Stores a linear equation system, allowing for efficient updates, queries for
%% all equations mentioning a variable, and queries for equations of a specific
%% size.
%%
%% It is sort of like a "database" table of {Primary, Terms, Const} indexed both
%% on Primary as well as the vars (map keys) in Terms.
-type row()       :: {Terms :: orddict:orddict(var(), float()),
		      Const :: float()}.
-type key()       :: non_neg_integer().
%% -type rowp()      :: {key(), row()}.
-type rev_index() :: #{var() => ordsets:ordset(key())}.
%% -type sizset()    :: #{key() => []}. % set
%% -type sizidx()    :: gb_trees:tree(non_neg_integer(), sizset()).
-record(eq_system, {
	  rows = #{}              :: #{key() => row()},
	  revidx = revidx_empty() :: rev_index(),
	  %% sizidx = sizidx_empty() :: sizidx(),
	  next_key = 0            :: key()
	 }).
-type eq_system() :: #eq_system{}.

eqs_new() -> #eq_system{}.

eqs_insert(Row, EQS=#eq_system{next_key=NextKey0}) ->
  Key = NextKey0,
  NextKey = NextKey0 + 1,
  eqs_insert(Key, Row, EQS#eq_system{next_key=NextKey}).

eqs_insert(Key, Row, EQS=#eq_system{rows=Rows, revidx=RevIdx0%% , sizidx=SizIdx0
				   }) ->
  %% io:fwrite("Adding ~w:~w to ~p~n", [Key, Row, EQS]),
  RevIdx = revidx_add(Key, Row, RevIdx0),
  %% SizIdx = sizidx_add(Key, Row, SizIdx0),
  EQS#eq_system{rows=Rows#{Key => Row}, revidx=RevIdx%% , sizidx=SizIdx
	       }.

eqs_put(Key, Row, EQS0) ->
  eqs_insert(Key, Row, eqs_remove(Key, EQS0)).

eqs_remove(Key, EQS=#eq_system{rows=Rows, revidx=RevIdx0%% , sizidx=SizIdx0
			      }) ->
  OldRow = maps:get(Key, Rows),
  %% io:fwrite("Removing ~w:~w from ~p~n", [Key, OldRow, EQS]),
  RevIdx = revidx_remove(Key, OldRow, RevIdx0),
  %% SizIdx = sizidx_remove(Key, OldRow, SizIdx0),
  EQS#eq_system{rows = maps:remove(Key, Rows), revidx=RevIdx%% , sizidx=SizIdx
	       }.

-spec eqs_get(key(), eq_system()) -> row().
eqs_get(Key, #eq_system{rows=Rows}) -> maps:get(Key, Rows).

%% Keys of all equations containing a nonzero coefficient for Var
-spec eqs_lookup(var(), eq_system()) -> ordsets:ordset(key()).
eqs_lookup(Var, #eq_system{revidx=RevIdx}) -> maps:get(Var, RevIdx).

%% -spec eqs_rows(eq_system()) -> [rowp()].
%% eqs_rows(#eq_system{rows=Rows}) -> maps:to_list(Rows).

%% -spec eqs_no_vars(eq_system()) -> [var()].
%% eqs_no_vars(#eq_system{revidx=RevIdx}) -> map_size(RevIdx).

-spec eqs_vars(eq_system()) -> [var()].
eqs_vars(#eq_system{revidx=RevIdx}) -> maps:keys(RevIdx).

%% -spec eqs_of_size(non_neg_integer(), eq_system()) -> [key()].
%% eqs_of_size(Siz, #eq_system{sizidx=SizIdx}) ->
%%   case gb_trees:lookup(Siz, SizIdx) of
%%     none -> [];
%%     {value, SizSet} -> sizset_to_list(SizSet)
%%   end.

%% eqs_print(EQS) ->
%%   lists:foreach(fun({_, Row}) ->
%% 		    row_print(Row)
%% 		end, eqs_rows(EQS)).

%% row_print(Row) ->
%%   CoefStrs = [io_lib:format("~wl~w", [Coef, Var])
%% 	      || {Var, Coef} <- row_coefs(Row)],
%%   CoefStr = lists:join(" + ", CoefStrs),
%%   io:format("~w = ~s~n", [row_const(Row), CoefStr]).

%% eqs_iterator(#eq_system{rows=Rows}) -> map_iterator(Rows).
%% eqs_next(Iter) -> map_next(Iter).

revidx_empty() -> #{}.

-spec revidx_add(key(), row(), rev_index()) -> rev_index().
revidx_add(Key, Row, RevIdx0) ->
  orddict:fold(fun(Var, _Coef, RevIdx1) ->
		?ASSERT(_Coef /= 0.0),
		RevIdx1#{Var => ordsets:add_element(
				  Key, maps:get(Var, RevIdx1, ordsets:new()))}
	    end, RevIdx0, row_coefs(Row)).

-spec revidx_remove(key(), row(), rev_index()) -> rev_index().
revidx_remove(Key, {Coefs, _}, RevIdx0) ->
  orddict:fold(fun(Var, _Coef, RevIdx1) ->
		case RevIdx1 of
		  #{Var := Keys0} ->
		    case ordsets:del_element(Key, Keys0) of
		      [] -> maps:remove(Var, RevIdx1);
		      Keys -> RevIdx1#{Var := Keys}
		    end
		   %% ;#{} -> RevIdx1 % Not in index (bug?)
		end
	    end, RevIdx0, Coefs).

%% sizidx_empty() -> gb_trees:empty().

%% -spec sizidx_add(key(), row(), sizidx()) -> sizidx().
%% sizidx_add(Key, Row, SizIdx) ->
%%   Size = row_size(Row),
%%   case gb_trees:lookup(Size, SizIdx) of
%%     none -> gb_trees:insert(Size, sizset_add(Key, sizset_empty()), SizIdx);
%%     {value, Set} ->
%%       gb_trees:update(Size, sizset_add(Key, Set), SizIdx)
%%   end.

%% -spec sizidx_remove(key(), row(), sizidx()) -> sizidx().
%% sizidx_remove(Key, Row, SizIdx) ->
%%   Size = row_size(Row),
%%   Set0 = gb_trees:get(Size, SizIdx),
%%   Set = sizset_del(Key, Set0),
%%   case sizset_is_empty(Set) of
%%     true -> gb_trees:delete(Size, SizIdx);
%%     false -> gb_trees:update(Size, Set, SizIdx)
%%   end.

%% sizset_empty() -> #{}.
%% sizset_is_empty(Set) -> map_size(Set) =:= 0.
%% sizset_add(E, Set) -> Set#{E => []}.
%% sizset_del(E, Set) -> maps:remove(E, Set).
%% %% sizset_to_list(Set) -> maps:keys(Set).

%% map_iterator(Map) -> maps:to_list(Map).
%% map_next([]) -> none;
%% map_next([{K,V}|Ps]) -> {K, V, Ps}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Target module interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TGT_IFACE_0(N), N(         {M,C}) -> M:N(         C)).
-define(TGT_IFACE_1(N), N(A1,      {M,C}) -> M:N(A1,      C)).
-define(TGT_IFACE_2(N), N(A1,A2,   {M,C}) -> M:N(A1,A2,   C)).
-define(TGT_IFACE_3(N), N(A1,A2,A3,{M,C}) -> M:N(A1,A2,A3,C)).

?TGT_IFACE_2(bb).
?TGT_IFACE_1(branch_preds).
?TGT_IFACE_1(labels).
