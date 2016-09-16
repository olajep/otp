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
-module(hipe_bb_weights).
-export([compute/3, weight/2, call_exn_pred/0]).
-export_type([bb_weights/0]).

-define(DO_ASSERT,1).
-include("../main/hipe.hrl").

-opaque bb_weights() :: [].

-type tuple(X)           :: {} | {X} | {X, X} | {X, X, X} | tuple(). % etc...
-type key()              :: integer().
-type row()              :: tuple(float()).
-type header()           :: tuple(key()).
-type eq_system()        :: {Header :: header(), Rows :: [row()]}.
-type assignment()       :: {key(), float()}.
-type partial_solution() :: [assignment()].
-type solution()         :: [assignment()].

%% Constant. Predicted probability of a call resulting in an exception.
-spec call_exn_pred() -> float().
call_exn_pred() -> 0.01.

compute(CFG, TgtMod, TgtCtx) ->
  Target = {TgtMod, TgtCtx},
  EqSys = build_eq_system(CFG, Target),
  solve(EqSys).

build_eq_system(_CFG, _Target) ->

  error(notimpl).

-spec triangelise(eq_system()) -> {partial_solution(), eq_system()}.
triangelise({Header0, Rows0}) ->
  {Known, Rows1} = lists:partition(fun row_constant/1, Rows0),
  EqSys1 = {Header0, Rows1},
  {Sols, EqSys2} = lists:mapfoldl(fun extract_constant/2, EqSys1, Known),
  io:fwrite(standard_error, "Sols: ~p~nEqSys1:~p~n", [Sols, EqSys2]),
  error(notimpl).

-spec row_constant(row()) -> boolean().
row_constant(Coef) -> row_constant(Coef, tuple_size(Coef)-1).
row_constant(_Coef, 0) -> false;
row_constant(Coef, I) ->
  case element(I, Coef) of
    0.0 -> row_constant(Coef, I-1);
    1.0 -> row_constant_1(Coef, I-1)
  end.
row_constant_1(_Coef, 0) -> true;
row_constant_1(Coef, I) ->
  element(I, Coef) =:= 0.0 andalso row_constant_1(Coef, I-1).

-spec extract_constant(row(), eq_system()) -> {assignment(), eq_system()}.
extract_constant(Row, {Header0, Rows0}) ->
  ?ASSERT(row_constant(Row)),
  Pos = const_pos(Row),
  Key = element(Pos, Header0),
  Header = tuple_delete_pos(Pos, Header0),
  %% {ConstRow, Rows1} = list_take_nth(Pos, Rows0),
  Rows = lists:map(fun(R) ->
		       tuple_delete_pos(
			 Pos, row_addmul(R, Row, -element(Pos, R)))
		   end, Rows0),
  {{Key, row_const_term(Row)}, {Header, Rows}}.

const_pos(Row) -> const_pos(Row, tuple_size(Row)-1).
const_pos(Row, I) ->
  case element(I, Row) of
    1.0 -> I;
    0.0 -> const_pos(Row, I-1)
  end.

%% header_pos(Key, Tuple) -> header_pos(Key, Tuple, tuple_size(Tuple)).
%% header_pos(Key, Tuple, 0) -> error(badarg, [Key, Tuple]);
%% header_pos(Key, Tuple, Index) ->
%%   case element(Index, Tuple) of
%%     Key -> Index;
%%     _Other -> header_pos(Key, Tuple, Index-1)
%%   end.

tuple_delete_pos(Pos, Tuple) ->
  list_to_tuple(list_delete_pos(Pos, tuple_to_list(Tuple))).

list_delete_pos(1, [_|Es]) -> Es;
list_delete_pos(P, [E|Es]) -> [E|list_delete_pos(P-1, Es)].

%% list_take_nth(Pos, List) -> {lists:nth(Pos, List), list_drop_nth(Pos, List)}.

%% list_drop_nth(1, [_|Es]) -> Es;
%% list_drop_nth(P, [E|Es]) -> [E|list_drop_nth(P-1, Es)].

%% Lhs + Rhs*Factor
-spec row_addmul(row(), row(), float()) -> row().
row_addmul(Lhs, Rhs, Factor) when tuple_size(Lhs) =:= tuple_size(Rhs) ->
  row_addmul_1(Lhs, Rhs, Factor, tuple_size(Lhs), []).

row_addmul_1(_Lhs, _Rhs, _Factor, 0, Acc) -> list_to_tuple(Acc);
row_addmul_1(Lhs, Rhs, Factor, Index, Acc) ->
  Elem = element(Index, Lhs) + element(Index, Rhs) * Factor,
  row_addmul_1(Lhs, Rhs, Factor, Index-1, [Elem | Acc]).

row_const_term(Row) -> element(tuple_size(Row), Row).

-spec solve(eq_system()) -> solution().
solve(EqSys0) ->
  {_Sol0, _EqSys1} = triangelise(EqSys0),
  error(notimpl).

weight(_Lbl, _Weights) ->
  error(notimpl).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Target module interface functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(TGT_IFACE_0(N), N(         {M,C}) -> M:N(         C)).
-define(TGT_IFACE_1(N), N(A1,      {M,C}) -> M:N(A1,      C)).
-define(TGT_IFACE_2(N), N(A1,A2,   {M,C}) -> M:N(A1,A2,   C)).
-define(TGT_IFACE_3(N), N(A1,A2,A3,{M,C}) -> M:N(A1,A2,A3,C)).

?TGT_IFACE_1(branch_preds).
