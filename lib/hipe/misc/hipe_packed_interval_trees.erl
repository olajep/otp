%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2016. All Rights Reserved.
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
%%% Perfect interval trees packed into tuples.
%%%
%%% Optimised for the build once, intersect lots use case.
%%%
%%% Keys are the (0-based) indices into the list passed to build.

-module(hipe_packed_interval_trees).

-export([build/1, intersect/2]).

-opaque tree() :: tuple().
-export_type([tree/0]).

-spec build([{integer(), integer()}]) -> tree().
build(ListOfIntervals) ->
    list_to_tuple(lists:append([[Lo, Hi] || {Lo, Hi} <- ListOfIntervals])).

-spec intersect(integer(), tree()) -> [non_neg_integer()].
intersect(Point, Tree) ->
    intersect(Point, Tree, size(Tree), []).

intersect(_Point, _Tree, 0, Acc) -> Acc;
intersect(Point, Tree, N, Acc0) ->
    Lo = element(N-1, Tree),
    Hi = element(N, Tree),
    Acc = if Lo =< Point, Point =< Hi -> [(N div 2) - 1 | Acc0];
	     true -> Acc0
	  end,
    intersect(Point, Tree, N-2, Acc).
