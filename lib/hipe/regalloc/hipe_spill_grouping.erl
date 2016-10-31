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
%%	                 GROUPING OF TEMPORARIES
%%
%% Range splitting introduces multiple temporaries that should be spilled to the
%% same spill slot.
%%
%% This module defines a conversion from a easy-to-use data structure for
%% temporary groupings (grouping_list()), to an opaque data structure
%% (grouping()) that can be used to efficiently combine the allocated spills
%% slots of all temps in the same group.
-module(hipe_spill_grouping).

-export([new/0, add/2, combine_spills/2]).
-export_type([grouping_list/0, grouping/0]).

-include("../main/hipe.hrl").

-type temp() :: non_neg_integer().
%% Invariant: Substitute > Base (Substitute is newer than Base, and was
%% introduced to hold the value of Base in some particular interval)
-type grouping_list() :: [{Substitute::temp(),Base::temp()}].
-opaque grouping() :: #{temp() => temp()}. % member => witness (like dset)

-spec new() -> grouping().
new() -> #{}.

%% @doc Adds new groupings to a spill grouping. Grouping lists must be added in
%% the order they are created.
-spec add(grouping_list(), grouping()) -> grouping().
add([{New,Base0}|List], Grouping0) when New > Base0 ->
  Grouping =
    case Grouping0 of
      #{New := _} -> error(badarg); % assertion
      #{Base0 := Base} ->
	Grouping0#{New => Base};
      #{} ->
	Grouping0#{Base0 => Base0, New => Base0}
    end,
  add(List, Grouping);
add([], Grouping) ->
  Grouping.

-spec combine_spills(hipe_map(), grouping()) -> hipe_map().
combine_spills(Alloc, Grouping) when map_size(Grouping) =:= 0 -> Alloc;
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
