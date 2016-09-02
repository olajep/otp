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

-export([split/3, combine_spills/2]).
-export_type([spill_grouping/0]).

-include("../main/hipe.hrl").

-opaque spill_grouping() :: [].
-type cfg()              :: any().
-type liveness()         :: any().
-type target()           :: module().


%% Tasks:
%%  * DSETS partitioning of program space at clobber points.
%%    Partitions: {entry, Lb} | {exit, Lb} | {internal, Lb, No}
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
%%
%%
%% Q: Do we need a partition refinement data structure? No, it does not do quite
%% what we need. We want something akin to a graph, where we delete edges (by
%% inserting splits) to partition the space. Can 'sofs' do that?

-spec split(cfg(), liveness(), target()) -> {cfg(), spill_grouping()}.
split(_CFG, _Liveness, _Target) ->
    %%error(notimpl).
    {_CFG, []}.

-spec combine_spills(hipe_map(), spill_grouping()) -> hipe_map().
combine_spills(_Alloc, _Grouping) ->
    %%error(notimpl).
    _Alloc.
