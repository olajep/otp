%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2015. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(epiphany).

%% This is the interface module to the epiphany server. It also contains the
%% built-in functions that are used to manage the Epiphany co-processor.

%% Built-in functions
-export([internal_spawn/3, boot/0, prepare_loading/2, module_loaded/1, host/0,
	 state/0]).

-export([load_module/2, load_module/1, spawn/3, spawn/1, spawn_monitor/3,
	 spawn_monitor/1, spawn_link/3, spawn_link/1, spawn_opt/4,
	 spawn_opt/2]).

-define(SERVER, epiphany_server).
-define(CALL_TIMEOUT, 10000).

-spec internal_spawn(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
internal_spawn(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% boot/0
-spec boot() -> ok | {error, Error} when
      Error :: disabled | offline | already_online | wait.
boot() ->
    erlang:nif_error(undefined).

%% prepare_loading/2
-spec prepare_loading(Module, Code)
			    -> PreparedCode | {error, Reason} when
      Module :: module(),
      Code :: binary(),
      PreparedCode :: binary(),
      Reason :: bad_file.
prepare_loading(_Module, _Code) ->
    erlang:nif_error(undefined).

%% module_loaded/1
-spec module_loaded(Module) -> boolean() when
      Module :: module().
module_loaded(_Module) ->
    erlang:nif_error(undefined).

%% host/0
-spec host() -> master | slave.
host() ->
    erlang:nif_error(undefined).

%% state/0
-spec state() -> offline | booting | online | unavailable.
state() ->
    erlang:nif_error(undefined).

%% load_module/2
-spec load_module(Module, Binary) -> {module, Module} | {error, Reason} when
      Module :: module(),
      Binary :: binary(),
      Reason :: badfile | not_purged | on_load.
load_module(Mod, Code) ->
    case ?MODULE:prepare_loading(Mod, Code) of
	{error,_}=Error ->
	    Error;
	Bin when is_binary(Bin) ->
	    case erlang:finish_loading([Bin]) of
		ok ->
		    {module,Mod};
		{Error,[Mod]} ->
		    {error,Error}
	    end
    end.

%% load_module/1
-spec load_module(Module) -> {module, Module} | {error, Reason} when
      Module :: module(),
      Reason :: badfile | not_purged | on_load | nofile.
load_module(Mod) ->
    case code:where_is_file(atom_to_list(Mod) ++ ".beam") of
	non_existing -> {error, nofile};
	File ->
	    case erl_prim_loader:get_file(File) of
		error -> {error, badfile};
		{ok, Bin, _FullName} ->
		    ?MODULE:load_module(Mod, Bin)
	    end
    end.

-spec spawn(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn(Module, Function, Args) ->
    ok = call(wait_ready),
    ?MODULE:internal_spawn(Module, Function, Args).

-spec spawn_monitor(Module, Function, Args) -> {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_monitor(Module, Function, Args) ->
    ?MODULE:spawn_opt(Module, Function, Args, [monitor]).

-spec spawn_link(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn_link(Module, Function, Args) ->
    ?MODULE:spawn_opt(Module, Function, Args, [link]).

-spec spawn_opt(Module, Function, Args, Options)
	       -> pid() | {pid(), reference()} when
      Module :: module(),
      Function :: atom(),
      Args :: [term()],
      Options :: [Option],
      Option :: link | monitor
	      | {priority, Level :: erlang:priority_level()}
	      | {fullsweep_after, Number :: non_neg_integer()}
	      | {min_heap_size, Size :: non_neg_integer()}
	      | {min_bin_vheap_size, VSize :: non_neg_integer()}.
spawn_opt(Module, Function, Args, Options) ->
    ok = call(wait_ready),
    erlang:spawn_opt(Module, Function, Args, [epiphany|Options]).

%% Spawns with a fun
-spec spawn(Fun) -> pid() when
      Fun :: function().
spawn(F) when is_function(F) ->
    ?MODULE:spawn(erlang, apply, [F, []]);
spawn({M,F}=MF) when is_atom(M), is_atom(F) ->
    ?MODULE:spawn(erlang, apply, [MF, []]);
spawn(F) ->
    error(badarg, [F]).

-spec spawn_link(Fun) -> pid() when
      Fun :: function().
spawn_link(F) when is_function(F) ->
    ?MODULE:spawn_link(erlang, apply, [F, []]);
spawn_link({M,F}=MF) when is_atom(M), is_atom(F) ->
    ?MODULE:spawn_link(erlang, apply, [MF, []]);
spawn_link(F) ->
    error(badarg, [F]).

-spec spawn_monitor(Fun) -> pid() when
      Fun :: function().
spawn_monitor(F) when is_function(F) ->
    ?MODULE:spawn_monitor(erlang, apply, [F, []]);
spawn_monitor({M,F}=MF) when is_atom(M), is_atom(F) ->
    ?MODULE:spawn_monitor(erlang, apply, [MF, []]);
spawn_monitor(F) ->
    error(badarg, [F]).

-spec spawn_opt(Fun, Options) -> pid() | {pid(), reference()} when
      Fun :: function(),
      Options :: [Option],
      Option :: link | monitor
	      | {priority, Level :: erlang:priority_level()}
              | {fullsweep_after, Number :: non_neg_integer()}
              | {min_heap_size, Size :: non_neg_integer()}
              | {min_bin_vheap_size, VSize :: non_neg_integer()}.
spawn_opt(F, O) when is_function(F) ->
    ?MODULE:spawn_opt(erlang, apply, [F, []], O);
spawn_opt({M,F}=MF, O) when is_atom(M), is_atom(F) ->
    ?MODULE:spawn_opt(erlang, apply, [MF, []], O);
spawn_opt(F, O) ->
    error(badarg, [F, O]).

-spec call(tuple()) -> term().
call(Cmd) ->
    %% We short-circuit here if the slave is offline, since the call will
    %% timeout in those cases.
    case ?MODULE:state() of
	offline -> error(offline);
	unavailable -> error(notsup);
	_ -> ?SERVER:call(Cmd)
    end.
