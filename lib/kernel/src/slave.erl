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
-module(slave).

%% This is the interface module to the slave server. It also contains the
%% built-in functions that are used to manage the

%% Built-in functions
-export([internal_spawn/3, print/1, boot/0, prepare_loading/2, module_loaded/1,
	 host/0, state/0]).

-export([load_module/2, load_module/1, spawn/3]).

-define(SERVER, slave_server).
-define(CALL_TIMEOUT, 10000).

%% spawn/3
-spec internal_spawn(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
internal_spawn(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% print/1
-spec print(term()) -> ok.
print(_Term) ->
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
    case slave:prepare_loading(Mod, Code) of
	{error,_}=Error ->
	    Error;
	Bin when erlang:is_binary(Bin) ->
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
		    slave:load_module(Mod, Bin)
	    end
    end.

-spec spawn(Module, Function, Args) -> pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn(Module, Function, Args) ->
    call({spawn, Module, Function, Args}).

-spec call(tuple()) -> term().
call(Cmd) ->
    %% We short-circuit here if the slave is offline, since the call will
    %% timeout in those cases.
    case slave:state() of
	offline -> error(offline);
	unavailable -> error(notsup);
	_ -> slave_server:call(Cmd)
    end.
