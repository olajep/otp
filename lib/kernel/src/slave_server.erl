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
-module(slave_server).

%%%-------------------------------------------------------------------
%%% Author  : Magnus Lång <margnus1@telia.com>
%%% Purpose : Boots the slave emulator and moderates process creation
%%%           on it.
%%% Created : 5 May 2015 by Magnus Lång <margnus1@telia.com>
%%%-------------------------------------------------------------------

-behaviour(gen_server).

%% API
-export([start_link/0, call/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TIMEOUT, 10000).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    %% We don't use gen_server:start_link because we want to initialise
    %% asynchronously (indeed, that is the reason slave_server exists in the
    %% first place; so that the slave emulator can be booted asynchronously
    %% without races to slave:spawn/3).
    proc_lib:start_link(?MODULE, init, [[]]).

-spec call(tuple()) -> term().
call(Cmd) ->
    gen_server:call(?SERVER, Cmd, ?TIMEOUT).

%%%===================================================================
%%% Initialisation
%%%===================================================================

-spec init([]) -> {ok, #state{}} | {stop, term()}.
init([]) ->
    case slave:state() of
	booting ->
	    register(?SERVER, self()),
	    proc_lib:init_ack({ok, self()}),
	    init_loop();
	online ->
	    register(?SERVER, self()),
	    proc_lib:init_ack({ok, self()}),
	    init_loop_2(prerequisite_modules());
	offline ->
	    proc_lib:init_ack(ignore)
    end.

-spec init_loop() -> {ok, #state{}} | {stop, term()}.
init_loop() ->
    case slave:boot() of
	ok ->
	    init_loop_2(prerequisite_modules());
	{error, wait} ->
	    timer:sleep(100),
	    init_loop();
	offline ->
	    init_stop(normal)
    end.

-spec init_loop_2([module()]) -> {ok, #state{}} | {stop, term()}.
init_loop_2([]) ->
    init_finish();
init_loop_2([Mod|Mods]) ->
    case code:ensure_loaded_slave(Mod) of
	{module, Mod} ->
	    init_loop_2(Mods);
	{error, What} ->
	    init_stop({failed_to_load, Mod, What})
    end.

-spec init_finish() -> no_return().
init_finish() ->
    State = #state{},
    gen_server:enter_loop(?MODULE, [], State).

-spec init_stop(term()) -> no_return().
init_stop(Reason) ->
    terminate(Reason, #state{}),
    exit(Reason).

%% @doc The set of modules that we require to be loaded (in addition to the
%% preloaded modules, which are already loaded by slave:boot()) before any
%% processes are spawn on the slaves.
-spec prerequisite_modules() -> [module()].
prerequisite_modules() ->
    [code, code_server, error_handler].

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec handle_call(term(), term(), #state{}) -> {reply, Reply, #state{}} when
      Reply :: ok | pid().
handle_call({spawn, M, F, A}, _From, State) ->
    {reply, slave:internal_spawn(M,F,A), State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

-spec handle_cast(term(), #state{}) -> {noreply, #state{}}.
handle_cast(_Msg, State) ->
    {noreply, State}.

-spec handle_info(term(), #state{}) -> {noreply, #state{}}.
handle_info(_Info, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
