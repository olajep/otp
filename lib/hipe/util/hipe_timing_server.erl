-module(hipe_timing_server).

-behaviour(gen_server).

-export([start/0, start_link/0]).

-export([push/2, pop/1,
	 begin_task/1, begin_task/2, end_task/0,
	 suspend/1, resume/1,
	 friendly_name/1, friendly_name/2,
	 msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(client_info, {
	  stack = []           :: [{string(), integer()}],
	  task = none          :: none | string(),
	  suspended = false    :: boolean(),
	  friendly_name = none :: none | string()
	 }).
-type client_info() :: #client_info{}.

-record(state, {
	  clients = #{}  :: #{pid() => client_info()},
	  refresh_timer  :: timer:tref(),
	  last_lines = 0 :: integer(),
	  pending_repaint = false :: boolean()
	 }).

-define(REFRESH_INTERVAL, 1000).

%%%===================================================================
%%% API
%%%===================================================================

start() ->
    gen_server:start({local, ?SERVER}, ?MODULE, [], []).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

push(Id, Text) ->
    Start = get_time(),
    gen_server:cast(?SERVER, {push, Id, Text, Start}).

pop(Id) ->
    gen_server:cast(?SERVER, {pop, Id}).

begin_task({_M,F,A}) ->
    begin_task("~w/~w", [F,A]);
begin_task(Desc) ->
    begin_task("~s", [Desc]).

begin_task(Desc, {_M,F,A}) ->
    begin_task("~s ~w/~w", [Desc, F,A]);
begin_task(Msg, Args) ->
    Id = self(),
    gen_server:cast(?SERVER, {begin_task, Id, io_lib:format(Msg, Args)}).

end_task() ->
    Id = self(),
    gen_server:cast(?SERVER, {end_task, Id}).

suspend(Id) ->
    Time = get_time(),
    gen_server:cast(?SERVER, {suspend, Id, Time}).

resume(Id) ->
    Time = get_time(),
    gen_server:cast(?SERVER, {resume, Id, Time}).

friendly_name({_M,F,A}) ->
    friendly_name("~w/~w", [F,A]);
friendly_name(Desc) ->
    friendly_name("~s", [Desc]).

friendly_name(Desc, {_M,F,A}) ->
    friendly_name("~s ~w/~w", [Desc, F,A]);
friendly_name(Msg, Args) ->
    Id = self(),
    gen_server:cast(?SERVER, {friendly_name, Id, io_lib:format(Msg, Args)}).

msg(Msg, Args) ->
    case whereis(?SERVER) of
	undefined -> io:format(Msg, Args);
	_Pid -> gen_server:cast(?SERVER, {msg, io_lib:format(Msg, Args)})
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, TRef} = timer:send_interval(?REFRESH_INTERVAL, refresh_timer_tick),
    {ok, #state{refresh_timer=TRef}}.

handle_call(_Request, _From, State) ->
    reply(ok, State).

handle_cast({push, Id, Text, Start}, State0) ->
    State = update_client(fun(CI0=#client_info{stack=Stack0}) ->
				  CI0#client_info{stack=[{Text, Start}|Stack0]}
			  end, Id, State0),
    noreply_repaint(State);
handle_cast({pop, Id}, State0) ->
    State = maybe_update_client(fun(Cli=#client_info{stack=[]}) -> Cli;
				   (Cli=#client_info{stack=[_|T]}) ->
					Cli#client_info{stack=T}
				end, Id, State0),
    noreply_repaint(State);
handle_cast({begin_task, Id, Text}, State0=#state{clients=Clients0}) ->
    Clients = case Clients0 of
		  #{Id := Cli} -> Clients0#{Id := Cli#client_info{task=Text}};
		  _ -> Clients0
	      end,
    State = State0#state{clients=Clients},
    noreply_repaint(State);
handle_cast({end_task, Id}, State0=#state{clients=Clients0}) ->
    Clients = case Clients0 of
		  #{Id := Cli} -> Clients0#{Id := Cli#client_info{task=none}};
		  _ -> Clients0
	      end,
    State = State0#state{clients=Clients},
    noreply_repaint(State);
handle_cast({suspend, Id, Time}, State0) ->
    State = maybe_update_client(
	      fun(Cli=#client_info{stack=Stack0, suspended=false}) ->
		      Stack = [{N, Time - S} || {N, S} <- Stack0],
		      Cli#client_info{suspended=true, stack=Stack}
	      end, Id, State0),
    noreply_repaint(State);
handle_cast({resume, Id, Time}, State0) ->
    State = maybe_update_client(
	      fun(Cli=#client_info{stack=Stack0, suspended=true}) ->
		      Stack = [{N, Time - T} || {N, T} <- Stack0],
		      Cli#client_info{suspended=false, stack=Stack}
	      end, Id, State0),
    noreply_repaint(State);
handle_cast({friendly_name, Id, Name}, State0) ->
    State = update_client(fun(Cli) ->
				  Cli#client_info{friendly_name=Name}
			  end, Id, State0),
    noreply_repaint(State);
handle_cast({msg, Txt0}, State) ->
    Txt = re:replace(Txt0, "\n", "\n\e[K"),
    io:fwrite(standard_error, "~s", [Txt]),
    noreply_repaint(State);
handle_cast(_Msg, State) ->
    noreply(State).

handle_info(refresh_timer_tick, State) ->
    noreply_repaint(State);
handle_info(timeout, State0) ->
    State =
	case map_size(State0#state.clients) of
	    0 -> State0;
	    _ -> repaint(State0)
	end,
    noreply(State#state{pending_repaint=false});
handle_info(_Info, State) ->
    noreply(State).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_update_client(Fun, Id, State=#state{clients=Clients0}) ->
    Clients = case Clients0 of
		  #{Id := Cli} -> Clients0#{Id := Fun(Cli)};
		  _ -> Clients0
	      end,
    State#state{clients=Clients}.

update_client(Fun, Id, State=#state{clients=Clients0}) ->
    Clients = Clients0#{Id => Fun(maps:get(Id, Clients0, #client_info{}))},
    State#state{clients=Clients}.

noreply(State=#state{pending_repaint=true}) -> {noreply, State, 0};
noreply(State) -> {noreply, State}.

reply(Rep, State=#state{pending_repaint=true}) -> {reply, Rep, State, 0};
reply(Rep, State) -> {reply, Rep, State}.

noreply_repaint(State) ->
    {noreply, State#state{pending_repaint=true}, 0}.

get_time() -> erlang:monotonic_time().

time_to_s(T) -> erlang:convert_time_unit(T, native, seconds).

repaint(State = #state{clients=Clients, last_lines=LastLines}) ->
    io:fwrite(standard_error, "\nWorkers:\e[K", []),
    Lines = lists:foldl(fun({C, I}, S) ->
				paint_client(C, I) + S
			end, 0, maps:to_list(Clients)),
    Lines2 = case LastLines - Lines of
		 Pos when Pos > 0 ->
		     io:fwrite(standard_error, "~s",
			       [["\n\e[K" || _ <- lists:seq(1, Pos)]]),
		     LastLines;
		 _ -> Lines
	     end,
    io:fwrite(standard_error, "\n\e[~wA\e[K", [Lines2+2]),
    State#state{last_lines = Lines}.

paint_client(_, #client_info{suspended=true}) -> 0;
paint_client(_, #client_info{stack=[]}) -> 0;
paint_client(C, #client_info{stack=[{Text, Start}|_],
			     task=Task0,
			     friendly_name=FriendlyName}) ->
    S = time_to_s(get_time() - Start),
    Task = case Task0 of none -> ""; _ ->
		   [": ", Task0]
	   end,
    Name = case FriendlyName of
	       none -> pid_to_list(C);
	       _ -> FriendlyName
	   end,
    io:fwrite(standard_error, "\n~30s ~s: ~ws~s\e[K", [Name, Text, S, Task]),
    1.
