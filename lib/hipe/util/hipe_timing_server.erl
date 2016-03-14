-module(hipe_timing_server).

-behaviour(gen_server).

-export([start/0, start_link/0]).

-export([push/2, pop/1, pop_msg/4, client_msg/3,
	 begin_task/1, begin_task/2, end_task/0,
	 suspend/1, resume/1,
	 friendly_name/1, friendly_name/2,
	 start_progress/1, inc_progress/1, inc_total/1, end_progress/0,
	 msg/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(client_info, {
	  stack         = []    :: [{string(), integer()}],
	  task          = none  :: none | string(),
	  suspended     = false :: boolean(),
	  friendly_name = none  :: none | string(),
	  progress              :: undefined | {integer(), integer()},
	  tag                   :: char(),
	  mon                   :: monitor()
	 }).
-type client_info() :: #client_info{}.

-record(state, {
	  clients         = #{}   :: #{pid() => client_info()},
	  refresh_timer           :: timer:tref(),
	  last_lines      = 0     :: integer(),
	  pending_repaint = false :: boolean(),
	  next_tag        = $A    :: char()
	 }).

-include("../main/hipe.hrl").

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

client_msg(Id, Msg, Args) ->
    MsgStr = io_lib:format(Msg, Args),
    case whereis(?SERVER) of
	undefined -> io:format(?MSGTAG "~s~n", [MsgStr]);
	_Pid -> gen_server:cast(?SERVER, {client_msg, Id, MsgStr})
    end.

pop_msg(Id, Msg, Args, DefaultTime) ->
    MsgStr = io_lib:format(Msg, Args),
    End = get_time(),
    case whereis(?SERVER) of
	undefined -> io:format(?MSGTAG "~s: ~wms~n", [MsgStr, DefaultTime]);
	_Pid -> gen_server:cast(?SERVER, {pop_msg, Id, End, MsgStr,
					  DefaultTime})
    end.

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

start_progress(Amount) ->
    Id = self(),
    gen_server:cast(?SERVER, {start_progress, Id, Amount}).

inc_progress(Amount) ->
    Id = self(),
    gen_server:cast(?SERVER, {inc_progress, Id, Amount}).

inc_total(Amount) ->
    Id = self(),
    gen_server:cast(?SERVER, {inc_total, Id, Amount}).

end_progress() ->
    Id = self(),
    gen_server:cast(?SERVER, {end_progress, Id}).

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
    State = update_client(fun(CI0=#client_info{stack=Stack0, mon=Mon}) ->
				  monitor_push_max(Mon),
				  CI0#client_info{stack=[{Text, Start}|Stack0]}
			  end, Id, State0),
    noreply_repaint(State);
handle_cast({client_msg, Id, MsgStr}, State0) ->
    State = update_client(fun(Cli=#client_info{tag=Tag}) ->
				  io:fwrite(standard_error,
					    ?MSGTAG "~s ~s\e[K~n",
					    [[Tag], MsgStr]),
				  Cli
			  end, Id, State0),
    noreply_repaint(State);
handle_cast({pop_msg, Id, End, MsgStr, DefaultTime}, State0) ->
    State = update_client(
	      fun(Cli=#client_info{stack=Stack, tag=Tag, mon=Mon,
				   suspended=Suspended}) ->
		      case Stack of
			  [{_,Start}|T] ->
			      MB = monitor_pop_max(Mon) div (1024*1024),
			      MS = case Suspended of
				       true -> Start;
				       false -> time_to_ms(End - Start)
				   end;
			  T=[] -> MS = DefaultTime, MB = 0
		      end,
		      io:fwrite(standard_error, ?MSGTAG "~s ~s: ~wMB ~wms\e[K~n",
				[[Tag], MsgStr, MB, MS]),
		      Cli#client_info{stack=T}
	      end, Id, State0),
    noreply_repaint(State);
handle_cast({pop, Id}, State0) ->
    State = maybe_update_client(fun(Cli=#client_info{stack=[]}) -> Cli;
				   (Cli=#client_info{stack=[_|T],mon=Mon}) ->
					monitor_pop_max(Mon),
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
    State = update_client(fun(Cli = #client_info{tag=Tag}) ->
				  io:fwrite(standard_error, ?MSGTAG
					    "~s worker friendly name: ~s\e[K~n",
					    [[Tag], Name]),
				  Cli#client_info{friendly_name=Name}
			  end, Id, State0),
    noreply_repaint(State);
handle_cast({start_progress, Id, Total}, State0) ->
    State = maybe_update_client(
	      fun(Cli=#client_info{}) ->
		      Cli#client_info{progress={0, Total}}
	      end, Id, State0),
    noreply_repaint(State);
handle_cast({inc_progress, Id, Amount}, State0) ->
    State = maybe_update_client(
	      fun(Cli=#client_info{progress={Now, Total}}) ->
		      Cli#client_info{progress={Now + Amount, Total}};
		 (Cli) -> Cli#client_info{progress={Amount, 0}}
	      end, Id, State0),
    noreply_repaint(State);
handle_cast({inc_total, Id, Amount}, State0) ->
    State = maybe_update_client(
	      fun(Cli=#client_info{progress={Now, Total}}) ->
		      Cli#client_info{progress={Now, Total + Amount}};
		 (Cli) -> Cli#client_info{progress={0, Amount}}
	      end, Id, State0),
    noreply_repaint(State);
handle_cast({end_progress, Id}, State0) ->
    State = maybe_update_client(
	      fun(Cli=#client_info{}) ->
		      Cli#client_info{progress=undefined}
	      end, Id, State0),
    noreply_repaint(State);
handle_cast({msg, Txt0}, State) ->
    Txt = re:replace(Txt0, "\n", "\e[K\n"),
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

update_client(Fun, Id, State=#state{clients=Clients0, next_tag=NextTag0}) ->
    {Clients, NextTag} =
	case Clients0 of
	    #{Id := Cli} -> {Clients0#{Id := Fun(Cli)}, NextTag0};
	    _ -> {Clients0#{Id => Fun(#client_info{tag=NextTag0,
						   mon=monitor_memory(Id)})},
		  next_tag(NextTag0)}
	end,
    State#state{clients=Clients, next_tag=NextTag}.

next_tag($Z) -> $a;
next_tag($z) -> $0;
next_tag($9) -> $A;
next_tag(Tag) -> Tag + 1.

noreply(State=#state{pending_repaint=true}) -> {noreply, State, 0};
noreply(State) -> {noreply, State}.

reply(Rep, State=#state{pending_repaint=true}) -> {reply, Rep, State, 0};
reply(Rep, State) -> {reply, Rep, State}.

noreply_repaint(State) ->
    {noreply, State#state{pending_repaint=true}, 0}.

get_time() -> erlang:monotonic_time().

time_to_s(T) -> erlang:convert_time_unit(T, native, seconds).
time_to_ms(T) -> erlang:convert_time_unit(T, native, milli_seconds).

repaint(State = #state{clients=Clients0, last_lines=LastLines}) ->
    case lists:filter(fun is_to_be_painted/1, maps:to_list(Clients0)) of
	[] ->
	    case LastLines of
		Pos when Pos > 0 ->
		    io:fwrite(standard_error, "~s",
			      [["\n\e[K" || _ <- lists:seq(1, Pos)]]),
		    io:fwrite(standard_error, "\e[~wA\e[K", [Pos]);
		_ -> ok
	    end,
	    State#state{last_lines=0};
	Clients ->
	    case io:columns(standard_error) of
		{ok, Cols} ->
		    io:fwrite(standard_error, "\nWorkers:\e[K", []),
		    Lines = lists:foldl(fun({C, I}, S) ->
						paint_client(Cols, C, I) + S
					end, 0, Clients),
		    Lines2 = case LastLines - Lines of
				 Pos when Pos > 0 ->
				     io:fwrite(standard_error, "~s",
					       [["\n\e[K"
						 || _ <- lists:seq(1, Pos)]]),
				     LastLines;
				 _ -> Lines
			     end,
		    io:fwrite(standard_error, "\n\e[~wA\e[K", [Lines2+2]),
		    State#state{last_lines = Lines};
		_ -> State
	    end
    end.

is_to_be_painted({_, #client_info{stack=[]}}) -> false;
is_to_be_painted({_, #client_info{suspended=true}}) -> false;
is_to_be_painted({_, #client_info{}}) -> true.

%% paint_client(Cols, C, #client_info{suspended=true, mon=Mon,
%% 				   friendly_name=FriendlyName}) ->
%%     Mem = monitor_poll(Mon),
%%     M = Mem div 1000000,
%%     Name = case FriendlyName of
%% 	       none -> pid_to_list(C);
%% 	       _ -> FriendlyName
%% 	   end,
%%     Line = lists:flatten(io_lib:format("~30s SUSP~5wMB", [Name, M])),
%%     io:fwrite(standard_error, "\n~s\e[K", [lists:sublist(Line, Cols-1)]),
%%     1;
paint_client(Cols, C, #client_info{stack=[{Text, Start}|_],
				   suspended=Suspended,
				   task=Task0,
				   friendly_name=FriendlyName,
				   progress=Progress,
				   tag=Tag,
				   mon=Mon}) ->
    S = time_to_s(get_time() - Start),
    M = case monitor_poll(Mon) of
	    0 -> "   DEAD";
	    Mem -> io_lib:format("~5wMB", [Mem div (1024*1024)])
	end,
    Task = case Task0 of none -> ""; _ ->
		   [": ", Task0]
	   end,
    Name = case FriendlyName of
	       none -> pid_to_list(C);
	       _ -> FriendlyName
	   end,
    P = case Progress of undefined -> "";
	    {Now, Total} -> io_lib:format(": ~w/~w", [Now, Total])
	end,
    Time = case Suspended of true -> " SUSP"; false ->
		   io_lib:format("~4ws", [S])
	   end,
    Line = lists:flatten(io_lib:format("~30s ~s~s~s ~s~s~s",
				       [Name, [Tag], Time, M, Text, Task, P])),
    io:fwrite(standard_error, "\n~s\e[K", [lists:sublist(Line, Cols-1)]),
    1.


%% =============================================================================
%% Memory monitoring daemon
-type monitor() :: pid().
-define(MONITOR_POLL_INTERVAL_MS, 1).

-spec monitor_memory(pid()) -> monitor().
monitor_memory(Process) ->
    spawn_link(fun() -> monitor_entry(Process) end).

-spec monitor_poll(monitor()) -> non_neg_integer().
monitor_poll(Mon) when is_pid(Mon) ->
    monitor_call(Mon, poll).

-spec monitor_push_max(monitor()) -> ok.
monitor_push_max(Mon) when is_pid(Mon) ->
    Mon ! push_max, ok.

-spec monitor_pop_max(monitor()) -> non_neg_integer().
monitor_pop_max(Mon) when is_pid(Mon) ->
    monitor_call(Mon, pop_max).

monitor_call(Mon, Call) when is_pid(Mon) ->
    Ref = monitor(process, Mon),
    Mon ! {Call, self(), Ref},
    receive
	{Ref, Answer} ->
	    demonitor(Ref, [flush]),
	    Answer;
	{'DOWN', Ref, process, Mon, Info} ->
	    error({monitor_died, Info})
    after 10000 -> error(timeout, [Mon, Call])
    end.

monitor_entry(Process) ->
    erlang:trace(Process, true, [set_on_spawn, procs]),
    {ok, _} = timer:send_after(?MONITOR_POLL_INTERVAL_MS, timeout),
    Mem = monitor_get([Process]),
    monitor_loop([Process], Mem, [Mem]).

monitor_loop(Procs, Mem, [Max|Maxs]=M) ->
    receive
	{poll, Client, Ref} ->
	    Client ! {Ref, Mem},
	    monitor_loop(Procs, Mem, M);
	{pop_max, Client, Ref} ->
	    Client ! {Ref, Max},
	    [NewMax|NewMaxs] = Maxs,
	    monitor_loop(Procs, Mem, [max(NewMax, Mem)|NewMaxs]);
	push_max ->
	    New = monitor_get(Procs),
	    monitor_loop(Procs, New, [New|M]);
	{trace, Whom, What, Arg} ->
	    monitor_loop(monitor_handle_trace(Procs, Whom, What, Arg), Mem, M);
	{trace, Whom, What, Arg1, Arg2} ->
	    monitor_loop(monitor_handle_trace(Procs, Whom, What, Arg1, Arg2),
			 Mem, M);
	timeout ->
	    New = monitor_get(Procs),
	    {ok, _} = timer:send_after(?MONITOR_POLL_INTERVAL_MS, timeout),
	    monitor_loop(Procs, New, [max(New, Max)|Maxs]);
	Unexpected ->
	    error({unexpected_message, Unexpected})
    end.

monitor_get(Procs) ->
    {Heaps, Bins} = monitor_get_collect(Procs, 0, #{}),
    Heaps + lists:sum([Size || {_,_,Size} <- maps:values(Bins)]).

monitor_get_collect([], Heaps, Bins) -> {Heaps, Bins};
monitor_get_collect([Proc | Procs], Heaps0, Bins0) ->
    Heaps = case process_info(Proc, memory) of
		{memory, Mem} -> Mem + Heaps0;
		_ -> Heaps0
	    end,
    Bins = case process_info(Proc, binary) of
	       {binary, PBins} ->
		   lists:foldl(fun(B={Id,_,_}, Bins1) -> Bins1#{Id=>B} end,
			       Bins0, PBins);
	       _ -> Bins0
	   end,
    monitor_get_collect(Procs, Heaps, Bins).

monitor_handle_trace(Procs, Proc, exit, _) -> Procs -- [Proc];
monitor_handle_trace(Procs, _, _, _) -> Procs.

monitor_handle_trace(Procs, _, spawn, New, _) -> [New|Procs];
monitor_handle_trace(Procs, _, _, _, _) -> Procs.
