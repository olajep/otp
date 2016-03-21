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
	  mon                   :: monitor(),
	  mem = 0               :: non_neg_integer(),
	  cum = 0               :: non_neg_integer()
	 }).
-type client_info() :: #client_info{}.

-record(state, {
	  clients         = #{}   :: #{pid() => client_info()},
	  aliases         = #{}   :: #{pid() => pid()},
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
				  io:format(?MSGTAG "~s ~s\e[K~n",
					    [[Tag], MsgStr]),
				  Cli
			  end, Id, State0),
    noreply_repaint(State);
handle_cast({pop_msg, Id, End, MsgStr, DefaultTime}, State0) ->
    State = update_client(
	      fun(Cli=#client_info{stack=Stack, tag=Tag, mon=Mon,
				   suspended=Suspended}) ->
		      case Stack of
			  [{_,Start}|Tl] ->
			      {MaxMem, Alloc} = monitor_pop_max(Mon),
			      Time = case Suspended of
				       true -> Start;
				       false -> End - Start
				   end;
			  Tl=[] ->
			      Time = time_from_ms(DefaultTime),
			      Alloc = MaxMem = 0
		      end,
		      io:format(?MSGTAG "~s ~s: ~s ~s ~s\e[K~n",
				[[Tag], MsgStr, format_bytes(MaxMem),
				 format_bytes(Alloc), format_time(Time)]),
		      Cli#client_info{stack=Tl}
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
				  io:format(?MSGTAG
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
    io:format("~s", [Txt]),
    noreply_repaint(State);
handle_cast(_Msg, State) ->
    noreply(State).

handle_info(refresh_timer_tick, State) ->
    noreply_repaint(State);
handle_info({mem_changed, Id, Mem, TaskCum}, State) ->
    noreply_repaint(maybe_update_client(
		      fun(Cli)->Cli#client_info{mem=Mem,cum=TaskCum}end,
		      Id, State));
handle_info({new_alias, Id, New}, State=#state{aliases=Aliases}) ->
    noreply(State#state{aliases=Aliases#{New => Id}});
handle_info({drop_alias, Id, Dead}, State=#state{aliases=Aliases})
%% We still need the identitiy aliases, even when that process is dead
  when Id =/= Dead ->
    noreply(State#state{aliases=maps:remove(Dead, Aliases)});
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

maybe_update_client(Fun, Alias, State=#state{clients=Clients0, aliases=Aliases}) ->
    Clients =
	case Aliases of
	    #{Alias := Id} ->
		#{Id := Cli} = Clients0,
		Clients0#{Id := Fun(Cli)};
	    #{} ->
		case
		    any_monitor_knows_child(Alias,
					    [Cli#client_info.mon
					     || {_,Cli} <- maps:to_list(Clients0)])
		of
		    false -> Clients0;
		    {true, Id} ->
			#{Id := Cli} = Clients0,
			Clients0#{Id := Fun(Cli)}
		end
	end,
    State#state{clients=Clients}.

update_client(Fun, Alias, State=#state{clients=Clients0, next_tag=NextTag0,
				       aliases=Aliases0}) ->
    {Clients, Aliases, NextTag} =
	case Aliases0 of
	    #{Alias := Id} ->
		#{Id := Cli} = Clients0,
		{Clients0#{Id := Fun(Cli)}, Aliases0, NextTag0};
	    #{} ->
		false = maps:is_key(Alias, Clients0),
		case
		    monitor_memory(Alias)
		of
		    {already_traced, Id} ->
			#{Id := Cli} = Clients0,
			{Clients0#{Id := Fun(Cli)},
			 Aliases0#{Alias => Id},
			 NextTag0};
		    {ok, Mon} ->
			{Clients0#{Alias => Fun(#client_info{
						   tag=NextTag0,
						   mon=Mon})},
			 Aliases0#{Alias => Alias},
			 next_tag(NextTag0)}
		end
	end,
    State#state{clients=Clients, aliases=Aliases, next_tag=NextTag}.

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
time_from_ms(MS) -> erlang:convert_time_unit(MS, milli_seconds, native).

format_time(T) -> io_lib:format("~wms", [time_to_ms(T)]).

format_bytes(B) -> io_lib:format("~wMB", [B div (1024 * 1024)]).

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
		    io:fwrite(standard_error, "\nWorkers:                        "
			      " Time CumMem CurMem Task\e[K", []),
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
				   mem=Mem,
				   cum=Cum}) ->
    S = time_to_s(get_time() - Start),
    A = io_lib:format("~7s", [format_bytes(Cum)]),
    M = case Mem of
	    0 -> "   DEAD";
	    Mem -> io_lib:format("~7s", [format_bytes(Mem)])
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
    Line = lists:flatten(io_lib:format("~30s ~s~s~s~s ~s~s~s",
				       [Name, [Tag], Time, A, M, Text, Task, P])),
    io:fwrite(standard_error, "\n~s\e[K", [lists:sublist(Line, Cols-1)]),
    1.


%% =============================================================================
%% Memory monitoring daemon
%% Tracks the current, cumulative, and maximum memory use of a process and all
%% its children.
-type monitor() :: pid().
-define(MONITOR_POLL_INTERVAL_MS, 250).
-define(WORDSIZE, erlang:system_info(wordsize)).

-record(memory_sample, {
	  total = 0   :: integer(),
	  procs = #{} :: #{pid() => {
				HeapUse::integer(), OldHeapUse::integer(),
				Heap::integer(), PBins::_,
				BinVHeap::integer()}}
	 }).

-record(mon_state, {
	  server  :: pid(),                     % Upstream timing server; the
						% process registred to ?SERVER
	  id      :: pid(),                     % Identifier for the monitor;
						% pid of root process
	  cum = 0 :: non_neg_integer(),         % Cumulative allocated heap
						% space
	  procs	  :: #{pid() => boolean()},     % Current processes, and whether
						% they are currently in GC
	  mem	  :: #memory_sample{},          % Last memory sample
	  max	  :: [{Max::non_neg_integer(),  % Stack of tasks; max lifetime
		       StartCum			% total and cum use when the
		       ::non_neg_integer()},...]% task started
	 }).

-spec monitor_memory(pid()) -> {ok, monitor()} | {already_traced, pid()}.
monitor_memory(Process) ->
    Self = self(),
    Mon = spawn_link(fun() -> monitor_entry(Self, Process) end),
    try erlang:trace(Process, true,
		     [{tracer, Mon}, set_on_spawn, procs, garbage_collection]),
	 {ok, Mon}
    catch error:badarg ->
	    stop_monitor(Mon),
	    receive
		{new_alias, Id, Process} ->
		    {already_traced, Id}
	    after 10000 -> error(timeout, [Process])
	    end
    end.

stop_monitor(Mon) ->
    Mon ! die,
    MonMonRef = monitor(process, Mon),
    receive
	{'DOWN', MonMonRef, process, Mon, _} -> ok
    after 10000 -> error(timeout, [Mon])
    end.

-spec monitor_push_max(monitor()) -> ok.
monitor_push_max(Mon) when is_pid(Mon) ->
    Mon ! push_max, ok.

-spec monitor_pop_max(monitor()) -> {non_neg_integer(), non_neg_integer()}.
monitor_pop_max(Mon) when is_pid(Mon) ->
    try monitor_call(Mon, pop_max)
    catch {monitor_down, _} -> {0, 0}
    end.

monitor_call(Mon, Call) when is_pid(Mon) ->
    Ref = monitor(process, Mon),
    Mon ! {Call, self(), Ref},
    receive
	{Ref, Answer} ->
	    demonitor(Ref, [flush]),
	    Answer;
	{'DOWN', Ref, process, Mon, Info} ->
	    throw({monitor_down, Info})
    after 10000 -> error(timeout, [Mon, Call])
    end.

-spec any_monitor_knows_child(pid(), [monitor()]) -> false | {true, pid()}.
any_monitor_knows_child(Child, Mons) ->
    Answers = monitor_multicall(Mons, knows_child, Child, fun(_) -> false end),
    any_monitor_knows_child_collect(Answers).

any_monitor_knows_child_collect([]) -> false;
any_monitor_knows_child_collect([false|Rest]) ->
    any_monitor_knows_child_collect(Rest);
any_monitor_knows_child_collect([A={true,_}|_]) -> A.

monitor_multicall(Mons, Call, CallArg, DownAction) ->
    MonRefs = maps:from_list([begin
				  Ref = monitor(process, Mon),
				  Mon ! {Call, CallArg, self(), Ref},
				  {Ref, Mon}
			      end || Mon <- Mons]),
    monitor_multicall_receive(MonRefs, DownAction).

%% If only is_key was allowed in guards... (I'm looking at you, OTP team)
-define(IS_KEY_GUARD(Key, Map), (map_size(Map) =:= map_size(Map#{Key => []}))).

monitor_multicall_receive(MonRefs, _) when map_size(MonRefs) =:= 0 -> [];
monitor_multicall_receive(MonRefs, DownAction) ->
    receive
	{Ref, Answer} when ?IS_KEY_GUARD(Ref, MonRefs) ->
	    demonitor(Ref, [flush]),
	    [Answer | monitor_multicall_receive(
			maps:remove(Ref, MonRefs), DownAction)];
	{'DOWN', Ref, process, Mon, Info} when ?IS_KEY_GUARD(Ref, MonRefs) ->
	    #{Ref := Mon} = MonRefs, %% Sanity
	    [DownAction(Info) | monitor_multicall_receive(
				  maps:remove(Ref, MonRefs), DownAction)]
     end.

monitor_entry(Server, Process) ->
    {ok, _} = timer:send_after(?MONITOR_POLL_INTERVAL_MS, timeout),
    #memory_sample{total=MemT} = Mem = monitor_get_initial(Process),
    Server ! {mem_changed, Process, MemT * ?WORDSIZE, 0},
    monitor_loop(#mon_state{server=Server, id=Process, mem=Mem, max=[{MemT, 0}],
			    procs=#{Process => false}}).

monitor_loop(#mon_state{server=Server, id=Id, procs=Procs, cum=Cum,
			mem = Mem = #memory_sample{total=MemT},
			max = [{Max,StartCum}|Maxs]=M} = State) ->
    NewState
	= #mon_state{procs=NewProcs, mem=NewMem, max=NewM=[{_,NewStartCum}|_],
		     cum=NewCum} =
	receive
	    %% Calls
	    {poll, Client, Ref} ->
		Client ! {Ref, MemT * ?WORDSIZE},
		State;
	    {pop_max, Client, Ref} ->
		{#memory_sample{total=NewT} = New, NewCum0}
		    = monitor_get(Procs, Mem, Cum),
		Max1 = max(Max, NewT),
		Client ! {Ref, {Max1 * ?WORDSIZE,
				(NewCum0-StartCum) * ?WORDSIZE}},
		[{NewMax,NewStartCum0}|NewMaxs] = Maxs,
		State#mon_state{
		  mem=New, max=[{max(NewMax, Max1),NewStartCum0}|NewMaxs],
		  cum = NewCum0};
	    {knows_child, Child, Client, Ref} ->
	    	case Procs of
	    	    #{Child := _} -> Client ! {Ref, {true, Id}};
	    	    _ -> Client ! {Ref, false}
		end,
		State;
	    %% Casts
	    push_max ->
		{#memory_sample{total=NewT} = New, NewCum0}
		    = monitor_get(Procs, Mem, Cum),
		State#mon_state{mem=New, max=[{NewT,NewCum0}|M], cum=NewCum0};
	    die -> exit(normal);
	    %% Others
	    {trace, _, spawn, New, _} ->
		Server ! {new_alias, Id, New},
		State#mon_state{procs=Procs#{New => false}};
	    {trace, Proc, exit, _} ->
		Server ! {drop_alias, Id, Proc},
		#memory_sample{procs=MProcs} = Mem,
		State#mon_state{
		  procs=maps:remove(Proc, Procs),
		  mem=Mem#memory_sample{procs=maps:remove(Proc, MProcs)}};
	    {trace, Whom, What, Arg} ->
		{NewCum1, NewProcs1, NewMem1 = #memory_sample{total=NewT}} =
		    monitor_handle_trace(Whom, What, Arg, Cum, Procs, Mem),
		State#mon_state{cum=NewCum1, procs=NewProcs1, mem=NewMem1,
				max=[{max(NewT, Max),StartCum}|Maxs]};
	    {trace, _, _, _, _} -> State; %% Ignore
	    timeout ->
		{#memory_sample{total=NewT} = New, NewCum0}
		    = monitor_get(Procs, Mem, Cum),
		{ok, _} = timer:send_after(?MONITOR_POLL_INTERVAL_MS, timeout),
		State#mon_state{mem=New, max=[{max(NewT, Max),StartCum}|Maxs],
				cum=NewCum0};
	    Unexpected ->
		error({unexpected_message, Unexpected})
	end,
    case {NewMemT = NewMem#memory_sample.total, Cum-StartCum,
	  NewCum-NewStartCum}
    of
	{MemT, Same, Same} -> ok;
	{_, _, TaskCum} ->
	    [{_, NewStartCum}|_] = NewM,
	    Server ! {mem_changed, Id, NewMemT * ?WORDSIZE, TaskCum * ?WORDSIZE}
    end,
    case {map_size(NewProcs), NewM} of
	{0, [_]} -> ok; %% Exit, nothing more to do
	_ -> monitor_loop(NewState)
    end.

monitor_get_initial(Proc) ->
    case
	try process_info(Proc, [total_heap_size, binary, heap_use])
	catch error:badarg -> process_info(Proc, [total_heap_size, binary])
	end
    of
	undefined ->
	    %% Dead already, we will either receive 'die' or a trace message
	    %% saying that Process has exited, and exit at that point
	    #memory_sample{};
	[{total_heap_size, Mem}, {binary, PBins} | Tl] ->
	    HU = case Tl of [] -> 0; [{heap_use, HU0}] -> HU0 end,
	    PBinSum = lists:sum([Size || {_,_,Size} <- PBins]) div ?WORDSIZE,
	    #memory_sample{total = Mem + PBinSum, 
			   procs=#{Proc=>{HU,0,Mem,PBins,PBinSum}}}
    end.

monitor_get(Procs, _Old = #memory_sample{procs=OldProcSamples}, OldCum) ->
    {Heaps, BinDelta, Cum, Bins, ProcSamples}
	= monitor_get_collect(maps:to_list(Procs), 0, 0, OldCum, #{},
			      OldProcSamples),
    {#memory_sample{
	total = Heaps + BinDelta +
	    lists:sum([Size || {_,_,Size} <- maps:values(Bins)]),
	procs = ProcSamples
       }, Cum}.

monitor_get_collect([], Heaps, BinDelta, Cum, Bins, PS) ->
    {Heaps, BinDelta, Cum, Bins, PS};
monitor_get_collect([{Proc,false}|Procs], Heaps0, BinDelta, Cum0, Bins0,
		    PS0) ->
    {Bins, Heaps, Cum, PS} =
	case
	    try process_info(Proc, [total_heap_size, binary, heap_use])
	    catch error:badarg -> process_info(Proc, [total_heap_size, binary])
	    end
	of
	    [{total_heap_size, Mem}, {binary, PBins} | Tl] ->
		#{Proc := {OldHU, OHU, _, _, _}} = PS0,
		HU = case Tl of [] -> OldHU; [{heap_use, HU0}] -> HU0 end,
		PBinSum = lists:sum([Size || {_,_,Size} <- PBins]),
		{lists:foldl(fun(B={Id,_,_}, Bins1) -> Bins1#{Id=>B} end,
			     Bins0, PBins),
		 Mem + Heaps0, Cum0 + (HU - OldHU),
		 PS0#{Proc=>{HU, OHU, Mem, PBins, PBinSum div ?WORDSIZE}}};
	    undefined -> {Bins0, Heaps0, Cum0, PS0}
	end,
    monitor_get_collect(Procs, Heaps, BinDelta, Cum, Bins, PS);
monitor_get_collect([{Proc,true}|Procs], Heaps0, BinDelta0, Cum, Bins0, PS) ->
    %% If we call process_info while it is garbage collecting, we will block.
    %% Reuse old info.
    #{Proc := {_, _, Mem, PBins, PBinSum}} = PS,
    OldPBinSum = (lists:sum([Size || {_,_,Size} <- PBins])div?WORDSIZE),
    monitor_get_collect(Procs, Heaps0 + Mem, BinDelta0 - OldPBinSum + PBinSum,
			Cum,
			lists:foldl(fun(B={Id,_,_}, Bins1) -> Bins1#{Id=>B} end,
				    Bins0, PBins),
			PS).

monitor_handle_trace(Proc, gc_start, Info, Cum, Procs, Mem) ->
    monitor_update_ms(Proc, true, Info, Cum, Procs, Mem);
monitor_handle_trace(Proc, gc_end, Info, Cum, Procs, Mem) ->
    monitor_update_ms(Proc, false, Info, Cum, Procs, Mem);
monitor_handle_trace(_, _, _, Cum, Procs, Mem) -> {Cum, Procs, Mem}.

monitor_update_ms(Proc,Start,Info,OldCum,Procs,
		  Mem=#memory_sample{total=OldTotal,procs=MProcs}) ->
    case MProcs of
	#{Proc := {OldHU, OldOHU, OldHeap, OldPBins, OldPBinSum}} -> ok;
	#{} ->
	    %% Just spawned, make up some values
	    OldHU = OldOHU = OldHeap = OldPBinSum = 0,
	    OldPBins = []
    end,
    #{heap_block_size := HS, old_heap_block_size := OHS, bin_vheap_size := BVS,
      mbuf_size := MBS, bin_old_vheap_size := OBVS,
      heap_size := HU, old_heap_size := OHU} = maps:from_list(Info),
    Heap = HS + OHS + MBS,
    PBinSum = BVS + OBVS,
    Cum = case Start of
	      true  -> OldCum + (HU - OldHU) + (OHU - OldOHU);
	      false -> OldCum + (OldHU - HU) + (OldOHU - OHU)
	  end,
    {Cum,
     Procs#{Proc := Start},
     Mem#memory_sample{
       total=OldTotal - OldHeap - OldPBinSum + Heap + PBinSum,
       procs=MProcs#{Proc => {HU, OHU, Heap, OldPBins, PBinSum}}}}.
