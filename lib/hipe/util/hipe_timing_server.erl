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
	  mem = 0               :: non_neg_integer()
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
			  [{_,Start}|T] ->
			      MB = monitor_pop_max(Mon) div (1024*1024),
			      MS = case Suspended of
				       true -> Start;
				       false -> time_to_ms(End - Start)
				   end;
			  T=[] -> MS = DefaultTime, MB = 0
		      end,
		      io:format(?MSGTAG "~s ~s: ~wMB ~wms\e[K~n",
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
handle_info({mem_changed, Id, Mem}, State) ->
    noreply_repaint(maybe_update_client(fun(Cli)->Cli#client_info{mem=Mem}end,
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
				   mem=Mem}) ->
    S = time_to_s(get_time() - Start),
    M = case Mem of
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
%% Tracks the current and maximum memory use of a process and all its children.
-type monitor() :: pid().
-define(MONITOR_POLL_INTERVAL_MS, 250).
-define(WORDSIZE, erlang:system_info(wordsize)).
-record(memory_sample, {
	  total = 0   :: integer(),
	  procs = #{} :: #{pid() => {Heap::integer(), PBins::_,
				     BinVHeap::integer()}}
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

-spec monitor_pop_max(monitor()) -> non_neg_integer().
monitor_pop_max(Mon) when is_pid(Mon) ->
    try monitor_call(Mon, pop_max)
    catch {monitor_down, _} -> 0
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
	    [Answer | monitor_multicall_receive(maps:remove(Ref, MonRefs), DownAction)];
	{'DOWN', Ref, process, Mon, Info} when ?IS_KEY_GUARD(Ref, MonRefs) ->
	    #{Ref := Mon} = MonRefs, %% Sanity
	    [DownAction(Info) | monitor_multicall_receive(maps:remove(Ref, MonRefs), DownAction)]
     end.

monitor_entry(Server, Process) ->
    {ok, _} = timer:send_after(?MONITOR_POLL_INTERVAL_MS, timeout),
    #memory_sample{total=MemT} = Mem = monitor_get_initial(Process),
    Server ! {mem_changed, Process, MemT * ?WORDSIZE},
    monitor_loop(Server, Process, #{Process => false}, Mem, [MemT]).

monitor_loop(Server, Id, Procs, Mem = #memory_sample{total=MemT}, [Max|Maxs]=M) ->
    {NewProcs, NewMem, NewM} =
	receive
	    %% Calls
	    {poll, Client, Ref} ->
		Client ! {Ref, MemT * ?WORDSIZE},
		{Procs, Mem, M};
	    {pop_max, Client, Ref} ->
		#memory_sample{total=NewT} = New = monitor_get(Procs, Mem),
		Max1 = max(Max, NewT),
		Client ! {Ref, Max1 * ?WORDSIZE},
		[NewMax|NewMaxs] = Maxs,
		{Procs, New, [max(NewMax, Max1)|NewMaxs]};
	    {knows_child, Child, Client, Ref} ->
	    	case Procs of
	    	    #{Child := _} -> Client ! {Ref, {true, Id}};
	    	    _ -> Client ! {Ref, false}
	    	end,
	    	{Procs, Mem, M};
	    %% Casts
	    push_max ->
		#memory_sample{total=NewT} = New = monitor_get(Procs, Mem),
		{Procs, New, [NewT|M]};
	    die -> exit(normal);
	    %% Others
	    {trace, _, spawn, New, _} ->
		Server ! {new_alias, Id, New},
		{Procs#{New => false}, Mem, M};
	    {trace, Proc, exit, _} ->
		Server ! {drop_alias, Id, Proc},
		#memory_sample{procs=MProcs} = Mem,
		{maps:remove(Proc, Procs),
		 Mem#memory_sample{procs=maps:remove(Proc, MProcs)},
		 M};
	    {trace, Whom, What, Arg} ->
		{NewProcs1, NewMem1 = #memory_sample{total=NewT}} =
		    monitor_handle_trace(Whom, What, Arg, Procs, Mem),
		{NewProcs1, NewMem1, [max(NewT, Max)|Maxs]};
	    {trace, _, _, _, _} -> {Procs, Mem, M}; %% Ignore
	    timeout ->
		#memory_sample{total=NewT} = New = monitor_get(Procs, Mem),
		{ok, _} = timer:send_after(?MONITOR_POLL_INTERVAL_MS, timeout),
		{Procs, New, [max(NewT, Max)|Maxs]};
	    Unexpected ->
		error({unexpected_message, Unexpected})
	end,
    case NewMemT = NewMem#memory_sample.total of
	MemT -> ok;
	_ -> Server ! {mem_changed, Id, NewMemT * ?WORDSIZE}
    end,
    case {map_size(NewProcs), NewM} of
	{0, [_]} -> ok; %% Exit, nothing more to do
	_ -> monitor_loop(Server, Id, NewProcs, NewMem, NewM)
    end.

monitor_get_initial(Proc) ->
    case process_info(Proc, [total_heap_size, binary]) of
	undefined ->
	    %% Dead already, we will either receive 'die' or a trace message
	    %% saying that Process has exited, and exit at that point
	    #memory_sample{};
	[{total_heap_size, Mem}, {binary, PBins}] ->
	    PBinSum = lists:sum([Size || {_,_,Size} <- PBins]) div ?WORDSIZE,
	    #memory_sample{total = Mem + PBinSum, 
			   procs=#{Proc=>{Mem,PBins,PBinSum}}}
    end.

monitor_get(Procs, _Old = #memory_sample{procs=OldProcSamples}) ->
    {Heaps, BinDelta, Bins, ProcSamples}
	= monitor_get_collect(maps:to_list(Procs), 0, 0, #{}, OldProcSamples),
    #memory_sample{
       total = Heaps + BinDelta +
	   lists:sum([Size || {_,_,Size} <- maps:values(Bins)]),
       procs = ProcSamples
      }.

monitor_get_collect([], Heaps, BinDelta, Bins, PS) -> {Heaps, BinDelta, Bins, PS};
monitor_get_collect([{Proc, true} | Procs], Heaps0, BinDelta0, Bins0, PS) ->
    %% If we call process_info while it is garbage collecting, we will block.
    %% Reuse old info.
    #{Proc := {Mem, PBins, PBinSum}} = PS,
    OldPBinSum = (lists:sum([Size || {_,_,Size} <- PBins])div?WORDSIZE),
    monitor_get_collect(Procs, Heaps0 + Mem, BinDelta0 - OldPBinSum + PBinSum,
			lists:foldl(fun(B={Id,_,_}, Bins1) -> Bins1#{Id=>B} end,
				    Bins0, PBins),
			PS);
monitor_get_collect([{Proc, false} | Procs], Heaps0, BinDelta, Bins0, PS0) ->
    {Bins, Heaps, PS} =
	case process_info(Proc, [total_heap_size, binary]) of
	    [{total_heap_size, Mem}, {binary, PBins}] ->
		PBinSum = lists:sum([Size || {_,_,Size} <- PBins]),
		{lists:foldl(fun(B={Id,_,_}, Bins1) -> Bins1#{Id=>B} end,
			     Bins0, PBins),
		 Mem + Heaps0, PS0#{Proc=>{Mem,PBins,PBinSum div?WORDSIZE}}};
	    undefined -> {Bins0, Heaps0, PS0}
	    end,
    monitor_get_collect(Procs, Heaps, BinDelta, Bins, PS).

monitor_handle_trace(Proc, gc_start, Info, Procs, Mem) ->
    {Procs#{Proc := true}, monitor_update_ms(Proc, Info, Mem)};
monitor_handle_trace(Proc, gc_end, Info, Procs, Mem) ->
    {Procs#{Proc := false}, monitor_update_ms(Proc, Info, Mem)};
monitor_handle_trace(_, _, _, Procs, Mem) -> {Procs, Mem}.

monitor_update_ms(Proc,Info,Mem=#memory_sample{total=OldTotal,procs=MProcs}) ->
    case MProcs of
	#{Proc := {OldHeap, OldPBins, OldPBinSum}} -> ok;
	#{} ->
	    %% Just spawned, make up some values
	    OldHeap = OldPBinSum = 0,
	    OldPBins = []
    end,
    #{heap_block_size := HS, old_heap_block_size := OHS, bin_vheap_size := BVS,
      mbuf_size := MBS, bin_old_vheap_size := OBVS} = maps:from_list(Info),
    Heap = HS + OHS + MBS,
    PBinSum = BVS + OBVS,
    Mem#memory_sample{total=OldTotal - OldHeap - OldPBinSum + Heap + PBinSum,
		      procs=MProcs#{Proc => {Heap, OldPBins, PBinSum}}}.
