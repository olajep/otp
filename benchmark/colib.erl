-module(colib).

-export([pmap/2, pmap/3, %% pforeach/2,
	 spawn_and_count/0,
	 poll_stats/0, select_timers/2]).

pmap(Fun, List) ->
    Collector = self(),
    Indices = lists:seq(0, length(List)-1),
    Jobs = lists:zip(Indices, List),
    Work = fun({Ix, El}) -> Collector ! {Ix, Fun(El)} end,
    spawn_link(fun() -> pforeach(Work, Jobs) end),
    [receive {Ix, El} -> El end || Ix <- Indices].

pmap({M, F}, [], List) ->  pmap(fun M:F/1, List);
pmap({M, F}, [A], List) -> pmap(fun (E) -> M:F(E, A) end, List);
pmap({M, F}, Extra, List) ->
    pmap(fun(E) -> erlang:apply(M, F, [E|Extra]) end, List);
pmap(Fun, [], List) ->  pmap(Fun, List);
pmap(Fun, [A], List) -> pmap(fun (E) -> Fun(E, A) end, List);
pmap(Fun, Extra, List) ->
    pmap(fun(E) -> erlang:apply(Fun, [E|Extra]) end, List).

%% Does not wait for completion
pforeach(Fun, Jobs) ->
    Self = self(),
    {Spawn, Count} = spawn_and_count(),
    Workers = [Spawn(fun()-> worker(Fun, Self) end)
	       || _ <- lists:seq(1,Count)],
    workserver(Workers, Jobs).

workserver(Workers, [Job|Jobs]) ->
    receive {get_job, Worker} ->
	    Worker ! {job, Job},
	    workserver(Workers, Jobs)
    end;
workserver(Workers, []) ->
    lists:foreach(fun(W)-> W ! stop end, Workers).

worker(Fun, Jobserver) ->
    Jobserver ! {get_job, self()},
    receive
	stop -> ok;
	{job, Job} ->
	    Fun(Job),
	    worker(Fun, Jobserver)
    end.

spawn_and_count() ->
    case try epiphany:state() catch error:undef -> offline end of
	offline ->
	    {fun regular_spawn_link/1,
	     erlang:system_info(schedulers_online)};
	booting ->
	    timer:sleep(100),
	    spawn_and_count();
	_ ->
	    start_server(),
	    {fun epiphany_spawn_link/1, epiphany:count()}
    end.

start_server() ->
    case whereis(colib_server) of
	undefined ->
	    {Self, ReportRef} = {self(), make_ref()},
	    {Pid, MonRef} = spawn_monitor(
			       fun() -> server_entry(Self, ReportRef) end),
	    receive
		ReportRef ->
		    demonitor(MonRef, [flush]),
		    Pid;
		{'DOWN', MonRef, _, _, Reason} ->
		    case whereis(colib_server) of
			undefined ->
			    error(Reason);
			Pid2 -> Pid2
		    end
	    end;
	Pid -> Pid
    end.

-record(state, {zero_sum = 0, one_sum = 0, red_sum = 0, processes = []}).

server_entry(Report, Ref) ->
    register(colib_server, self()),
    Report ! Ref,
    server_loop(#state{}).

server_loop(State0 = #state{zero_sum = ZeroSum, one_sum = OneSum, red_sum = RedSum,
			    processes = Processes}) ->
    receive
	{spawned, Pid} ->
	    monitor(process, Pid),
	    server_loop(State0#state{processes = [Pid|Processes]});
	{died, Pid, Stats} ->
	    {Zero, One, Reds} = Stats,
	    server_loop(State0#state{zero_sum  = ZeroSum + Zero,
				     one_sum   = OneSum  + One,
				     red_sum   = RedSum  + Reds,
				     processes = lists:delete(Pid, Processes)});
	{'DOWN', _, process, Pid, Abnormal} when Abnormal =/= normal ->
	    io:fwrite(standard_error, "A colib slave ~p died abnormally: ~p~n",
		      [Pid, Abnormal]),
	    server_loop(State0#state{processes = lists:delete(Pid, Processes)});
	{poll, Asker, Ref} ->
	    State =
		case Processes of
		    [] ->
			Asker ! {Ref, {ZeroSum, OneSum, RedSum}},
			State0;
		    _Other ->
			%% io:fwrite(standard_error,
			%% 	  "Awaiting pending processes for poll: ~p~n",
			%% 	  [Processes]),
			State1 = #state{zero_sum=ZeroSum1, one_sum=OneSum1,
					red_sum=RedSum1}
			    = server_poll_loop(State0),
			Asker ! {Ref, {ZeroSum1, OneSum1, RedSum1}},
			State1
		end,
	    server_loop(State#state{zero_sum = 0, one_sum = 0, red_sum = 0});
	_Other ->
	    server_loop(State0)
    end.

%% XXX: Eww
server_poll_loop(State = #state{processes=[]}) -> State;
server_poll_loop(State0 = #state{zero_sum = ZeroSum, one_sum = OneSum,
				 red_sum = RedSum, processes = Processes}) ->
    receive
	{died, Pid, Stats} ->
	    {Zero, One, Reds} = Stats,
	    server_poll_loop(State0#state{zero_sum  = ZeroSum + Zero,
					  one_sum   = OneSum  + One,
					  red_sum   = RedSum  + Reds,
					  processes = lists:delete(Pid, Processes)});
	{'DOWN', _, process, Pid, Abnormal} when Abnormal =/= normal ->
	    io:fwrite(standard_error, "A colib slave ~p died abnormally: ~p~n",
		      [Pid, Abnormal]),
	    server_poll_loop(State0#state{processes = lists:delete(Pid, Processes)})
    end.

-spec poll_stats() -> {integer(), integer(), integer()}.
poll_stats() ->
    start_server() ! {poll, self(), Ref = make_ref()},
    receive {Ref, Answer} -> Answer end.

select_timers(ZeroTimer, OneTimer) ->
    application:set_env(colib, timers, {ZeroTimer, OneTimer}).

regular_spawn_link(Fun) ->
    spawn_link(
      fun() ->
	      colib_server ! {spawned, self()},
	      try
		  Fun()
	      after
		  {reductions, Reds} = process_info(self(), reductions),
		  colib_server ! {died, self(), {0, 0, Reds}}
	      end
      end).

epiphany_spawn_link(Fun) ->
    epiphany:spawn_link(
      fun() ->
	      colib_server ! {spawned, self()},
	      {ZeroConf, OneConf} = application:get_env(colib, timers, {off, off}),
	      _ = epiphany:timers(ZeroConf, OneConf),
	      try
		  Fun()
	      after
		  {Zero, One} = epiphany:timers(off, off),
		  {reductions, Reds} = process_info(self(), reductions),
		  colib_server ! {died, self(), {Zero, One, Reds}}
	      end
      end).
