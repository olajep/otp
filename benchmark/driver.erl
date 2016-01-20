-module(driver).

-export([run/1, scaling/1]).

-define(SAMPLES, 10).
-define(US_PER_S, 1000000).

-define(STATS_FORMAT, "\t~p\t~p\t~p\t~p\t~p\t~p\t~p\t~p").
-define(STATS_HEADERS, [time, time_sd, reds, reds_sd,
			fetch_stall, fs_sd, load_stall, ls_sd]).
-define(STATS_PATTERN, {Time, TimeSd, Reds, RedsSd, Zero, ZeroSd, One, OneSd}).
-define(STATS_FMTARGS, [Time, TimeSd, Reds, RedsSd, Zero, ZeroSd, One, OneSd]).

run([Outfile|Benchmarks]) ->
    case epiphany:state() of
	booting ->
	    timer:sleep(100),
	    run([Outfile|Benchmarks]);
	_ ->
	    {ok, IoDev} = file:open(atom_to_list(Outfile), [write]),
	    io:format(IoDev, "~p" ?STATS_FORMAT "\n", [benchmark | ?STATS_HEADERS]),
	    io:format(       "~p" ?STATS_FORMAT "\n", [benchmark | ?STATS_HEADERS]),
	    try run(IoDev, Benchmarks)
	    after
		ok = file:close(IoDev),
		erlang:halt()
	    end
    end.

run(_IoDev, []) -> ok;
run(IoDev, Benchmark) when is_atom(Benchmark) ->
    Pregen = prepare(Benchmark),
    ?STATS_PATTERN = bench(Benchmark, [Pregen], ?SAMPLES),
    io:format(IoDev, "~p" ?STATS_FORMAT "\n", [Benchmark | ?STATS_FMTARGS]),
    io:format(       "~p" ?STATS_FORMAT "\n", [Benchmark | ?STATS_FMTARGS]),
    ok;
run(IoDev, [Benchmark|Benchmarks]) ->
    run(IoDev, Benchmark),
    run(IoDev, Benchmarks).

scaling([Outfile|Benchmarks]) ->
    case epiphany:state() of
	booting ->
	    timer:sleep(100),
	    scaling([Outfile|Benchmarks]);
	_ ->
	    {ok, IoDev} = file:open(atom_to_list(Outfile), [write]),
	    io:format(IoDev, "~p\t~p" ?STATS_FORMAT "\n", [benchmark, cores | ?STATS_HEADERS]),
	    io:format(       "~p\t~p" ?STATS_FORMAT "\n", [benchmark, cores | ?STATS_HEADERS]),
	    try scaling(IoDev, Benchmarks)
	    after
		ok = file:close(IoDev),
		erlang:halt()
	    end
    end.

scaling(_IoDev, []) -> ok;
scaling(IoDev, [Benchmark|Benchmarks]) ->
    {_Spawn, Count} = colib:spawn_and_count(),
    Pregen = prepare(Benchmark),
    scaling(IoDev, Benchmark, Pregen, Count),
    scaling(IoDev, Benchmarks).

scaling(_IoDev, _Benchmark, _Pregen, 0) -> ok;
scaling(IoDev, Benchmark, Pregen, Cores) ->
    ?STATS_PATTERN = bench(Benchmark, [Pregen, Cores], ?SAMPLES),
    io:format(IoDev, "~p\t~p" ?STATS_FORMAT "\n", [Benchmark, Cores | ?STATS_FMTARGS]),
    io:format(       "~p\t~p" ?STATS_FORMAT "\n", [Benchmark, Cores | ?STATS_FMTARGS]),
    scaling(IoDev, Benchmark, Pregen, Cores div 2).

prepare(Benchmark) ->
    {Pregen, Modules0} = Benchmark:prepare(),
    Modules = [Benchmark, epiphany, epiphany_server | Modules0],
    Spawn =
    case epiphany:state() of
	offline ->
	    lists:foreach(fun code:ensure_loaded/1, Modules),
	    fun erlang:spawn/1;
	online ->
	    lists:foreach(fun code:ensure_loaded_epiphany/1, Modules),
	    fun epiphany:spawn/1
    end,
    %% Roundtrip to ensure everything is ready before we start measuring
    Self = self(),
    Spawn(fun() -> Self ! ready end),
    receive ready -> ok after 10000 -> error(timeout) end,
    Pregen.

bench(Benchmark, Args, Samples) ->
    {Times, Reds, FetchStalls, LoadStalls}
	= bench(Benchmark, Args, [], [], [], [], Samples),
    {avg(Times) / ?US_PER_S,
     stdev(Times) / ?US_PER_S,
     avg(Reds),
     stdev(Reds),
     case FetchStalls of nan -> nan; _ -> avg(FetchStalls) end,
     case FetchStalls of nan -> nan; _ -> stdev(FetchStalls) end,
     case LoadStalls  of nan -> nan; _ -> avg(LoadStalls) end,
     case LoadStalls  of nan -> nan; _ -> stdev(LoadStalls) end}.

avg(List) -> lists:sum(List) / length(List).

stdev(List) ->
    Len = length(List),
    Mean = lists:sum(List) / Len,
    math:sqrt(lists:sum([(X-Mean)*(X-Mean) || X<-List])
	      / (Len - 1)).

bench(_Benchmark, _Args, Times, Reds, FetchStalls, LoadStalls, 0) ->
    {Times, Reds, FetchStalls, LoadStalls};
bench(Benchmark, Args, Times, Reds, FetchStalls0, LoadStalls0,
      Samples) when Samples > 0 ->
    case Samples rem 2 of
	1 -> colib:select_timers(clk, ext_fetch_stalls);
	0 -> colib:select_timers(clk, ext_load_stalls)
    end,
    timer:sleep(50),
    _ = colib:poll_stats(),
    {Time, _} = timer:tc(Benchmark, bench, Args),
    {Zero, One, Red} = colib:poll_stats(),
    Quotient = case Zero of 0 -> nan; _ -> One / Zero end,
    io:format("~p samples of ~p remaining: ~p s, reds: ~p, T0: ~p, T1: ~p\n",
	      [Samples - 1,
	       Benchmark, Time / ?US_PER_S,
	       Red, Zero, One]),
    {FetchStalls, LoadStalls} =
	case Samples rem 2 of
	    _ when Quotient =:= nan -> {nan, nan};
	    1 -> {[Quotient | FetchStalls0], LoadStalls0};
	    0 -> {FetchStalls0, [Quotient | LoadStalls0]}
	end,
    bench(Benchmark, Args, [Time | Times], [Red | Reds], FetchStalls,
	  LoadStalls, Samples - 1).
