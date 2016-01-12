-module(driver).

-export([run/1, scaling/1]).

-define(SAMPLES, 10).
-define(US_PER_S, 1000000).

-define(STATS_FORMAT, "\t~p\t~p\t~p\t~p").
-define(STATS_HEADERS, [time, reds, fetch_stall, load_stall]).
-define(STATS_PATTERN, {Time, Reds, Zero, One}).
-define(STATS_FMTARGS, [Time, Reds, Zero, One]).

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
    colib:select_timers(clk, ext_fetch_stalls),
    {FirstTime, FirstReds, Clk1, FetchStall}
	= bench(Benchmark, Args, 0, 0, 0, 0, Samples div 2),
    colib:select_timers(clk, ext_load_stalls),
    {SecondTime, SecondReds, Clk2, LoadStall}
	= bench(Benchmark, Args, 0, 0, 0, 0, (Samples+1) div 2),
    {((FirstTime + SecondTime) / ?US_PER_S) / Samples,
     (FirstReds + SecondReds) / Samples,
     case Clk1 of 0 -> nan; _ -> FetchStall / Clk1 end,
     case Clk2 of 0 -> nan; _ -> LoadStall  / Clk2 end}.

bench(_Benchmark, _Args, TimeSum, RedSum, ZeroSum, OneSum, 0) ->
    {TimeSum,
     RedSum,
     ZeroSum,
     OneSum};
bench(Benchmark, Args, TimeSum, RedSum, ZeroSum, OneSum, Samples) when Samples > 0 ->
    timer:sleep(50),
    _ = colib:poll_stats(),
    {Time, _} = timer:tc(Benchmark, bench, Args),
    {Zero, One, Reds} = colib:poll_stats(),
    io:format("~p samples of ~p remaining: ~p s, reds: ~p, T0: ~p, T1: ~p\n",
	      [Samples - 1,
	       Benchmark, Time / ?US_PER_S,
	       Reds, Zero, One]),
    bench(Benchmark, Args, TimeSum + Time, RedSum + Reds, ZeroSum + Zero, OneSum + One,
	  Samples - 1).
