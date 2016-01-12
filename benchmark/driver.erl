-module(driver).

-export([run/1, scaling/1]).

-define(SAMPLES, 10).
-define(US_PER_S, 1000000).

run([Outfile|Benchmarks]) ->
    case epiphany:state() of
	booting ->
	    timer:sleep(100),
	    run([Outfile|Benchmarks]);
	_ ->
	    {ok, IoDev} = file:open(atom_to_list(Outfile), [write]),
	    io:format(IoDev, "~p\t~p\n", [benchmark, time]),
	    io:format(       "~p\t~p\n", [benchmark, time]),
	    try run(IoDev, Benchmarks)
	    after
		ok = file:close(IoDev),
		erlang:halt()
	    end
    end.

run(_IoDev, []) -> ok;
run(IoDev, Benchmark) when is_atom(Benchmark) ->
    Pregen = prepare(Benchmark),
    Time = bench(Benchmark, [Pregen], 0, ?SAMPLES),
    io:format(IoDev, "~p\t~p\n", [Benchmark, Time]),
    io:format(       "~p\t~p\n", [Benchmark, Time]),
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
	    io:format(IoDev, "~p\t~p\t~p\n", [benchmark, cores, time]),
	    io:format(       "~p\t~p\t~p\n", [benchmark, cores, time]),
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
    Time = bench(Benchmark, [Pregen, Cores], 0, ?SAMPLES),
    io:format(IoDev, "~p\t~p\t~p\n", [Benchmark, Cores, Time]),
    io:format(       "~p\t~p\t~p\n", [Benchmark, Cores, Time]),
    scaling(IoDev, Benchmark, Pregen, Cores div 2).

prepare(Benchmark) ->
    {Pregen, Modules0} = Benchmark:prepare(),
    Modules = [Benchmark, epiphany, epiphany_server | Modules0],
    {Spawn, _Count} = colib:spawn_and_count(),
    %% Roundtrip to ensure everything is ready before we start measuring
    Self = self(),
    Spawn(fun() -> Self ! ready end),
    receive ready -> ok after 10000 -> error(timeout) end,

    Pregen.

bench(_Benchmark, _Args, TimeSum, 0) -> TimeSum / (?SAMPLES * ?US_PER_S);
bench(Benchmark, Args, TimeSum, Samples) when Samples > 0 ->
    timer:sleep(50),
    {Time, _} = timer:tc(Benchmark, bench, Args),
    io:format("Sample ~p/~p of ~p: ~p s\n", [?SAMPLES - Samples + 1, ?SAMPLES,
					     Benchmark, Time / ?US_PER_S]),
    bench(Benchmark, Args, TimeSum + Time, Samples - 1).
