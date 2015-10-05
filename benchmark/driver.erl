-module(driver).

-export([run/1]).

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

    Time = bench(Benchmark, Pregen, 0, ?SAMPLES),
    io:format(IoDev, "~p\t~p\n", [Benchmark, Time]),
    io:format(       "~p\t~p\n", [Benchmark, Time]),
    ok;
run(IoDev, [Benchmark|Benchmarks]) ->
    run(IoDev, Benchmark),
    run(IoDev, Benchmarks).

bench(_Benchmark, _Pregen, TimeSum, 0) -> TimeSum / (?SAMPLES * ?US_PER_S);
bench(Benchmark, Pregen, TimeSum, Samples) when Samples > 0 ->
    timer:sleep(50),
    {Time, _} = timer:tc(Benchmark, bench, [Pregen]),
    io:format("Sample ~p/~p of ~p: ~p s\n", [?SAMPLES - Samples + 1, ?SAMPLES,
					     Benchmark, Time / ?US_PER_S]),
    bench(Benchmark, Pregen, TimeSum + Time, Samples - 1).
