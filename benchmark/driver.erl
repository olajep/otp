-module(driver).

-export([run/1]).

run([Outfile|Benchmarks]) ->
    {ok, IoDev} = file:open(atom_to_list(Outfile), [write]),
    run(IoDev, Benchmarks).

run(IoDev, []) ->
    ok = file:close(IoDev),
    erlang:halt();
run(IoDev, Benchmark) when is_atom(Benchmark) ->
    {Pregen, Modules} = Benchmark:prepare(),
    case epiphany:state() of
	offline -> lists:foreach(fun code:ensure_loaded/1,          Modules);
	online  -> lists:foreach(fun code:ensure_loaded_epiphany/1, Modules)
    end,
    {Time, _} = timer:tc(Benchmark, bench, [Pregen]),
    io:format(IoDev, "~p\t~p\n", [Benchmark, Time]),
    io:format(       "~p\t~p\n", [Benchmark, Time]),
    ok;
run(IoDev, [Benchmark|Benchmarks]) ->
    run(IoDev, Benchmark),
    run(IoDev, Benchmarks).
