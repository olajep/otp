-module(unit).

-compile([export_all]).
-include_lib("eunit/include/eunit.hrl").
-import(regressions, [eval/1]).

%% Test that simple computations work
fib_test() ->
    P = epiphany:spawn(fun ?MODULE:fibonacci_server/0),
    Ref = monitor(process, P),
    P ! {self(), 14},
    377 = receive M -> M end,
    P ! stop,
    receive {'DOWN', Ref, process, P, normal} -> ok end.

fibonacci_server() ->
    receive stop -> ok;
	    {Pid, Num} ->
	    Pid ! (fun Fib(0) -> 0;
		       Fib(1) -> 1;
		       Fib(N) -> Fib(N-1) + Fib(N-2)
		   end) (Num),
	    fibonacci_server()
    end.

matmul_test_() ->
    {timeout, 10.0, ?_assert(is_integer(matmul:test(16)))}.

fannkuch_test_() ->
    {timeout, 30.0, ?_assertMatch({16,228}, fannkuchredux:main(7))}.

mandelbrot_test_() ->
    {timeout, 30.0,
     ?_assertMatch(108976420, erlang:phash2(mandelbrot:main(["20"])))}.

fac_test() ->
    Fac = eval("fun Fac(0)->1; Fac(N) -> N * Fac(N-1) end."),
    Self = self(),
    {P, Ref} = epiphany:spawn_monitor(fun()-> Self ! Fac(20) end),
    receive N -> 2432902008176640000 = N end,
    receive {'DOWN', Ref, process, P, normal} -> ok end.
