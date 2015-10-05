-module(colib).

-export([pmap/2, pmap/3, pforeach/2, spawn_and_count/0]).

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
	    {fun erlang:spawn_link/1,
	     erlang:system_info(schedulers_online)};
	booting ->
	    timer:sleep(100),
	    spawn_and_count();
	_ -> {fun epiphany:spawn_link/1, epiphany:count()}
    end.
