-module(ts_epi).

-export([run/1]).

-define(TIMEOUT, 60000).

run([]) ->
    init:stop();
run([Mod|Mods]) ->
    ok = run_mod(Mod, Mod:all()),
    run(Mods).

run_mod(_Mod, []) -> ok;
run_mod(Mod, [Test|Tests]) ->
    io:fwrite(" --> Running ~p:~p([])~n", [Mod, Test]),
    Self = self(),
    {Pid, Ref} = epiphany:spawn_monitor(fun() -> Self ! Mod:Test([]) end),
    ok = receive Res -> Res after ?TIMEOUT -> {error, res_timeout} end,
    {'DOWN', Ref, process, Pid, normal}
	= receive Mon -> Mon after ?TIMEOUT -> {error, mon_timeout} end,
    run_mod(Mod, Tests).
