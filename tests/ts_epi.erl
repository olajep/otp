-module(ts_epi).

-export([run/1]).

-define(TIMEOUT, 10*60*1000).
-define(EXIT_TIMEOUT, 5*1000).

run([]) ->
    init:stop();
run([Mod|Mods]) ->
    Datadir = atom_to_list(Mod) ++ "_data",
    setup(Mod),
    Opts = case filelib:is_dir(Datadir) of
	       true  -> [{data_dir, Datadir}];
	       false -> []
	   end
	++ [{priv_dir, priv_dir(Mod)}],
    try
	ok = run_mod(Mod, Mod:all(), Opts),
	cleanup(Mod)
    catch T:E ->
	    io:fwrite("~p:~p~n", [T,E]),
	    io:fwrite("At ~p~n", [erlang:get_stacktrace()]),
	    cleanup(Mod),
	    halt(1)
    end,
    run(Mods).

setup(Mod) ->
    file:make_dir(priv_dir(Mod)).

cleanup(Mod) ->
    DelCmd = case os:type() of
		 {unix, _} -> "rm -r ";
		 {win32, _} -> "del/S "
	     end,
    io:put_chars(Out = os:cmd(DelCmd ++ priv_dir(Mod))),
    "" = Out,
    ok.

priv_dir(_Mod) -> "priv".

run_mod(_Mod, [], _Opts) -> ok;
run_mod(Mod, [Test|Tests], Opts) ->
    io:fwrite("   \e[1m-->\e[0m Running ~p:~p(~p)~n", [Mod, Test, Opts]),
    Self = self(),
    {Pid, Ref} = epiphany:spawn_monitor(fun() -> Self ! {ok, Mod:Test(Opts)} end),
    %% Some tests do not return 'ok', like map_SUITE:t_pdict/1
    {ok, _} = receive Res -> Res
	      after ?TIMEOUT ->
		      io:fwrite("   \e[1m-->\e[0m ~p:~p TIMED OUT~n", [Mod, Test]),
		      Pid ! '$junk',
		      timer:sleep(3000),
		      %% Might be truncated or contain junk frames on top
		      BT = process_info(Pid, current_stacktrace),
		      exit(Pid, kill),
		      {error, {res_timeout, BT}}
	      end,
    {'DOWN', Ref, process, Pid, normal}
	= receive Mon -> Mon after ?EXIT_TIMEOUT -> {error, mon_timeout} end,
    run_mod(Mod, Tests, Opts).
