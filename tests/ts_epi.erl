-module(ts_epi).

-export([run/1]).

-define(TIMEOUT, 10*60*1000).
-define(EXIT_TIMEOUT, 5*1000).
-define(MAX_CONC, 8).

-define(MAX_TERM_PRINT_DEPTH, 10000).

run([]) ->
    init:stop();
run(['mods_conc'|Mods]) ->
    put(concurrent, true),
    run_mods_con(Mods);
run(['conc'|Mods]) ->
    put(concurrent, true),
    run(Mods);
run([Mod|Mods]) ->
    setup(Mod),
    Opts = opts(Mod),
    try
	ok = run_tests([{Mod, Fun, Opts} || Fun <- Mod:all()]),
	cleanup(Mod)
    catch T:E ->
	    erlang:display({T, E}),
	    io:fwrite("~P:~P~n", [T,?MAX_TERM_PRINT_DEPTH,E,?MAX_TERM_PRINT_DEPTH]),
	    io:fwrite("At ~P~n", [erlang:get_stacktrace(),?MAX_TERM_PRINT_DEPTH]),
	    cleanup(Mod),
	    halt(1)
    end,
    run(Mods).

run_mods_con(Mods) ->
    lists:foreach(fun setup/1, Mods),
    Tests = lists:concat([begin
			      Opts = opts(Mod),
			      [{Mod, Fun, Opts} || Fun <- Mod:all()]
			  end || Mod <- Mods]),
    try
	ok = run_tests(Tests),
	lists:foreach(fun cleanup/1, Mods)
    catch T:E ->
	    erlang:display({T, E}),
	    io:fwrite("~P:~P~n", [T,?MAX_TERM_PRINT_DEPTH,E,?MAX_TERM_PRINT_DEPTH]),
	    io:fwrite("At ~P~n", [erlang:get_stacktrace(),?MAX_TERM_PRINT_DEPTH]),
	    lists:foreach(fun cleanup/1, Mods),
	    halt(1)
    end.

opts(Mod) ->
    Datadir = atom_to_list(Mod) ++ "_data",
    case filelib:is_dir(Datadir) of
	true  -> [{data_dir, Datadir}];
	false -> []
    end
	++ [{priv_dir, priv_dir(Mod)}].

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

priv_dir(Mod) -> atom_to_list(Mod) ++ "_priv".

run_tests(Tests) ->
    case get(concurrent) of
	true -> run_con(Tests);
	_    -> run_seq(Tests)
    end.

run_con(Tests) ->
    run_con(Tests, []).

run_con([], []) -> ok;
run_con([Test|Tests], Running) when length(Running) < ?MAX_CONC ->
    New = spawn_opt(fun() -> run_single(Test) end, [link,monitor]),
    run_con(Tests, [New|Running]);
run_con(Tests, Running) ->
    receive {'DOWN', Ref, process, Pid, Reason} ->
	    normal = Reason,
	    run_con(Tests, lists:delete({Pid,Ref},Running))
    end.

run_single(Test = {Mod, Fun, _Opts}) ->
    try
	run_seq([Test]),
	io:fwrite("   \e[1m-->\e[0m ~p:~p OK!~n", [Mod, Fun])
    catch T:E ->
	    io:fwrite("   \e[1m-->\e[0m ~p:~p FAIL!~n", [Mod, Test]),
	    io:fwrite("~P:~P~n", [T,?MAX_TERM_PRINT_DEPTH,E,?MAX_TERM_PRINT_DEPTH]),
	    io:fwrite("At ~P~n", [erlang:get_stacktrace(),?MAX_TERM_PRINT_DEPTH]),
	    case T of
		throw -> throw(E);
		error -> error(E);
		exit -> exit(E)
	    end
    end.

run_seq([]) -> ok;
run_seq([{Mod, Test, Opts}|Tests]) ->
    io:fwrite("   \e[1m-->\e[0m Running ~p:~p(~p)~n", [Mod, Test, Opts]),
    Self = self(),
    {Pid, Ref} = epiphany:spawn_monitor(fun() ->
						Self ! {ok, Mod:Test(Opts)}
					end),
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
    run_seq(Tests).
