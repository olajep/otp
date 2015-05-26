-module(regressions).

-export([working/0, broken/0, run/1]).
-export([message/0, message_gc/0, fun_gc/0, mbuf_gc/0, map_hole/0]).

-export([display_server/0]).

working() ->
    Tests = [message, message_gc, fun_gc, mbuf_gc],
    lists:foreach(fun(T)-> io:fwrite("~p~n", [T]) end, Tests).

broken() ->
    Tests = [map_hole],
    lists:foreach(fun(T)-> io:fwrite("~p~n", [T]) end, Tests).

%% Return code indicates test result
run([Test]) ->
    ok = ?MODULE:Test(),
    %% We want to do an orderly shutdown (not a halt), to verify that
    %% nothing has broken.
    init:stop().

message() ->
    P = epiphany:spawn(?MODULE, display_server, []),
    Ref = monitor(process, P),
    P ! "hej",
    ok = receive M = {'DOWN', Ref, _, _, _} -> M after 1000 -> ok end,
    P ! stop,
    receive {'DOWN', Ref, process, P, normal} -> ok end.

%% Test that the message queue is included as GC roots.
message_gc() ->
    P1 = epiphany:spawn(?MODULE,display_server,[]),
    Ref1 = monitor(process, P1),
    ok = receive M1 = {'DOWN', Ref1, _, _, _} -> M1 after 10 -> ok end,
    P1 ! "hej",
    ok = receive M2 = {'DOWN', Ref1, _, _, _} -> M2 after 10 -> ok end,
    P1 ! hej,
    ok = receive M3 = {'DOWN', Ref1, _, _, _} -> M3 after 10 -> ok end,
    P1 ! stop,
    receive {'DOWN', Ref1, process, P1, normal} -> ok end,
    %% We're trying to make P2 garbage collect when receiving a message. The
    %% message is the reply from code_server when loading the modules required
    %% for io:fwrite for the first time.
    P2 = epiphany:spawn(eval("fun() -> io:fwrite(\"hej~n\") end.")),
    Ref2 = monitor(process, P2),
    receive {'DOWN', Ref2, process, P2, normal} -> ok end.

%% Test that funs allocated in a slave process can be GC:ed.
fun_gc() ->
    Fun = eval("fun(A,B) -> io:fwrite(\"~p>~p= \", [A,B]), "
	       ++ "io:fwrite(\"~p~n\", [A>B]) end."),
    P = epiphany:spawn(erlang,apply,[Fun, ["hej","nej"]]),
    Ref = monitor(process, P),
    receive {'DOWN', Ref, process, P, normal} -> ok end.

%% Test that slaves do not leave slave-allocated mbufs on processes.
mbuf_gc() ->
    Fun = eval("fun() -> dict:to_list({dict,a,a,a,a,a,a,a,a}) end."),
    P1 = epiphany:spawn(Fun),
    %% An unrelated bug causes the exit tuples to be corrupted; let's not
    %% monitor these processes, for now.
    %% Ref1 = monitor(process, P1),
    %% receive {'DOWN', Ref1, process, P1, _} -> ok end,
    wait_for_exit(P1),
    P2 = epiphany:spawn(Fun),
    %% Ref2 = monitor(process, P2),
    %% receive {'DOWN', Ref2, process, P2, _} -> ok end.
    wait_for_exit(P2).

%% Known to be broken
map_hole() ->
    P = epiphany:spawn(eval("fun()->#{}end.")),
    Ref = monitor(process, P),
    receive {'DOWN', Ref, process, P, normal} -> ok end.

display_server() ->
    receive stop -> ok;
	    T -> erlang:display(T),
		 display_server()
    end.

%% Wait for the exit of P without using monitors.
wait_for_exit(P) ->
    case process_info(P) of
	undefined -> ok;
	_ ->
	    timer:sleep(10),
	    wait_for_exit(P)
    end.

eval(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, [AbsForm]} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:expr(AbsForm, orddict:new()),
    Value.
