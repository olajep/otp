-module(regressions).

-export([working/0, broken/0, run/1]).
-export([eval/1]).
-export([message/0, message_gc/0, fun_gc/0, mbuf_gc/0, map_align/0,
	 mbuf_free/0, matchstate_align/0]).

-export([display_server/0]).

working() ->
    Tests = [message, message_gc, fun_gc, mbuf_gc, map_align, mbuf_free,
	     matchstate_align],
    lists:foreach(fun(T)-> io:fwrite("~p~n", [T]) end, Tests).

broken() ->
    Tests = [],
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
    Ref1 = monitor(process, P1),
    %% Another regression tested by this case is the corruption of stacktraces
    %% that starts in BIFs. Just to be on the safe side, we match it a bit, too.
    receive {'DOWN', Ref1, process, P1, Reason1} ->
	    {badarg,[{erlang,tuple_size, _, _}|_]} = Reason1
    end,
    P2 = epiphany:spawn(Fun),
    Ref2 = monitor(process, P2),
    receive {'DOWN', Ref2, process, P2, Reason2} ->
	    {badarg,[{erlang,tuple_size, _, _}|_]} = Reason2
    end,
    ok.

%% Tests that the map_t structure isn't padded, causing holes in the heap.
map_align() ->
    P = epiphany:spawn(eval("fun()->#{}end.")),
    Ref = monitor(process, P),
    receive {'DOWN', Ref, process, P, normal} -> ok end.

%% Tests that mbufs that are allocated on the master are cleaned up properly
%% when slave processes quit.
mbuf_free() ->
    {P1, Ref1} = spawn_monitor(fun()->matmul:test(epiphany:count()+1)end),
    receive {'DOWN', Ref1, process, P1, Reason1} ->
	    {system_limit, _} = Reason1
    end,
    true = is_integer(matmul:test(epiphany:count())),
    ok.

%% Tests that a GC during binary matching works
matchstate_align() ->
    ts_epi:run([matchstate_align]).

display_server() ->
    receive stop -> ok;
	    T -> erlang:display(T),
		 display_server()
    end.

eval(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    {ok, [AbsForm]} = erl_parse:parse_exprs(Tokens),
    {value, Value, _} = erl_eval:expr(AbsForm, orddict:new()),
    Value.
