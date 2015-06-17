%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2014. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(matchstate_align).

-include_lib("test_server/include/test_server.hrl").

-export([all/0,
	 copy_terms/1, conversions/1]).

%% Internal exports.
-export([sleeper/0]).

all() ->
    [copy_terms, conversions].

-define(heap_binary_size, 64).

copy_terms(Config) when is_list(Config) ->
    Self = self(),
    ?line Pid = spawn_link(fun() -> copy_server(Self) end),
    F = fun(Term) ->
		Pid ! Term,
		receive
		    Term -> ok;
		    Other ->
			io:format("Sent: ~P\nGot back:~P", [Term,12,Other,12]),
			?t:fail(bad_term)
		end
	end,
    ?line test_terms(F),
    ok.

copy_server(Parent) ->
    receive
	Term ->
	    Parent ! Term,
	    copy_server(Parent)
    end.

%% Tests list_to_binary/1, binary_to_list/1 and size/1,
%% using flat lists.

-define(ppline(A), io:fwrite(" -> ~s~n", [??A]), ?line A).

conversions(suite) -> [];
conversions(Config) when is_list(Config) ->
    ?ppline(test_bin([])),
    ?ppline(test_bin([1])),
    ?ppline(test_bin([1, 2])),
    ?ppline(test_bin([1, 2, 3])),
    ?ppline(test_bin(lists:seq(0, ?heap_binary_size))),
    ?ppline(test_bin(lists:seq(0, ?heap_binary_size+1))),
    ?ppline(test_bin(lists:seq(0, 255))),
    ?ppline(test_bin(lists:duplicate(1000, $@))),

    %% Binary in list.
    List = [1,2,3,4,5],
    ?ppline(B1 = make_sub_binary(list_to_binary(List))),
    ?ppline(5 = size(B1)),
    ?ppline(5 = size(make_unaligned_sub_binary(B1))),
    ?ppline(40 = bit_size(B1)),
    ?ppline(40 = bit_size(make_unaligned_sub_binary(B1))),
    ?ppline(B2 = list_to_binary([42,B1,19])),
    ?ppline(B2 = list_to_binary([42,make_unaligned_sub_binary(B1),19])),
    ?ppline(B2 = iolist_to_binary(B2)),
    ?ppline(B2 = iolist_to_binary(make_unaligned_sub_binary(B2))),
    ?ppline(7 = size(B2)),
    ?ppline(7 = size(make_sub_binary(B2))),
    ?ppline(56 = bit_size(B2)),
    ?ppline(56 = bit_size(make_sub_binary(B2))),
    ?ppline([42,1,2,3,4,5,19] = binary_to_list(B2)),
    ?ppline([42,1,2,3,4,5,19] = binary_to_list(make_sub_binary(B2))),
    ?ppline([42,1,2,3,4,5,19] = binary_to_list(make_unaligned_sub_binary(B2))),
    ?ppline([42,1,2,3,4,5,19] = bitstring_to_list(B2)),
    ?ppline([42,1,2,3,4,5,19] = bitstring_to_list(make_sub_binary(B2))),
    ?ppline([42,1,2,3,4,5,19] = bitstring_to_list(make_unaligned_sub_binary(B2))),

    ok.

test_bin(List) ->
    ?line Size = length(List),
    ?line Bin = list_to_binary(List),
    ?line Bin = iolist_to_binary(List),
    ?line Bin = list_to_bitstring(List),
    ?line Size = iolist_size(List),
    ?line Size = iolist_size(Bin),
    ?line Size = iolist_size(make_unaligned_sub_binary(Bin)),
    ?line Size = size(Bin),
    ?line Size = size(make_sub_binary(Bin)),
    ?line Size = size(make_unaligned_sub_binary(Bin)),
    ?line List = binary_to_list(Bin),
    ?line List = binary_to_list(make_sub_binary(Bin)),
    ?line List = binary_to_list(make_unaligned_sub_binary(Bin)),
    ?line List = bitstring_to_list(Bin),
    ?line List = bitstring_to_list(make_unaligned_sub_binary(Bin)).

test_terms(Test_Func) ->
    ?line Test_Func(atom),
    ?line Test_Func(''),
    ?line Test_Func('a'),
    ?line Test_Func('ab'),
    ?line Test_Func('abc'),
    ?line Test_Func('abcd'),
    ?line Test_Func('abcde'),
    ?line Test_Func('abcdef'),
    ?line Test_Func('abcdefg'),
    ?line Test_Func('abcdefgh'),

    ?line Test_Func(fun() -> ok end),
    X = id([a,{b,c},c]),
    Y = id({x,y,z}),
    Z = id(1 bsl 8*257),
    ?line Test_Func(fun() -> X end),
    ?line Test_Func(fun() -> {X,Y} end),
    ?line Test_Func([fun() -> {X,Y,Z} end,
		     fun() -> {Z,X,Y} end,
		     fun() -> {Y,Z,X} end]),

    ?line Test_Func({trace_ts,{even_bigger,{some_data,fun() -> ok end}},{1,2,3}}),
    ?line Test_Func({trace_ts,{even_bigger,{some_data,<<1,2,3,4,5,6,7,8,9,10>>}},
		     {1,2,3}}),

    ?line Test_Func(1),
    ?line Test_Func(42),
    ?line Test_Func(-23),
    ?line Test_Func(256),
    ?line Test_Func(25555),
    ?line Test_Func(-3333),

    ?line Test_Func(1.0),

    ?line Test_Func(183749783987483978498378478393874),
    ?line Test_Func(-37894183749783987483978498378478393874),
    Very_Big = very_big_num(),
    ?line Test_Func(Very_Big),
    ?line Test_Func(-Very_Big+1),

    ?line Test_Func([]),
    ?line Test_Func("abcdef"),
    ?line Test_Func([a, b, 1, 2]),
    ?line Test_Func([a|b]),

    ?line Test_Func({}),
    ?line Test_Func({1}),
    ?line Test_Func({a, b}),
    ?line Test_Func({a, b, c}),
    ?line Test_Func(list_to_tuple(lists:seq(0, 255))),
    ?line Test_Func(list_to_tuple(lists:seq(0, 256))),

    ?line Test_Func(make_ref()),
    ?line Test_Func([make_ref(), make_ref()]),

    %% ?line Test_Func(make_port()),

    ?line Test_Func(make_pid()),

    ?line Test_Func(Bin0 = list_to_binary(lists:seq(0, 14))),
    ?line Test_Func(Bin1 = list_to_binary(lists:seq(0, ?heap_binary_size))),
    ?line Test_Func(Bin2 = list_to_binary(lists:seq(0, ?heap_binary_size+1))),
    ?line Test_Func(Bin3 = list_to_binary(lists:seq(0, 255))),

    ?line Test_Func(make_unaligned_sub_binary(Bin0)),
    ?line Test_Func(make_unaligned_sub_binary(Bin1)),
    ?line Test_Func(make_unaligned_sub_binary(Bin2)),
    ?line Test_Func(make_unaligned_sub_binary(Bin3)),

    ?line Test_Func(make_sub_binary(lists:seq(42, 43))),
    ?line Test_Func(make_sub_binary([42,43,44])),
    ?line Test_Func(make_sub_binary([42,43,44,45])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46,47])),
    ?line Test_Func(make_sub_binary([42,43,44,45,46,47,48])),
    ?line Test_Func(make_sub_binary(lists:seq(42, 49))),
    ?line Test_Func(make_sub_binary(lists:seq(0, 14))),
    ?line Test_Func(make_sub_binary(lists:seq(0, ?heap_binary_size))),
    ?line Test_Func(make_sub_binary(lists:seq(0, ?heap_binary_size+1))),
    ?line Test_Func(make_sub_binary(lists:seq(0, 255))),

    ?line Test_Func(make_unaligned_sub_binary(lists:seq(42, 43))),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46,47])),
    ?line Test_Func(make_unaligned_sub_binary([42,43,44,45,46,47,48])),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(42, 49))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, 14))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, ?heap_binary_size))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, ?heap_binary_size+1))),
    ?line Test_Func(make_unaligned_sub_binary(lists:seq(0, 255))),

    %% Bit level binaries.
    ?line Test_Func(<<1:1>>),
    ?line Test_Func(<<2:2>>),
    ?line Test_Func(<<42:10>>),
    ?line Test_Func(list_to_bitstring([<<5:6>>|lists:seq(0, 255)])),

    ?line Test_Func(F = fun(A) -> 42*A end),
    ?line Test_Func(lists:duplicate(32, F)),

    ?line Test_Func(FF = fun ?MODULE:all/0),
    ?line Test_Func(lists:duplicate(32, FF)),

    ok.

very_big_num() ->
    very_big_num(33, 1).

very_big_num(Left, Result) when Left > 0 ->
    ?line very_big_num(Left-1, Result*256);
very_big_num(0, Result) ->
    ?line Result.

make_pid() ->
    ?line spawn_link(?MODULE, sleeper, []).

sleeper() ->
    ?line receive after infinity -> ok end.

%% Utilities.

make_sub_binary(Bin) when is_binary(Bin) ->
    {_,B} = split_binary(list_to_binary([0,1,3,Bin]), 3),
    B;
make_sub_binary(List) ->
    make_sub_binary(list_to_binary(List)).

make_unaligned_sub_binary(Bin0) when is_binary(Bin0) ->
    Bin1 = <<0:3,Bin0/binary,31:5>>,
    Sz = size(Bin0),
    <<0:3,Bin:Sz/binary,31:5>> = id(Bin1),
    Bin;
make_unaligned_sub_binary(List) ->
    make_unaligned_sub_binary(list_to_binary(List)).

id(I) -> I.
