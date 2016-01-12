% The following benchmark is based on an entry in
% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%
% contributed by Isaac Gouy (Erlang novice)
% parallelized by Kevin Scaldeferri

%% Revised BSD license
%%
%% Copyright © 2004-2008 Brent Fulgham, 2005-2015 Isaac Gouy
%%
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%
%%   * Redistributions of source code must retain the above copyright notice,
%%     this list of conditions and the following disclaimer.
%%   * Redistributions in binary form must reproduce the above copyright notice,
%%     this list of conditions and the following disclaimer in the documentation
%%     and/or other materials provided with the distribution.
%%   * Neither the name of "The Computer Language Benchmarks Game" nor the name
%%     of "The Computer Language Shootout Benchmarks" nor the names of its
%%     contributors may be used to endorse or promote products derived from this
%%     software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%% ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
%% LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
%% SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
%% CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
%% ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(binarytrees).
-export([prepare/0, bench/1, main/1]).
-export([depth/2]).

-define(Min,4).

prepare() ->
    {nil, [colib, timer, lists, io]}.

bench(nil) ->
    main(["10"]).

main([Arg]) ->
   N = list_to_integer(Arg),
   Max = lists:max([?Min+2,N]),

   Stretch = Max + 1,
   io:fwrite("stretch tree of depth ~w\t check: ~w~n",
      [ Stretch, itemCheck(bottomUp(0,Stretch)) ]),

   LongLivedTree = bottomUp(0,Max),
   depthLoop(?Min,Max),

   io:fwrite("long lived tree of depth ~w\t check: ~w~n",
	     [ Max, itemCheck(LongLivedTree) ]),

    ok.


depthLoop(D,M) ->
    Results = colib:pmap({?MODULE, depth}, [M], lists:seq(D, M, 2)),
    lists:foreach(fun(Result) ->
                          io:fwrite("~w\t trees of depth ~w\t check: ~w~n", Result)
                  end,
                  Results).

depth(D,M) ->
    N = 1 bsl (M-D + ?Min),
    [ 2*N, D, sumLoop(N,D,0) ].

sumLoop(0,_,Sum) -> Sum;
sumLoop(N,D,Sum) ->
   sumLoop(N-1,D, Sum + itemCheck(bottomUp(N,D)) + itemCheck(bottomUp(-N,D))).

bottomUp(I,0) -> {I, nil, nil};
bottomUp(I,D) -> {I, bottomUp(2*I-1,D-1), bottomUp(2*I,D-1)}.

itemCheck(nil) -> 0;
itemCheck({I,Left,Right}) ->
   I + itemCheck(Left) - itemCheck(Right).
