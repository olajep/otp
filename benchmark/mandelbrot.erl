% The following test is based on an entry in
% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
%% Contributed by Johan Karlsson based on Fredrik Svahn's mandelbrot program

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

-module(mandelbrot).
-export([prepare/0, bench/1, bench/2]).
-define(LIM_SQR, 4.0).
-define(ITER, 50).
-define(SR, -1.5).
-define(SI, -1).

-define(DEFAULT_WORKERS, 16).

prepare() -> {nil, [lists]}.

bench(nil) -> bench(nil, ?DEFAULT_WORKERS).
bench(nil, NoWorkers) ->
    main(["64"], NoWorkers).

main([Arg], NoWorkers) ->
    N = list_to_integer(Arg),
    Jobs = lists:seq(0, N-1),
    Self = self(),
    Row = fun(Y)-> Self ! {Y, row(N-1, 0, ?SI+Y*2/N, N, 0, [], 7)} end,
    spawn_link(fun() -> workserver_entry(Row, Jobs, NoWorkers) end),
    ["P4\n", Arg, " ", Arg, "\n"] ++ [receive {Job, C} -> C end || Job <- Jobs].

workserver_entry(Fun, Jobs, NoWorkers) ->
    Self = self(),
    Spawn = case epiphany:state() of
		offline ->
		    io:fwrite(standard_error, "Falling back to CPU~n", []),
		    fun erlang:spawn_link/1;
		_ ->
		    fun epiphany:spawn_link/1
	    end,
    Workers = [Spawn(fun()-> worker(Fun, Self) end)
	       || _ <- lists:seq(1,NoWorkers)],
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

%% Iterate over a row, collect bits, bytes and finally print the row
row(X,X, _, _, Bits, Bytes, C) ->
    case C of
	7 -> lists:reverse(Bytes);
	C -> lists:reverse([Bits bsl (C+1) | Bytes])
    end;

row(M,X, Y2, N, Bits, Bytes, 0) ->
    row(M,X+1, Y2, N, 0, [Bits bsl 1 + m(?ITER, ?SR+(X+X)/N, Y2) | Bytes], 7);

row(M,X, Y2, N, Bits, Bytes, BitC) ->
    row(M,X+1, Y2, N, Bits bsl 1 + m(?ITER, ?SR+(X+X)/N, Y2), Bytes, BitC-1).

%Mandelbrot algorithm
m(Iter, CR,CI) -> m(Iter - 1, CR, CI, CR, CI).

m(Iter, R, I, CR, CI) when is_float(R), is_float(I), is_float(CR), is_float(CI) ->
    case R*R+I*I > ?LIM_SQR of
	false when Iter > 0 -> m(Iter-1, R*R-I*I+CR, 2*R*I+CI, CR, CI);
	false -> 1;
	true -> 0
    end.
