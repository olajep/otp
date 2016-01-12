% The following benchmark is based on an entry in
% The Computer Language Benchmarks Game
% http://benchmarksgame.alioth.debian.org/
% Contributed by: Hynek Vychodil 2009
% Inspired by regex-dna Erlang HiPE #5 program
%    by Sergei Matusevich 2007 and Thanassis Avgerinos 2009

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

-module(regexdna).

-export([prepare/0, bench/1, main/1]).

prepare() ->
    Patterns = [{Pat, re:compile(Pat, [caseless])}
		|| Pat <- patterns()],
    {Patterns, [?MODULE, re, lists, epiphany, epiphany_server]}.

bench(Patterns) ->
    do(Patterns).

main(_) -> main().

main() -> do(), halt().

do() ->
    S = self(),
    Worker = spawn_link(fun () -> work(S) end),
    Worker ! {data, read()},
    receive finish -> ok end.

do(Patterns) ->
    S = self(),
    Worker = spawn_link(fun () -> work(S, Patterns) end),
    Worker ! {data, read()},
    receive finish -> ok end.

work(Master) ->
    Patterns = [{Pat, re:compile(Pat, [caseless])}
		|| Pat <- patterns()],
    work(Master, Patterns).


work(Master, Patterns) ->
    S = self(),
    {RawSize, [B3, B2, B1 | _]} = receive
				    {data, Data} -> Data
				  end,
    [L1, L2, L3] = L = [size(X) || X <- [B1, B2, B3]],
    Size = lists:sum(L),
    {Spawn, _Count} = colib:spawn_and_count(),
    PIDS = [{Spawn(matcher(S, B2, B3, MR)),
	     printer(Pat)}
	    || {Pat, {ok, MR}} <- Patterns],
    ExpandedSize = L1 + L3 + size(expand(B2, L2, 0, <<>>)),
    results(PIDS),
    io:format("~n~b~n~b~n~b~n",
	      [RawSize, Size, ExpandedSize]),
    Master ! finish.

expand(B, S, I, R) when I < S ->
    case B of
      <<_:I/binary, $B, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(c|g|t)">>);
      <<_:I/binary, $D, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|g|t)">>);
      <<_:I/binary, $H, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|c|t)">>);
      <<_:I/binary, $K, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(g|t)">>);
      <<_:I/binary, $M, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|c)">>);
      <<_:I/binary, $N, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|c|g|t)">>);
      <<_:I/binary, $R, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|g)">>);
      <<_:I/binary, $S, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(c|g)">>);
      <<_:I/binary, $V, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|c|g)">>);
      <<_:I/binary, $W, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(a|t)">>);
      <<_:I/binary, $Y, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, "(c|t)">>);
      <<_:I/binary, X, _/binary>> ->
	  expand(B, S, I + 1, <<R/binary, X>>)
    end;
expand(_, _, _, R) -> R.

matcher(S, B2, B3, MR) ->
    fun () ->
	    S !
	      {self(), countMatches(B2, MR) + countMatches(B3, MR)}
    end.

printer(Pat) ->
    fun (Num) -> io:format("~s ~b~n", [Pat, Num]) end.

countMatches(Data, RE) ->
    case re:run(Data, RE, [global]) of
      {match, M} -> length(M);
      nomatch -> 0
    end.

results([{PID, Fin} | R]) ->
    receive {PID, Ret} -> Fin(Ret), results(R) end;
results([]) -> ok.

patterns() ->
    ["agggtaaa|tttaccct", "[cgt]gggtaaa|tttaccc[acg]",
     "a[act]ggtaaa|tttacc[agt]t",
     "ag[act]gtaaa|tttac[agt]ct",
     "agg[act]taaa|ttta[agt]cct",
     "aggg[acg]aaa|ttt[cgt]ccct",
     "agggt[cgt]aa|tt[acg]accct",
     "agggta[cgt]a|t[acg]taccct",
     "agggtaa[cgt]|[acg]ttaccct"].

read() ->
    {ok, IoDevice} = file:open("regexdna-input", [read, binary]),
    read(IoDevice, 0, [], []).

read(IoDevice, Size, Seg, R) ->
    case file:read_line(IoDevice) of
	{ok, <<$>:8, _/binary>> = Line} ->
	  read(IoDevice, Size + size(Line) + 1, [],
	       [iolist_to_binary(lists:reverse(Seg, [])) | R]);
	{ok, Line} ->
	    read(IoDevice, Size + size(Line) + 1, [Line | Seg], R);
	eof ->
	    file:close(IoDevice),
	    {Size, [iolist_to_binary(lists:reverse(Seg, [])) | R]};
	Other ->
	    io:format(">>>>>>> Wrong! ~p~n", [Other]),
	    exit(bad_data)
    end.
