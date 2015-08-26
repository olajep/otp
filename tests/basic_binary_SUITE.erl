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

-module(basic_binary_SUITE).

%%-define(line_trace,1).
-include_lib("test_server/include/test_server.hrl").

-export([all/0,
	 exact_match/1]).

all() -> [exact_match].

exact_match(Config) when is_list(Config) ->
    ?line <<>> = id(<<>>),
    ?line <<"a">> = id(<<"a">>),
    ?line <<"b">> = element(1, id({<<"b">>, a})),
    ?line <<"c">> = maps:get(k, id(#{k=><<"c">>, q=><<"b">>})),
    ?line {<<"d">>, a} = maps:get(r, id(#{q=><<"c">>, r=>{<<"d">>,a}})),
    ok.

id(X) -> X.
