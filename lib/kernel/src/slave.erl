%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
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
-module(slave).

-export([spawn/3, print/1, boot/0, prepare_loading/2, finish_loading/1,
         load_module/2, load_module/1]).

%% spawn/3
-spec spawn(Module, Function, Args) -> undefined | pid() when
      Module :: module(),
      Function :: atom(),
      Args :: [term()].
spawn(_Module, _Function, _Args) ->
    erlang:nif_error(undefined).

%% print/1
-spec print(term()) -> ok.
print(_Term) ->
    erlang:nif_error(undefined).

%% boot/0
-spec boot() -> undefined | ok.
boot() ->
    erlang:nif_error(undefined).

%% prepare_loading/2
-spec erlang:prepare_loading(Module, Code)
                            -> PreparedCode | {error, Reason} | undefined when
      Module :: module(),
      Code :: binary(),
      PreparedCode :: binary(),
      Reason :: bad_file.
prepare_loading(_Module, _Code) ->
    erlang:nif_error(undefined).

%% finish_loading/1
-spec erlang:finish_loading(PreparedCodeBinaries) -> ok | Error | undefined when
      PreparedCodeBinaries :: [PreparedCodeBinary],
      PreparedCodeBinary :: binary(),
      ModuleList :: [module()],
      Error :: {not_purged,ModuleList} | {on_load,ModuleList}.
finish_loading(_List) ->
    erlang:nif_error(undefined).

%% load_module/2
-spec load_module(Module, Binary)
                 -> {module, Module} | {error, Reason} | undefined when
      Module :: module(),
      Binary :: binary(),
      Reason :: badfile | not_purged | on_load.
load_module(Mod, Code) ->
    case slave:prepare_loading(Mod, Code) of
        undefined -> undefined;
	{error,_}=Error ->
	    Error;
	Bin when erlang:is_binary(Bin) ->
	    case slave:finish_loading([Bin]) of
		ok ->
		    {module,Mod};
		{Error,[Mod]} ->
		    {error,Error}
	    end
    end.

%% load_module/1
-spec load_module(Module)
                 -> {module, Module} | {error, Reason} | undefined when
      Module :: module(),
      Reason :: badfile | not_purged | on_load | nofile.
load_module(Mod) ->
    case code:where_is_file(atom_to_list(Mod) ++ ".beam") of
	non_existing -> {error, nofile};
	File ->
	    case erl_prim_loader:get_file(File) of
		error -> {error, badfile};
		{ok, Bin, _FullName} ->
		    slave:load_module(Mod, Bin)
	    end
    end.
