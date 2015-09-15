%% -*- erlang-indent-level: 2 -*-
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2015. All Rights Reserved.
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

-module(hipe_epiphany_main).
-export([rtl_to_epiphany/3]).

%%-define(DEBUG, 1).

-ifdef(DEBUG).
-define(IFDEBUG(X),X).
-else.
-define(IFDEBUG(X),ok).
-endif.

rtl_to_epiphany(MFA, RTL, Options) ->
  Defun1 = hipe_rtl_to_epiphany:translate(RTL),
  ?IFDEBUG(pp(Defun1, MFA, Options, "after translate", [])),

  Defun2 = hipe_epiphany_ra:ra(Defun1, Options),
  ?IFDEBUG(pp(Defun2, MFA, Options, "after regalloc", [])),

  Defun3 = hipe_epiphany_frame:frame(Defun2),
  ?IFDEBUG(pp(Defun3, MFA, Options, "after frame", [])),

  Defun4 = hipe_epiphany_finalise:finalise(Defun3),
  pp(Defun4, MFA, Options, "after finalise", []),

  {native, epiphany, {unprofiled, Defun4}}.

pp(Defun, MFA, Options, _DbgFmt, _DbgArgs) ->
  case proplists:get_value(pp_native, Options) of
    true ->
      ?IFDEBUG(io:format("~w: " ++ _DbgFmt ++ "\n", [?MODULE|_DbgArgs])),
      hipe_epiphany_pp:pp(Defun);
    {only,Lst} when is_list(Lst) ->
      case lists:member(MFA,Lst) of
	true ->
	  ?IFDEBUG(io:format("~w: " ++ _DbgFmt ++ "\n", [?MODULE|_DbgArgs])),
	  hipe_epiphany_pp:pp(Defun);
	false ->
	  ok
      end;
    {only,MFA} ->
      ?IFDEBUG(io:format("~w: " ++ _DbgFmt ++ "\n", [?MODULE|_DbgArgs])),
      hipe_epiphany_pp:pp(Defun);
    {file,FileName} ->
      {ok, File} = file:open(FileName, [write,append]),
      ?IFDEBUG(io:format(File, "~w: " ++ _DbgFmt ++ "\n", [?MODULE|_DbgArgs])),
      hipe_epiphany_pp:pp(File, Defun),
      ok = file:close(File);
    _ ->
      ok
  end.
