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

-module(hipe_epiphany_ra).
-export([ra/2]).

ra(Defun0, Options) ->
  {Defun1, Coloring}
    = case proplists:get_value(regalloc, Options, coalescing) of
	coalescing ->
	  ra(Defun0, Options, hipe_coalescing_regalloc);
	optimistic ->
	  ra(Defun0, Options, hipe_optimistic_regalloc);
	graph_color ->
	  ra(Defun0, Options, hipe_graph_coloring_regalloc);
	linear_scan ->
	  hipe_epiphany_ra_ls:ra(Defun0, 0, Options);
	naive ->
	  hipe_epiphany_ra_naive:ra(Defun0, Options);
	Other ->
	  exit({unknown_regalloc_compiler_option, Other})
      end,
  hipe_epiphany_ra_finalise:finalise(Defun1, Coloring).

ra(Defun, Options, RegAllocMod) ->
  hipe_regalloc_loop:ra(Defun, 0, Options, RegAllocMod, hipe_epiphany_specific).

