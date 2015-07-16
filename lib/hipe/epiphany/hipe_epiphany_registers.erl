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

-module(hipe_epiphany_registers).

-export([
	 reg_name/1,
	 first_virtual/0,
	 is_precoloured/1,
	 all_precoloured/0,

	 allocatable/0,
	 is_fixed/1,

	 nr_args/0,
	 arg/1,
	 args/1,
	 is_arg/1,

	 nr_rets/0,
	 ret/1,

	 call_clobbered/0,
	 tailcall_clobbered/0,
	 live_at_return/0
	]).

-include("../rtl/hipe_literals.hrl").

%% ETODO: delete me
-ifndef(EPIPHANY_NR_ARG_REGS).
-define(EPIPHANY_NR_ARG_REGS, 4).
-endif.
-ifndef(EPIPHANY_NR_RET_REGS).
-define(EPIPHANY_NR_RET_REGS, ?EPIPHANY_NR_ARG_REGS).
-endif.

-type reg() :: non_neg_integer().
-export_type([reg/0]).

-define(R0, 0).
-define(R1, 1).
-define(R2, 2).
-define(R3, 3).
-define(R4, 4).
-define(R5, 5).
-define(R6, 6).
-define(R7, 7).
-define(R8, 8).
-define(R9, 9).
-define(R10, 10).
-define(R11, 11).
-define(R12, 12).
-define(R13, 13).
-define(R14, 14).
-define(R15, 15).
-define(R16, 16).
-define(R17, 17).
-define(R18, 18).
-define(R19, 19).
-define(R20, 20).
-define(R21, 21).
-define(R22, 22).
-define(R23, 23).
-define(R24, 24).
-define(R25, 25).
-define(R26, 26).
-define(R27, 27).
-define(R28, 28).
-define(R29, 29).
-define(R30, 30).
-define(R31, 31).
-define(R32, 32).
-define(R33, 33).
-define(R34, 34).
-define(R35, 35).
-define(R36, 36).
-define(R37, 37).
-define(R38, 38).
-define(R39, 39).
-define(R40, 40).
-define(R41, 41).
-define(R42, 42).
-define(R43, 43).
-define(R44, 44).
-define(R45, 45).
-define(R46, 46).
-define(R47, 47).
-define(R48, 48).
-define(R49, 49).
-define(R50, 50).
-define(R51, 51).
-define(R52, 52).
-define(R53, 53).
-define(R54, 54).
-define(R55, 55).
-define(R56, 56).
-define(R57, 57).
-define(R58, 58).
-define(R59, 59).
-define(R60, 60).
-define(R61, 61).
-define(R62, 62).
-define(R63, 63).
-define(LAST_PRECOLOURED, 63).

-define(ARG0, ?R0).
-define(ARG1, ?R1).
-define(ARG2, ?R2).
-define(ARG3, ?R3).

-define(TEMP1, ?R15).
%%-define(TEMP2, ?R).
%%-define(TEMP3, ?R).

-define(RET0, ?ARG0). %% The return registers are the argument registers
-define(RET1, ?ARG1).
-define(RET2, ?ARG2).
-define(RET3, ?ARG3).

%%-define(HEAP_POINTER, ?R).
-define(STACK_POINTER, ?R13).
%%-define(PROC_POINTER, ?R).

reg_name(R) when R =< ?LAST_PRECOLOURED -> [$r | integer_to_list(R)].

first_virtual() -> ?LAST_PRECOLOURED + 1.

is_precoloured(R) -> R =< ?LAST_PRECOLOURED.

all_precoloured() -> lists:seq(0, ?LAST_PRECOLOURED).

%% ETODO: Reserve regs for C?
fixed() -> [
	    %% fixed global registers
	    %%?HEAP_POINTER,
	    %%?PROC_POINTER,
	    ?STACK_POINTER
	   ].

allocatable() ->
  all_precoloured() -- fixed().

%% Needed for hipe_graph_coloring_regalloc.
%% Presumably true for Reg in AllPrecoloured \ Allocatable.
is_fixed(Reg) -> lists:member(Reg, fixed()).

nr_args() -> ?EPIPHANY_NR_ARG_REGS.

args(Arity) when is_integer(Arity) ->
  N = min(Arity, ?EPIPHANY_NR_ARG_REGS),
  args(N-1, []).

args(I, Rest) when is_integer(I), I < 0 -> Rest;
args(I, Rest) -> args(I-1, [arg(I) | Rest]).

arg(N) ->
  if N < ?EPIPHANY_NR_ARG_REGS ->
      case N of
	0 -> ?ARG0;
	1 -> ?ARG1;
	2 -> ?ARG2;
	3 -> ?ARG3;
	_ -> exit({?MODULE, arg, N})
      end;
     true ->
      exit({?MODULE, arg, N})
  end.

is_arg(R) ->
  case R of
    ?ARG0 -> ?EPIPHANY_NR_ARG_REGS > 0;
    ?ARG1 -> ?EPIPHANY_NR_ARG_REGS > 1;
    ?ARG2 -> ?EPIPHANY_NR_ARG_REGS > 2;
    ?ARG3 -> ?EPIPHANY_NR_ARG_REGS > 3;
    _ -> false
  end.

nr_rets() -> ?EPIPHANY_NR_RET_REGS.

ret(N) ->
  if N < ?EPIPHANY_NR_RET_REGS ->
      case N of
	0 -> ?RET0;
	1 -> ?RET1;
	2 -> ?RET2;
	3 -> ?RET3;
	_ -> exit({?MODULE, ret, N})
      end;
     true ->
      exit({?MODULE, ret, N})
  end.

call_clobbered() ->		% does the RA strip the type or not?
  lists:append([[{R,tagged},{R,untagged}]
		|| R <- allocatable()]).

tailcall_clobbered() ->		% tailcall crapola needs one temp
  [{?TEMP1,tagged},{?TEMP1,untagged}].

live_at_return() ->
  [{?STACK_POINTER,untagged}
  %%,{?PROC_POINTER,untagged}
  %%,{?HEAP_POINTER,untagged}
  ].
