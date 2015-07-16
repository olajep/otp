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

-define(UNSIGNED_RANGE(BITS), 0 .. (1 bsl BITS) - 1).
-define(SIGNED_RANGE(BITS), -(1 bsl (BITS - 1)) .. (1 bsl (BITS - 1)) - 1).

%%%--------------------------------------------------------------------
%%% Basic Values:
%%%
%%% temp	::= #epiphany_temp{reg, type, allocatable}
%%% reg		::= <token from hipe_epiphany_registers>
%%% type	::= tagged | untagged
%%% allocatable	::= true | false
%%%
%%% arity	::= uint8
%%%
%%% mfa		::= #epiphany_mfa{atom, atom, arity}

-type reg() :: hipe_epiphany_registers:reg().
-type type() :: tagged | untagged.

-record(epiphany_mfa, {m::atom(), f::atom(), a::arity()}).
-record(epiphany_temp, {reg::reg(), type::type(), allocatable::boolean()}).
-record(epiphany_simm11, {value :: ?SIGNED_RANGE(11)}).
-record(epiphany_simm24, {value :: ?SIGNED_RANGE(24)}). %% Only pc-relative
-record(epiphany_uimm5,  {value :: ?UNSIGNED_RANGE(5)}).
-record(epiphany_uimm16, {value :: ?UNSIGNED_RANGE(16)}).

%% Always appears together with a sign bit, but since that bit is available for
%% register operands as well, it cannot be included in the immediate without
%% making the representation less regular.
-record(epiphany_uimm11, {value :: ?UNSIGNED_RANGE(11)}).

%% Short immediates for halfword-sized instructions.
-ifdef(undef).
-record(epiphany_simm3,  {value :: ?SIGNED_RANGE(3)}). %% Expands to simm11
-record(epiphany_simm8,  {value :: ?SIGNED_RANGE(8)}). %% Expands to simm24, only pc-relative
-record(epiphany_uimm3,  {value :: ?UNSIGNED_RANGE(3)}). %% Expands to uimm11
-record(epiphany_uimm8,  {value :: ?UNSIGNED_RANGE(8)}). %% Expands to uimm16
-endif.

-type temp() :: #epiphany_temp{allocatable::true}.
-type pseudo_temp() :: #epiphany_temp{}.

%%% Instructions:

-type 'cond'() :: 'always'.
-type aluop() :: none().

-record(alu, {aluop :: aluop(), dst :: temp(), src1 :: temp(),
	      src2 :: temp() | #epiphany_simm11{} | #epiphany_uimm5{}}).
-record(label, {label :: non_neg_integer()}).
-record(movcc, {dst :: temp(), src :: temp(), 'cond' = 'always' :: 'cond'()}).
-record(mov, {dst :: temp(), src :: #epiphany_uimm16{}}).
-record(movt, {dst :: temp(), src :: #epiphany_uimm16{}}).
%% At most one operand may be a pseudo
-record(pseudo_move, {dst :: pseudo_temp(), src :: pseudo_temp()}).

%%% Function definitions.

-include("../misc/hipe_consttab.hrl").

-record(defun, {mfa :: mfa(), formals, code,
	       	data	  :: hipe_consttab(),
	        isclosure :: boolean(),
		isleaf    :: boolean(),
		var_range = [],
		label_range = []}).
