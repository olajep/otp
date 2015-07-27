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
-record(epiphany_prim, {prim::atom()}).
-record(epiphany_sdesc, {exnlab=[]::non_neg_integer()|[],
			 fsize::non_neg_integer(),
			 arity::arity(),
			 %% live is a sorted tuple of frame slot numbers
			 live={}::tuple()}).
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
-record(epiphany_simm8,  {value :: ?SIGNED_RANGE(8)}). %% Expands to simm24
-record(epiphany_uimm3,  {value :: ?UNSIGNED_RANGE(3)}). %% Expands to uimm11
-record(epiphany_uimm8,  {value :: ?UNSIGNED_RANGE(8)}). %% Expands to uimm16
-endif.

-type temp() :: #epiphany_temp{allocatable::true}.
-type pseudo_temp() :: #epiphany_temp{}.
-type link_time_immediate() :: atom() | {label, {non_neg_integer(), constant}}.

%%% Instructions (see below for field types):

-record(alu, {aluop, dst, src1, src2}).
-record(bcc, {'cond' = 'always', label}).
-record(comment, {term}).
-record(label, {label}).
-record(ldr, {size, dst, base, sign='+', offset}).
-record(mov, {dst, src}).
-record(movt, {dst, src}).
-record(movfs, {dst, src}).
-record(pseudo_call, {funv, sdesc, contlab, linkage}).
-record(pseudo_move, {dst, src}).
-record(pseudo_switch, {jtab, index, labels}).
-record(pseudo_tailcall, {funv, arity, stkargs, linkage}).
-record(pseudo_tailcall_prepare, {}).
-record(pseudo_bcc, {'cond' = 'always', true_label, false_label, pred}).
-record(rts, {nr_rets}). %% Alias for "jr lr"
-record(str, {size, src, base, sign='+', offset}).

%%% Instructions introduced by lowering pseudos, after register allocation:

%% A tail-recursive call
-record(b, {funv, linkage}).
%% A (non-tail) recursive call
-record(bl, {funv, sdesc, linkage}).
%% A (non-tail) recursive call
-record(jalr, {funv, sdesc}).
%% A tail-recursive call
-record(jr, {funv}).
-record(movcc, {'cond' = 'always', dst, src}).

%% Types for the instruction records:

-type 'cond'() :: 'always'
		| 'eq' | 'ne' | 'gt' | 'lt' | 'gte' | 'lte' | 'gtu' | 'ltu'
		| 'gteu' | 'lteu'
		| 'beq' | 'bne'.
-type aluop() :: 'add' | 'sub'
	       | 'orr' | 'and' | 'eor' | 'lsl' | 'lsr' | 'asr'
	       | 'imul'.
-type spec_reg() :: status.
-type linkage() :: remote | not_remote.
-type mem_size() :: 'b' | 'h' | 'w' | 'd'.
-type addr_sign() :: '+' | '-'.

-type alu() :: #alu{aluop::aluop(), dst::temp(), src1::temp(),
		    src2::temp() | #epiphany_simm11{} | #epiphany_uimm5{}}.
-type b() :: #b{funv::#epiphany_mfa{} | #epiphany_prim{}, linkage::linkage()}.
-type bcc() :: #bcc{'cond'::'cond'(), label::non_neg_integer()}.
-type comment() :: #comment{}.
-type bl() :: #bl{funv::#epiphany_mfa{} | #epiphany_prim{},
		  sdesc::#epiphany_sdesc{}, linkage::linkage()}.
-type jalr() :: #jalr{funv::temp(), sdesc::#epiphany_sdesc{}}.
-type jr() :: #jr{funv::temp()}.
-type label() :: #label{label :: non_neg_integer()}.
-type movcc() :: #movcc{'cond' :: 'cond'(), dst :: temp(), src :: temp()}.
-type mov() :: #mov{dst :: temp(), src :: #epiphany_uimm16{} |
					  {lo16, link_time_immediate()}}.
-type movt() :: #movt{dst :: temp(), src :: #epiphany_uimm16{} |
					    {hi16, link_time_immediate()}}.
-type ldr() :: #ldr{size::mem_size(), dst::temp(), base::temp(),
		    sign::addr_sign(), offset :: temp() | #epiphany_uimm11{}}.
-type movfs() :: #movfs{dst :: temp(), src :: spec_reg()}.
-type rts() :: #rts{nr_rets::non_neg_integer()}.
-type str() :: #str{size::mem_size(), src::temp(), base::temp(),
		    sign::addr_sign(), offset :: temp() | #epiphany_uimm11{}}.
-type pseudo_call() ::
	#pseudo_call{funv :: #epiphany_mfa{} | #epiphany_prim{} | temp(),
		     sdesc::#epiphany_sdesc{}, contlab::non_neg_integer(),
		     linkage::linkage()}.
-type pseudo_move() :: #pseudo_move{dst :: temp(),        src :: pseudo_temp()}
		     | #pseudo_move{dst :: pseudo_temp(), src :: temp()}.
-type pseudo_switch() ::
	#pseudo_switch{jtab::temp(), index::temp(), labels::[non_neg_integer()]}.
-type pseudo_tailcall() ::
	#pseudo_tailcall{funv :: #epiphany_mfa{} | #epiphany_prim{} | temp(),
			 arity::arity(), stkargs::[temp()],
			 linkage::linkage()}.
-type pseudo_tailcall_prepare() :: #pseudo_tailcall_prepare{}.
-type pseudo_bcc() :: #pseudo_bcc{'cond' :: 'cond'(),
				  true_label :: non_neg_integer(),
				  false_label :: non_neg_integer(),
				  pred :: number()}.

-type common_instr() :: alu() | bcc() | comment() | label() | mov() | movt()
		      | ldr() | movfs() | rts() | str().
-type lowered_instr() :: b() | bl() | jalr() | jr() | movcc().
-type pseudo_instr() :: pseudo_call() | pseudo_move() | pseudo_switch()
		      | pseudo_tailcall() | pseudo_tailcall_prepare()
		      | pseudo_bcc().

-type instr() :: common_instr() | lowered_instr() | pseudo_instr().

%%% Function definitions.

-include("../misc/hipe_consttab.hrl").

-record(defun,
	{mfa :: mfa(),
	 formals :: [pseudo_temp()],
	 code :: [instr()],
	 data	  :: hipe_consttab(),
	 isclosure :: boolean(),
	 isleaf    :: boolean(),
	 var_range   = [] :: [] | {non_neg_integer(), non_neg_integer()},
	 label_range = [] :: [] | {non_neg_integer(), non_neg_integer()}}).
