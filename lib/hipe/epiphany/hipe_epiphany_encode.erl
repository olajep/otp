%%% -*- erlang-indent-level: 2 -*-
%%%
%%% %CopyrightBegin%
%%% 
%%% Copyright Ericsson AB 2015. All Rights Reserved.
%%% 
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved online at http://www.erlang.org/.
%%% 
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%% 
%%% %CopyrightEnd%
%%%

-module(hipe_epiphany_encode).

-export([insn_encode/1]).

-include("hipe_epiphany.hrl").

-undef(ASSERT).
-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

-define(FITS_SIMM3(Val),  ((-16#4   =< (Val)) and ((Val) < 16#4))).
-define(FITS_SIMM8(Val), ((-16#80 =< (Val)) and ((Val) < 16#80))).
-define(FITS_SIMM11(Val), ((-16#400 =< (Val)) and ((Val) < 16#400))).
-define(FITS_SIMM24(Val), ((-16#800000 =< (Val)) and ((Val) < 16#800000))).
-define(FITS_UIMM3(Val), ((0 =< (Val)) and ((Val) < 16#8))).
-define(FITS_UIMM5(Val), ((0 =< (Val)) and ((Val) < 16#20))).
-define(FITS_UIMM8(Val), ((0 =< (Val)) and ((Val) < 16#100))).
-define(FITS_UIMM11(Val), ((0 =< (Val)) and ((Val) < 16#800))).
-define(FITS_UIMM16(Val), ((0 =< (Val)) and ((Val) < 16#10000))).

-type encoding() :: tuple().
-type half_opcode() :: none | 0..16#FFFF.
-type word_opcode() :: 0..16#FFFFFFFF.

%% @doc encodings() maps each instruction to a list of its possible encodings.
%%      An encoding is a value of the instruction record, but each field
%%      contains an atom describing how to encode that particular field, or
%%      'undefined', meaning to ignore that field.
-spec encodings() -> #{atom() => [{half_opcode(), word_opcode(), encoding()}]}.
encodings() ->
    %% Since fpu ops have a different opcode, we represent it as a different
    %% encoding, although semantically they should be different instructions.
    #{alu =>
	   %% au_imm looks like it could be merged with aluop, since all
	   %% valid encodings are the same. However, it would accept lu_imm
	   %% class operations too, thus making it sensitive to the ordering
	   %% of this table.
	  [{2#0011,
	    2#1011,
	    #alu{aluop=au_imm,dst=rd,src1=rn,src2=simm11}},
	   {2#0110,
	    2#1111 bor (2#0110 bsl 16),
	    #alu{aluop=lu_imm,dst=rd,src1=rn,src2=uimm5}},
	   %% The immediate LUs are nasty because they borrow a bit from the
	   %% opcode field to fit their aluop as well as a 5-bit immediate.
	   %% Therefore, we call 'asr' its own encoding because of that.
	   {2#1110,
	    2#1111 bor (2#1110 bsl 16),
	    #alu{aluop=asr,dst=rd,src1=rn,src2=uimm5}},
	   {2#1010,
	    2#1111 bor (2#1010 bsl 16),
	    #alu{aluop=aluop,dst=rd,src1=rn,src2=rm}},
	   {2#0111,
	    2#1111 bor (2#0111 bsl 16),
	    #alu{aluop=fpuop,dst=rd,src1=rn,src2=rm}}],
      bcc =>
	  [{2#0000,
	    2#1000,
	    #bcc{'cond'='cond',label=simm24}}],
      ldr =>
	  [{2#00100,
	    2#01100 bor (0 bsl 25),
	    #ldr{size=size,dst=rd,base=rn,sign=disp_sign,offset=uimm11}},
	   {2#00001,
	    2#01001,
	    #ldr{size=size,dst=rd,base=rn,sign=ix_sign,offset=rm}}],
      str =>
	  [{2#10100,
	    2#11100 bor (0 bsl 25),
	    #str{size=size,src=rd,base=rn,sign=disp_sign,offset=uimm11}},
	   {2#10001,
	    2#11001,
	    #str{size=size,src=rd,base=rn,sign=ix_sign,offset=rm}}],
      %% The bor (2#0010 bsl 16) term in mov and movt is not required according
      %% to the architecture reference manual, but binutils won't dissassemble
      %% without it.
      mov =>
	  [{2#00011,
	    2#01011 bor (2#0010 bsl 16),
	    #mov{dst=rd,src=uimm16}}],
      movt =>
	  [{none,
	    2#01011 bor (2#0010 bsl 16) bor (1 bsl 28),
	    #movt{dst=rd,src=uimm16}}],
      movcc =>
	  [{2#0010,
	    2#1111 bor (2#0010 bsl 16),
	    #movcc{'cond'='cond',dst=rd,src=rn}}],
      movfs =>
	  [{2#0100010010,
	    2#0100011111 bor (2#0010 bsl 16),
	    #movfs{dst=rd,src=sn}}],
      jr =>
	  [{2#0101000010,
	    2#0101001111 bor (2#0010 bsl 16),
	    #jr{funv=rn}}],
      jalr =>
	  [{2#0101010010,
	    2#0101011111 bor (2#0010 bsl 16),
	    #jalr{funv=rn}}]
     }.

insn_encode(#rts{}) ->
    LR = hipe_epiphany:mk_temp(hipe_epiphany_registers:lr(), 'untagged'),
    encode(#jr{funv=LR});
insn_encode(I) ->
    encode(I).

%% To encode an instruction, we pick its list of encodings from the table and
%% try them in order
encode(I) ->
    Op = element(1, I),
    encode(I, maps:get(Op, encodings(), [])).

encode(I, []) ->
    exit({?MODULE, encode, {no_matching_encoding, I}});
encode(I, [{HalfOpcode, WordOpcode, Fmt}|Fmts]) ->
    Args = tuple_size(I),
    Args = tuple_size(Fmt),
    case encode_fmt(I, Fmt, Args, HalfOpcode =:= none, 0) of
	{Full, Encoding0} ->
	    Encoding =
		case Full of
		    true -> WordOpcode;
		    false -> HalfOpcode
		end bor Encoding0,
	    {Full, Encoding};
	error ->
	    encode(I, Fmts)
    end.

encode_fmt(_I, _Fmt, 1, Full, Acc) -> {Full, Acc};
encode_fmt(I, Fmt, Arg, Full0, Acc) ->
    Pattern = element(Arg, Fmt),
    Value   = element(Arg, I),
    case encode_arg(Pattern, Value) of
	{Full1, Enc} ->
	    encode_fmt(I, Fmt, Arg-1, Full0 or Full1, Acc bor Enc);
	error ->
	    error
    end.

encode_arg(au_imm, add)   -> {false, 2#001 bsl 4};
encode_arg(au_imm, sub)   -> {false, 2#011 bsl 4};
encode_arg(lu_imm, lsr)   -> {false, 2#0 bsl 4};
encode_arg(lu_imm, lsl)   -> {false, 2#1 bsl 4};
encode_arg(asr,    asr)   -> {false, 2#0 bsl 4};
encode_arg(aluop,  add)   -> {false, 2#001 bsl 4};
encode_arg(aluop,  sub)   -> {false, 2#011 bsl 4};
encode_arg(aluop,  'and') -> {false, 2#101 bsl 4};
encode_arg(aluop,  orr)   -> {false, 2#111 bsl 4};
encode_arg(aluop,  eor)   -> {false, 2#000 bsl 4};
encode_arg(aluop,  asr)   -> {false, 2#110 bsl 4};
encode_arg(aluop,  lsr)   -> {false, 2#100 bsl 4};
encode_arg(aluop,  lsl)   -> {false, 2#010 bsl 4};
encode_arg(fpuop,  iadd)  -> {false, 2#000 bsl 4};
encode_arg(fpuop,  isub)  -> {false, 2#001 bsl 4};
encode_arg(fpuop,  imul)  -> {false, 2#010 bsl 4};

encode_arg('cond', eq)    -> {false, 2#0000 bsl 4};
encode_arg('cond', ne)    -> {false, 2#0001 bsl 4};
encode_arg('cond', gtu)   -> {false, 2#0010 bsl 4};
encode_arg('cond', gteu)  -> {false, 2#0011 bsl 4};
encode_arg('cond', lteu)  -> {false, 2#0100 bsl 4};
encode_arg('cond', ltu)   -> {false, 2#0101 bsl 4};
encode_arg('cond', gt)    -> {false, 2#0110 bsl 4};
encode_arg('cond', gte)   -> {false, 2#0111 bsl 4};
encode_arg('cond', lt)    -> {false, 2#1000 bsl 4};
encode_arg('cond', lte)   -> {false, 2#1001 bsl 4};
encode_arg('cond', always)-> {false, 2#1110 bsl 4};
encode_arg('cond', link)  -> {false, 2#1111 bsl 4};

encode_arg(rm, #epiphany_temp{reg=Reg}) -> encode_reg(Reg, 7);
encode_arg(rn, #epiphany_temp{reg=Reg}) -> encode_reg(Reg, 10);
encode_arg(rd, #epiphany_temp{reg=Reg}) -> encode_reg(Reg, 13);

encode_arg(sn, Special) -> encode_special(Special, 10);

encode_arg(size, b) -> {false, 2#00 bsl 5};
encode_arg(size, h) -> {false, 2#01 bsl 5};
encode_arg(size, w) -> {false, 2#10 bsl 5};
encode_arg(size, d) -> {false, 2#11 bsl 5};

encode_arg(disp_sign, '+') -> {false, 0};
encode_arg(disp_sign, '-') -> {true, 1 bsl 24};
encode_arg(ix_sign,   '+') -> {false, 0};
encode_arg(ix_sign,   '-') -> {true, 1 bsl 20};

encode_arg(uimm5, #epiphany_uimm5{value=Value}) ->
    ?ASSERT(?FITS_UIMM5(Value)),
    {false, Value bsl 5};
encode_arg(simm11, #epiphany_simm11{value=Value}) ->
    <<Upper:8, Lower:3>> = <<Value:11/integer-big>>,
    LowEnc = Lower bsl 7,
    Enc = LowEnc bor (Upper bsl 16),
    if ?FITS_SIMM3(Value)  -> {false, LowEnc};
       ?FITS_SIMM11(Value) -> {true,  Enc}
    end;
encode_arg(uimm11, #epiphany_uimm11{value=Value}) ->
    <<Upper:8, Lower:3>> = <<Value:11/integer-big>>,
    LowEnc = Lower bsl 7,
    Enc = LowEnc bor (Upper bsl 16),
    if ?FITS_UIMM3(Value)  -> {false, LowEnc};
       ?FITS_UIMM11(Value) -> {true,  Enc}
    end;

encode_arg(uimm16, #epiphany_uimm16{value=Value}) ->
    <<Upper:8, Lower:8>> = <<Value:16/integer-big>>,
    LowEnc = Lower bsl 5,
    Enc = LowEnc bor (Upper bsl 20),
    if ?FITS_UIMM8(Value)  -> {false, LowEnc};
       ?FITS_UIMM16(Value) -> {true,  Enc}
    end;
encode_arg(uimm16, uimm8)  -> {false, 0};
encode_arg(uimm16, uimm16) -> {true,  0};

encode_arg(simm24, #epiphany_simm24{value=Value}) ->
    LowEnc = (Value band 16#FF)     bsl 8,
    Enc    = (Value band 16#FFFFFF) bsl 8,
    if ?FITS_SIMM8(Value)  -> {false, LowEnc};
       ?FITS_SIMM24(Value) -> {true,  Enc}
    end;
encode_arg(simm24, simm8)  -> {false, 0};
encode_arg(simm24, simm24) -> {true,  0};

encode_arg(undefined, _) -> {false, 0};
encode_arg(_, _) -> error.

encode_reg(No, Shift) when is_integer(No), No >= 0, No < 64 ->
    if No < 8 -> {false, No bsl Shift};
       No >= 8 ->
	    <<Upper:3, Lower:3>> = <<No:6>>,
	    {true, (Lower bsl Shift) bor (Upper bsl (Shift + 16))}
    end.

encode_special(Special, Shift) ->
    {Bank, No} =
	case Special of
	    status -> {0, 1}
	end,
    case Bank of
	0 -> encode_reg(No, Shift);
	M ->
	    {_, Enc0} = encode_reg(No, Shift),
	    {true, Enc0 bor (M bsl 20)}
    end.
