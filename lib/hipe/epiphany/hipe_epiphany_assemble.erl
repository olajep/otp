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

-module(hipe_epiphany_assemble).
-export([assemble/4]).

-include("../main/hipe.hrl").	% for VERSION_STRING, when_option
-include("hipe_epiphany.hrl").
-include("../../kernel/src/hipe_ext_format.hrl").
-include("../rtl/hipe_literals.hrl").
-include("../misc/hipe_sdi.hrl").
-undef(ASSERT).
-define(ASSERT(G), if G -> [] ; true -> exit({assertion_failed,?MODULE,?LINE,??G}) end).

-define(FITS_SIMM24(Val), ((-16#800000 =< (Val)) and ((Val) < 16#800000))).

assemble(CompiledCode, Closures, Exports, Options) ->
  print("****************** Assembling *******************\n", [], Options),
  %%
  Code = [{MFA,
	   hipe_epiphany:defun_code(Defun),
	   hipe_epiphany:defun_data(Defun)}
	  || {MFA, Defun} <- CompiledCode],
  %%
  {ConstAlign,ConstSize,ConstMap,RefsFromConsts} =
    hipe_pack_constants:pack_constants(Code, 4),
  %%
  {CodeSize,CodeBinary,AccRefs,LabelMap,ExportMap} =
    encode(translate(Code, ConstMap), Options),
  print("Total num bytes=~w\n", [CodeSize], Options),
  %%
  SC = hipe_pack_constants:slim_constmap(ConstMap),
  DataRelocs = hipe_pack_constants:mk_data_relocs(RefsFromConsts, LabelMap),
  SSE = hipe_pack_constants:slim_sorted_exportmap(ExportMap,Closures,Exports),
  SlimRefs = hipe_pack_constants:slim_refs(AccRefs),
  Bin = term_to_binary([{?VERSION_STRING(),?HIPE_SYSTEM_CRC},
			ConstAlign, ConstSize,
			SC,
			DataRelocs, % nee LM, LabelMap
			SSE,
			CodeSize,CodeBinary,SlimRefs,
			0,[] % ColdCodeSize, SlimColdRefs
		       ]),
  %%
  Bin.

%%%
%%% Assembly Pass 1.
%%% Process initial {MFA,Code,Data} list.
%%% Translate each MFA's body, choosing operand & instruction kinds.
%%%
%%% Assembly Pass 2.
%%% Perform short/long form optimisation for jumps.
%%%
%%% Result is {MFA,NewCode,CodeSize,LabelMap} list.
%%%

translate(Code, ConstMap) ->
  translate_mfas(Code, ConstMap, []).

translate_mfas([{MFA,Insns,_Data}|Code], ConstMap, NewCode) ->
  {NewInsns,CodeSize,LabelMap} =
    translate_insns(Insns, MFA, ConstMap, hipe_sdi:pass1_init(), 0, []),
  translate_mfas(Code, ConstMap, [{MFA,NewInsns,CodeSize,LabelMap}|NewCode]);
translate_mfas([], _ConstMap, NewCode) ->
  lists:reverse(NewCode).

translate_insns([I|Insns], MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  NewIs = translate_insn(I, MFA, ConstMap),
  add_insns(NewIs, Insns, MFA, ConstMap, SdiPass1, Address, NewInsns);
translate_insns([], _MFA, _ConstMap, SdiPass1, Address, NewInsns) ->
  {LabelMap,CodeSizeIncr} = hipe_sdi:pass2(SdiPass1),
  {lists:reverse(NewInsns), Address+CodeSizeIncr, LabelMap}.

add_insns([I|Is], Insns, MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  NewSdiPass1 =
    case I of
      {'.label',L,_} ->
	hipe_sdi:pass1_add_label(SdiPass1, Address, L);
      {bcc_sdi,{_,{label,L}},_} ->
	SdiInfo = #sdi_info{incr=(4-2),lb=-16#80*2,ub=16#7F*2},
	hipe_sdi:pass1_add_sdi(SdiPass1, Address, L, SdiInfo);
      _ ->
	SdiPass1
    end,
  Address1 = Address + insn_size(I),
  add_insns(Is, Insns, MFA, ConstMap, NewSdiPass1, Address1, [I|NewInsns]);
add_insns([], Insns, MFA, ConstMap, SdiPass1, Address, NewInsns) ->
  translate_insns(Insns, MFA, ConstMap, SdiPass1, Address, NewInsns).

insn_size(I) ->
  case I of
    {'.label',_,_} -> 0;
    {'.reloc',_,_} -> 0;
    {bcc_sdi, _, _} -> 2; %% It is assumed short, initially
    {encoded, {false, _}, _} -> 2;
    {encoded, {true,  _}, _} -> 4
  end.

%% -> [{Op,Opnd,OrigI} | {encoded, {WordSized,Encoding}, OrigI}]
translate_insn(I, MFA, ConstMap) ->
  case I of
    #b{} -> do_b(I);
    #bcc{} -> do_bcc(I);
    #bl{} -> do_bl(I);
    #comment{} -> [];
    #jalr{} -> do_jalr(I);
    #label{} -> do_label(I);
    #mov{src={lo16, _}} -> do_mov(I, MFA, ConstMap);
    #movt{src={hi16, _}} -> do_movt(I, MFA, ConstMap);
    _ -> [{encoded, encode_insn(I), I}]
  end.

do_b(I=#b{funv=FunV,linkage=Linkage}) ->
  %% Force long (32-bit) form with special value 'simm24'
  [{'.reloc', {b_fun,FunV,Linkage}, #comment{term='fun'}},
   {encoded, encode_insn(#bcc{'cond'='always',label=simm24}), I}].

do_bcc(I=#bcc{'cond'=Cond,label=Label}) ->
  %% Encoding of bcc is deferred until we know the immediate
  [{bcc_sdi, {{'cond',Cond},do_label_ref(Label)}, I}].

do_bl(I=#bl{funv=FunV,sdesc=SDesc,linkage=Linkage}) ->
  %% Force long (32-bit) form with special value 'simm24'
  [{'.reloc', {b_fun,FunV,Linkage}, #comment{term='fun'}},
   {encoded, encode_insn(#bcc{'cond'='link',label=simm24}), I},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}}].

do_jalr(I=#jalr{sdesc=SDesc}) ->
  [{encoded, encode_insn(I), I},
   {'.reloc', {sdesc,SDesc}, #comment{term=sdesc}}].

do_mov(I=#mov{src=Reloc={lo16, _}}, MFA, ConstMap) ->
  Directive = do_reloc(Reloc, MFA, ConstMap),
  %% Force long (32-bit) form with special value 'uimm16'
  [Directive, {encoded, encode_insn(I#mov{src=uimm16}), I}].

do_movt(I=#movt{src=Reloc={hi16, _}}, MFA, ConstMap) ->
  Directive = do_reloc(Reloc, MFA, ConstMap),
  %% Force long (32-bit) form with special value 'uimm16'
  [Directive, {encoded, encode_insn(I#movt{src=uimm16}), I}].

do_label(I=#label{label=Label}) ->
  [{'.label', Label, I}].

do_reloc({Part, Imm}, MFA, ConstMap) ->
  RelocData =
    case Imm of
      Atom when is_atom(Atom) ->
	{load_atom, Part, Atom};
      {label, {Label, constant}} ->
	ConstNo = hipe_pack_constants:find_const({MFA,Label}, ConstMap),
	{load_address, Part, {constant, ConstNo}};
      {label, {Label, closure}} ->
	{load_address, Part, {closure, Label}};
      {label, {Label, c_const}} ->
	{load_address, Part, {c_const, Label}}
    end,
  {'.reloc', RelocData, #comment{term=reloc}}.

do_label_ref(Label) when is_integer(Label) ->
  {label,Label}.	% symbolic, since offset is not yet computable

encode_insn(I) ->
  hipe_epiphany_encode:insn_encode(I).

%%%
%%% Assembly Pass 3.
%%% Process final {MFA,Code,CodeSize,LabelMap} list from pass 2.
%%% Translate to a single binary code segment.
%%% Collect relocation patches.
%%% Build ExportMap (MFA-to-address mapping).
%%% Combine LabelMaps to a single one (for mk_data_relocs/2 compatibility).
%%% Return {CombinedCodeSize,BinaryCode,Relocs,CombinedLabelMap,ExportMap}.
%%%

encode(Code, Options) ->
  CodeSize = compute_code_size(Code, 0),
  ExportMap = build_export_map(Code, 0, []),
  {AccCode,Relocs} = encode_mfas(Code, 0, [], [], Options),
  CodeBinary = list_to_binary(lists:reverse(AccCode)),
  ?ASSERT(CodeSize =:= byte_size(CodeBinary)),
  CombinedLabelMap = combine_label_maps(Code, 0, gb_trees:empty()),
  {CodeSize,CodeBinary,Relocs,CombinedLabelMap,ExportMap}.

compute_code_size([{_MFA,_Insns,CodeSize,_LabelMap}|Code], Size) ->
  compute_code_size(Code, Size+CodeSize);
compute_code_size([], Size) -> Size.

build_export_map([{{M,F,A},_Insns,CodeSize,_LabelMap}|Code], Address, ExportMap) ->
  build_export_map(Code, Address+CodeSize, [{Address,M,F,A}|ExportMap]);
build_export_map([], _Address, ExportMap) -> ExportMap.

combine_label_maps([{MFA,_Insns,CodeSize,LabelMap}|Code], Address, CLM) ->
  NewCLM = merge_label_map(gb_trees:to_list(LabelMap), MFA, Address, CLM),
  combine_label_maps(Code, Address+CodeSize, NewCLM);
combine_label_maps([], _Address, CLM) -> CLM.

merge_label_map([{Label,Offset}|Rest], MFA, Address, CLM) ->
  NewCLM = gb_trees:insert({MFA,Label}, Address+Offset, CLM),
  merge_label_map(Rest, MFA, Address, NewCLM);
merge_label_map([], _MFA, _Address, CLM) -> CLM.

encode_mfas([{MFA,Insns,CodeSize,LabelMap}|Code], Address, AccCode, Relocs, Options) ->
  print("Generating code for: ~w\n", [MFA], Options),
  print("Offset   | Opcode   | Instruction\n", [], Options),
  {Address1,Relocs1,AccCode1} =
    encode_insns(Insns, Address, Address, LabelMap, Relocs, AccCode, Options),
  ExpectedAddress = Address + CodeSize,
  ?ASSERT(Address1 =:= ExpectedAddress),
  print("Finished.\n", [], Options),
  encode_mfas(Code, Address1, AccCode1, Relocs1, Options);
encode_mfas([], _Address, AccCode, Relocs, _Options) ->
  {AccCode,Relocs}.

encode_insns([I|Insns], Address, FunAddress, LabelMap, Relocs, AccCode, Options) ->
  case I of
    {'.label',L,_} ->
      LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
      print_insn(Address, [], I, Options),
      ?ASSERT(Address =:= LabelAddress),	% sanity check
      encode_insns(Insns, Address, FunAddress, LabelMap, Relocs, AccCode, Options);
    {'.reloc',Data,_} ->
      print_insn(Address, [], I, Options),
      Reloc = encode_reloc(Data, Address, FunAddress, LabelMap),
      encode_insns(Insns, Address, FunAddress, LabelMap, [Reloc|Relocs], AccCode, Options);
    {bcc_sdi,_,_} ->
      encode_insns(fix_bcc_sdi(I, Insns, Address, FunAddress, LabelMap),
		   Address, FunAddress, LabelMap, Relocs, AccCode, Options);
    {encoded, Enc={WordSize, Data}, _} ->
      print_insn(Address, Enc, I, Options),
      {Segment, Len} =
	case WordSize of
	  true  -> {<<Data:32/integer-little>>, 4};
	  false -> {<<Data:16/integer-little>>, 2}
	end,
      NewAccCode = [Segment|AccCode],
      encode_insns(Insns, Address+Len, FunAddress, LabelMap, Relocs, NewAccCode, Options)
  end;
encode_insns([], Address, _FunAddress, _LabelMap, Relocs, AccCode, _Options) ->
  {Address,Relocs,AccCode}.

encode_reloc(Data, Address, FunAddress, LabelMap) ->
  case Data of
    {b_fun,MFAorPrim,Linkage} ->
      %% b and bl are patched the same, so no need to distinguish
      %% call from tailcall
      PatchTypeExt =
	case Linkage of
	  remote -> ?CALL_REMOTE;
	  not_remote -> ?CALL_LOCAL
	end,
      {PatchTypeExt, Address, untag_mfa_or_prim(MFAorPrim)};
    {load_atom, Part, Atom} ->
      {?LOAD_ATOM, Address, {Part, Atom}};
    {load_address, Part, X} ->
      {?LOAD_ADDRESS, Address, {Part, X}};
    {sdesc, SDesc} ->
      #epiphany_sdesc{exnlab=ExnLab,fsize=FSize,arity=Arity,live=Live} = SDesc,
      ExnRA =
	case ExnLab of
	  [] -> [];	% don't cons up a new one
	  ExnLab -> gb_trees:get(ExnLab, LabelMap) + FunAddress
	end,
      {?SDESC, Address,
       ?STACK_DESC(ExnRA, FSize, Arity, Live)}
    end.

untag_mfa_or_prim(#epiphany_mfa{m=M,f=F,a=A}) -> {M,F,A};
untag_mfa_or_prim(#epiphany_prim{prim=Prim}) -> Prim.

fix_bcc_sdi(I, Insns, InsnAddress, FunAddress, LabelMap) ->
  {bcc_sdi,Opnds,OrigI} = I,
  {{'cond',BCond},Label} = Opnds,
  {label,L} = Label,
  LabelAddress = gb_trees:get(L, LabelMap) + FunAddress,
  BD = (LabelAddress - InsnAddress) div 2,
  %% if BD >= -16#80, BD =< 16#7F ->
  %%     NewI = #bcc{'cond'=BCond,label=hipe_epiphany:mk_simm8(BD)},
  %%     [{encoded, encode_insn(NewI), OrigI} | Insns];
  %%    ?FITS_SIMM24(BD) ->
  NewI = #bcc{'cond'=BCond,label=hipe_epiphany:mk_simm24(BD)},
  [{encoded, encode_insn(NewI), OrigI} | Insns].
  %% end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%
%%% Assembly listing support (pp_asm option).
%%%

print(String, Arglist, Options) ->
  ?when_option(pp_asm, Options, io:format(String, Arglist)).

print_insn(Address, Word, I, Options) ->
  ?when_option(pp_asm, Options, print_insn_2(Address, Word, I)).

print_insn_2(Address, Word, {NewI,NewArgs,OrigI}) ->
  io:format("~8.16.0b | ", [Address]),
  print_code_list(word_to_bytes(Word), 0),
  case NewI of
    '.reloc' ->
      io:format("\t.reloc ~w\n", [NewArgs]);
    _ ->
      hipe_epiphany_pp:pp_insn(OrigI)
  end.

word_to_bytes(Enc) ->
  case Enc of
    [] -> [];	% label or other pseudo instruction
    {true,  W} -> [(W bsr 24) band 16#FF, (W bsr 16) band 16#FF,
		   (W bsr 8) band 16#FF, W band 16#FF];
    {false, W} -> [(W bsr 8) band 16#FF, W band 16#FF]
  end.

print_code_list([Byte|Rest], Len) ->
  print_byte(Byte),
  print_code_list(Rest, Len+1);
print_code_list([], Len) ->
  fill_spaces(8-(Len*2)),
  io:format(" | ").

print_byte(Byte) ->
  io:format("~2.16.0b", [Byte band 16#FF]).

fill_spaces(N) when N > 0 ->
  io:format(" "),
  fill_spaces(N-1);
fill_spaces(0) ->
  [].

