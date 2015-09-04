-module(bs_SUITE).

-export([suite/0, init_per_suite/0, init_per_suite/1,
         end_per_suite/1, all/0]).
-export([bs_add/1, bs_bincomp/1, bs_bits/1, bs_bitsize/1, bs_bugs_R08/1,
         bs_bugs_R09/1, bs_bugs_R12/1, bs_build/1, bs_catch_bug/1,
         bs_checksum/1, bs_construct/1, bs_decode/1, bs_des/1, bs_extract/1,
         bs_flatb/1, bs_id3/1, bs_match/1, bs_orber/1, bs_pmatch/1,
         bs_pmatch_bugs/1, bs_pmatch_in_guards/1, bs_potpurri/1, bs_remove3/1,
         bs_save/1, bs_shell_native/1, bs_split/1, bs_system_limit_32/1,
         bs_utf/1, bs_var_segs/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap, {minutes, 2}}].

init_per_suite() ->
    [].

init_per_suite(Config) ->
    case erlang:system_info(hipe_architecture) of
	undefined -> {skip, "HiPE not available or enabled"};
	_ -> Config
    end.

end_per_suite(_Config) ->
    ok.

all() ->
    [
     %% Exposes a known, nondeterministic, bug in HiPE. Skip for now.
     %% bs_remove3,

     bs_shell_native, %% Slow, start first

     %% Exposes another bug when heap_flush is inlined
     %% bs_des,bs_bits,

     bs_add,bs_bincomp,bs_bitsize,bs_bugs_R08,bs_bugs_R09,bs_bugs_R12,
     bs_build,bs_catch_bug,bs_checksum,bs_construct,bs_decode,bs_extract,
     bs_flatb,bs_id3,bs_match,bs_orber,bs_pmatch, bs_pmatch_bugs,
     bs_pmatch_in_guards,bs_potpurri,bs_save,
     bs_split,bs_system_limit_32,bs_utf,bs_var_segs].

test(Config, TestCase) ->
    Dir = ?config(data_dir, Config),
    F = filename:join(Dir, atom_to_list(TestCase) ++ ".erl"),
    {ok, TestCase} = compile:file(F),
    code:ensure_loaded_epiphany(TestCase),
    case erlang:function_exported(TestCase, prepare_for_test, 0) of
	true -> ok = TestCase:prepare_for_test();
	false -> ok
    end,
    ok = TestCase:test(),
    HiPEOpts = try TestCase:hipe_options() catch error:undef -> [] end,
    {ok, TestCase} = hipe:c(TestCase, [{target, epiphany}|HiPEOpts]),
    ok = TestCase:test().

bs_add(Config) ->
    test(Config, bs_add).

bs_bincomp(Config) ->
    test(Config, bs_bincomp).

bs_bits(Config) ->
    test(Config, bs_bits).

bs_bitsize(Config) ->
    test(Config, bs_bitsize).

bs_bugs_R08(Config) ->
    test(Config, bs_bugs_R08).

bs_bugs_R09(Config) ->
    test(Config, bs_bugs_R09).

bs_bugs_R12(Config) ->
    test(Config, bs_bugs_R12).

bs_build(Config) ->
    test(Config, bs_build).

bs_catch_bug(Config) ->
    test(Config, bs_catch_bug).

bs_checksum(Config) ->
    test(Config, bs_checksum).

bs_construct(Config) ->
    test(Config, bs_construct).

bs_decode(Config) ->
    test(Config, bs_decode).

bs_des(Config) ->
    test(Config, bs_des).

bs_extract(Config) ->
    test(Config, bs_extract).

bs_flatb(Config) ->
    test(Config, bs_flatb).

bs_id3(Config) ->
    test(Config, bs_id3).

bs_match(Config) ->
    test(Config, bs_match).

bs_orber(Config) ->
    test(Config, bs_orber).

bs_pmatch(Config) ->
    test(Config, bs_pmatch).

bs_pmatch_bugs(Config) ->
    test(Config, bs_pmatch_bugs).

bs_pmatch_in_guards(Config) ->
    test(Config, bs_pmatch_in_guards).

bs_potpurri(Config) ->
    test(Config, bs_potpurri).

bs_remove3(Config) ->
    test(Config, bs_remove3).

bs_save(Config) ->
    test(Config, bs_save).

bs_shell_native(Config) ->
    test(Config, bs_shell_native).

bs_split(Config) ->
    test(Config, bs_split).

bs_system_limit_32(Config) ->
    test(Config, bs_system_limit_32).

bs_utf(Config) ->
    test(Config, bs_utf).

bs_var_segs(Config) ->
    test(Config, bs_var_segs).
