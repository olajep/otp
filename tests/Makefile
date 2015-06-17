TS_MODS=pdict_SUITE maps_SUITE map_SUITE bs_SUITE
TS_MODS+=bs_match_bin_SUITE bs_match_int_SUITE bs_match_misc_SUITE bs_match_tail_SUITE
TS_MODS+=bs_bincomp_SUITE bs_construct_SUITE bs_utf_SUITE
TS_MODS+=binary_SUITE
TS_OBJS=$(addsuffix .beam,$(TS_MODS))
OBJS=regressions.beam unit.beam matmul.beam fannkuchredux.beam mandelbrot.beam \
	matchstate_align.beam \
	ts_epi.beam $(TS_OBJS)

ERL=../bin/erl
ERLC=../bin/erlc
SLAVE=../bin/epiphany-unknown-elf/slave.smp.srec

.PHONY: fast test beam clean

test: $(OBJS)
	./run $(FLAGS) $(TS_MODS)

fast: $(OBJS)
	./run --fast $(FLAGS) $(TS_MODS)

beam: $(OBJS)

clean:
	rm -f $(OBJS)

%.beam: %.erl
	@echo -e "  ERLC\t"$<
	@$(ERLC) $<
