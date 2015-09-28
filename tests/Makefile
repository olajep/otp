TS_MODS=pdict_SUITE maps_SUITE map_SUITE basic_binary_SUITE bs_match_tail_SUITE
TS_MODS+=bs_bincomp_SUITE bs_match_bin_SUITE bs_SUITE

# binary_SUITE, bs_utf_SUITE, bs_match_misc_SUITE, bs_match_int_SUITE,
# bs_construct_SUITE are known to be broken with HiPE

TS_OBJS=$(addsuffix .beam,$(TS_MODS))
OBJS=regressions.beam unit.beam matmul.beam fannkuchredux.beam mandelbrot.beam \
	matchstate_align.beam \
	ts_epi.beam $(TS_OBJS)

include ../make/target.mk
include ../make/output.mk
include ../erts/emulator/slave/info.mk

ERL_TOP=$(shell readlink -f ..)

ifeq ("$(wildcard $(ERL_TOP)/bin/$(TARGET))","")
ifneq ("$(wildcard $(ERL_TOP)/bin/arm-unknown-linux-gnueabihf)","")
# Default target if the detected target is not built (say, if somebody
# cross-compiled the emulator we're currently testing)
override TARGET=arm-unknown-linux-gnueabihf
endif
endif

BINDIR=$(ERL_TOP)/bin/$(TARGET)
ERL=$(ERL_TOP)/bin/erl
ERLC=$(ERL_TOP)/bin/erlc

# Fully concurrent testing
FLAGS=--mods-conc

# Hipe targets whatever emulator is running it, so we need to build the debug
# files with debug erlc. An alternative would be to build a HiPE cross-compiler
# (not very easy without modifying the existing build scripts)
ERL_DEBUG=env ROOTDIR=$(ERL_TOP) BINDIR=$(BINDIR) EMU=beam.debug $(BINDIR)/erlexec
ERLC_DEBUG=${ERL_DEBUG} +sbtu +A0 -noinput -mode minimal -boot start_clean
ERLC_DEBUG+=-s erl_compile compile_cmdline -extra
SLAVE=$(ERL_TOP)/bin/epiphany-unknown-elf/slave.smp.srec

.PHONY: fast test beam compile clean

test: $(addprefix beam/,$(OBJS)) $(addprefix debug/,$(OBJS))
	TARGET=$(TARGET) SLAVE_TARGET=$(SLAVE_TARGET) \
		./run $(FLAGS) $(TS_MODS)

fast: $(addprefix beam/,$(OBJS)) $(addprefix opt/,$(OBJS))
	TARGET=$(TARGET) SLAVE_TARGET=$(SLAVE_TARGET) \
		./run --fast $(FLAGS) $(TS_MODS)

beam: $(addprefix beam/,$(OBJS))
	TARGET=$(TARGET) SLAVE_TARGET=$(SLAVE_TARGET) \
		./run --beam $(FLAGS) $(TS_MODS)

compile: $(addprefix beam/,$(OBJS)) $(addprefix opt/,$(OBJS)) \
	$(addprefix debug/,$(OBJS))

clean:
	rm -f $(OBJS) $(addprefix beam/,$(OBJS)) $(addprefix opt/,$(OBJS)) \
		$(addprefix debug/,$(OBJS))

beam/%.beam: %.erl
	$(erlc_verbose)cd beam && $(ERLC) ../$<

## These rules create the file before they succeed, make sure they won't stay if
## the rule fails
.DELETE_ON_ERROR:

opt/%.beam: beam/%.beam
	$(V_at)cp $< $@
	$(erlc_verbose)cd opt && \
		$(ERLC) +native +'{hipe,{target,epiphany}}' $(notdir $<)

debug/%.beam: beam/%.beam
	$(V_at)cp $< $@
	$(erlc_verbose)cd debug && \
		$(ERLC_DEBUG) +native +'{hipe,{target,epiphany}}' $(notdir $<)
