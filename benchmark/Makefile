SCALE_MODS=mandelbrot matmul
BENCH_MODS=binarytrees $(SCALE_MODS) fannkuchredux
## regexdna # not used -- spends all its time in PCRE anyway
## fasta # requires erts_internal:port_command/3, but crashes after a while with
	 # beam/erl_port_task.c:1533:erts_port_task_schedule(): Internal error:
	 # Missing run-queue
BENCH_OBJS=$(addsuffix .beam,$(BENCH_MODS))
OBJS=driver.beam colib.beam $(BENCH_OBJS)

EXTRA_OPT_OBJS=lists.beam epiphany.beam epiphany_server.beam
# re.beam

ERL_TOP=$(shell readlink -f ..)

include ../make/target.mk
include ../make/output.mk
include ../erts/emulator/slave/info.mk

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
SLAVE=$(ERL_TOP)/bin/epiphany-unknown-elf/slave.smp.srec
TEST =env SLAVE_BINARY=$(SLAVE) $(ERL) -noshell -s driver run
SCALE=env SLAVE_BINARY=$(SLAVE) $(ERL) -noshell -s driver scaling
TEST_ARM =$(ERL) -noshell -s driver run
SCALE_ARM=$(ERL) -noshell -s driver scaling

# Hipe targets whatever emulator is running it, so we need to build the debug
# files with debug erlc. An alternative would be to build a HiPE cross-compiler
# (not very easy without modifying the existing build scripts)
ERL_DEBUG=env ROOTDIR=$(ERL_TOP) BINDIR=$(BINDIR) EMU=beam.debug $(BINDIR)/erlexec
ERLC_DEBUG=${ERL_DEBUG} +sbtu +A0 -noinput -mode minimal -boot start_clean
ERLC_DEBUG+=-s erl_compile compile_cmdline -extra
SLAVE_DEBUG=$(ERL_TOP)/bin/epiphany-unknown-elf/slave.debug.smp.srec
TEST_DEBUG=env SLAVE_BINARY=$(SLAVE_DEBUG) $(ERL_DEBUG) -noshell -s driver run

.PHONY: all compile clean

all: beam.txt hipe.txt arm_beam.txt arm_hipe.txt scale_beam.txt scale_hipe.txt \
	arm_scale_beam.txt arm_scale_hipe.txt

.DELETE_ON_ERROR:

beam.txt: $(addprefix beam/,$(OBJS))
	$(gen_verbose)$(TEST) $@ $(BENCH_MODS) -pa beam

hipe.txt: $(addprefix opt/,$(OBJS) $(EXTRA_OPT_OBJS))
	$(gen_verbose)$(TEST) $@ $(BENCH_MODS) -pa opt

debug_hipe.txt: $(addprefix debug/,$(OBJS) $(EXTRA_OPT_OBJS))
	$(gen_verbose)$(TEST_DEBUG) $@ $(BENCH_MODS) -pa debug

arm_beam.txt: $(addprefix beam/,$(OBJS))
	$(gen_verbose)$(TEST_ARM) $@ $(BENCH_MODS) -pa beam

arm_hipe.txt: $(addprefix opt/,$(OBJS) $(EXTRA_OPT_OBJS))
	$(gen_verbose)$(TEST_ARM) $@ $(BENCH_MODS) -pa opt

scale_beam.txt: $(addprefix beam/,$(OBJS))
	$(gen_verbose)$(SCALE) $@ $(SCALE_MODS) -pa beam

scale_hipe.txt: $(addprefix opt/,$(OBJS) $(EXTRA_OPT_OBJS))
	$(gen_verbose)$(SCALE) $@ $(SCALE_MODS) -pa opt

arm_scale_beam.txt: $(addprefix beam/,$(OBJS))
	$(gen_verbose)$(SCALE_ARM) $@ $(SCALE_MODS) -pa beam

arm_scale_hipe.txt: $(addprefix opt/,$(OBJS) $(EXTRA_OPT_OBJS))
	$(gen_verbose)$(SCALE_ARM) $@ $(SCALE_MODS) -pa opt

compile: $(addprefix beam/,$(OBJS)) $(addprefix opt/,$(OBJS) $(EXTRA_OPT_OBJS))

compile_debug: $(addprefix debug/,$(OBJS) $(EXTRA_OPT_OBJS))

clean:
	rm -f $(OBJS) $(addprefix beam/,$(OBJS)) \
		$(addprefix opt/,$(OBJS) $(EXTRA_OPT_OBJS)) \
		$(addprefix debug/,$(OBJS) $(EXTRA_OPT_OBJS))

beam/%.beam: %.erl
	$(erlc_verbose)cd beam && $(ERLC) ../$<

opt/%.beam: beam/%.beam
	$(V_at)cp $< $@
	$(erlc_verbose)cd opt && \
		$(ERLC) +native $(notdir $<) && \
		$(ERLC) +native +'{hipe,{target,epiphany}}' $(notdir $<)

opt/%.beam: $(ERL_TOP)/lib/kernel/ebin/%.beam
	$(V_at)cp $< $@
	$(erlc_verbose)cd opt && \
		$(ERLC) +native $(notdir $<) && \
		$(ERLC) +native +'{hipe,{target,epiphany}}' $(notdir $<)

opt/%.beam: $(ERL_TOP)/lib/stdlib/ebin/%.beam
	$(V_at)cp $< $@
	$(erlc_verbose)cd opt && \
		$(ERLC) +native $(notdir $<) && \
		$(ERLC) +native +'{hipe,{target,epiphany}}' $(notdir $<)

debug/%.beam: beam/%.beam
	$(V_at)cp $< $@
	$(erlc_verbose)cd debug && \
		$(ERLC_DEBUG) +native $(notdir $<) && \
		$(ERLC_DEBUG) +native +'{hipe,{target,epiphany}}' $(notdir $<)

debug/%.beam: $(ERL_TOP)/lib/kernel/ebin/%.beam
	$(V_at)cp $< $@
	$(erlc_verbose)cd debug && \
		$(ERLC_DEBUG) +native $(notdir $<) && \
		$(ERLC_DEBUG) +native +'{hipe,{target,epiphany}}' $(notdir $<)

debug/%.beam: $(ERL_TOP)/lib/stdlib/ebin/%.beam
	$(V_at)cp $< $@
	$(erlc_verbose)cd debug && \
		$(ERLC_DEBUG) +native $(notdir $<) && \
		$(ERLC_DEBUG) +native +'{hipe,{target,epiphany}}' $(notdir $<)
