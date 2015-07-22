#
# %CopyrightBegin%
#
# Copyright Ericsson AB 2015. All Rights Reserved.
#
# The contents of this file are subject to the Erlang Public License,
# Version 1.1, (the "License"); you may not use this file except in
# compliance with the License. You should have received a copy of the
# Erlang Public License along with this software. If not, it can be
# retrieved online at http://www.erlang.org/.
#
# Software distributed under the License is distributed on an "AS IS"
# basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
# the License for the specific language governing rights and limitations
# under the License.
#
# %CopyrightEnd%
#


ifndef EBIN
EBIN = ../ebin
endif

ifndef DOCS
DOCS = ../doc
endif

include $(ERL_TOP)/make/target.mk
include $(ERL_TOP)/make/$(TARGET)/otp.mk

# ----------------------------------------------------
# Application version
# ----------------------------------------------------
include ../vsn.mk
VSN=$(HIPE_VSN)

# ----------------------------------------------------
# Release directory specification
# ----------------------------------------------------
RELSYSDIR = $(RELEASE_PATH)/lib/hipe-$(VSN)

# ----------------------------------------------------
# Target Specs
# ----------------------------------------------------
# Please keep this list sorted.
MODULES=hipe_epiphany \
	hipe_epiphany_cfg \
	hipe_epiphany_defuse \
	hipe_epiphany_finalise \
	hipe_epiphany_frame \
	hipe_epiphany_liveness \
	hipe_epiphany_main \
	hipe_epiphany_pp \
	hipe_epiphany_ra \
	hipe_epiphany_ra_finalise \
	hipe_epiphany_ra_ls \
	hipe_epiphany_ra_naive \
	hipe_epiphany_ra_postconditions \
	hipe_epiphany_registers \
	hipe_rtl_to_epiphany

HRL_FILES=hipe_epiphany.hrl
ERL_FILES=$(MODULES:%=%.erl)
TARGET_FILES=$(MODULES:%=$(EBIN)/%.$(EMULATOR))
DOC_FILES= $(MODULES:%=$(DOCS)/%.html)

# ----------------------------------------------------
# FLAGS
# ----------------------------------------------------

include ../native.mk

ERL_COMPILE_FLAGS += +warn_exported_vars

# ----------------------------------------------------
# Targets
# ----------------------------------------------------

debug opt: $(TARGET_FILES)

docs:

# Moved docs target to edocs so the standard docs rule work properly.
edocs: $(DOC_FILES)

clean:
	rm -f $(TARGET_FILES)
	rm -f core

distclean: clean
realclean: clean

$(DOCS)/%.html:%.erl
	erl -noshell -run edoc_run file '"$<"' '[{dir, "$(DOCS)"}]' -s init stop

# ----------------------------------------------------
# Special Build Targets
# ----------------------------------------------------

# ----------------------------------------------------
# Release Target
# ----------------------------------------------------
include $(ERL_TOP)/make/otp_release_targets.mk

release_spec: opt
	$(INSTALL_DIR) "$(RELSYSDIR)/ebin"
	$(INSTALL_DATA) $(TARGET_FILES) "$(RELSYSDIR)/ebin"

release_docs_spec:

# Please keep this list sorted.
# $(EBIN)/hipe_epiphany_assemble.beam: ../main/hipe.hrl ../../kernel/src/hipe_ext_format.hrl ../rtl/hipe_literals.hrl
$(EBIN)/hipe_epiphany_cfg.beam: ../flow/cfg.hrl ../flow/cfg.inc
$(EBIN)/hipe_epiphany_frame.beam: ../rtl/hipe_literals.hrl
$(EBIN)/hipe_epiphany_liveness.beam: ../flow/liveness.inc
$(EBIN)/hipe_epiphany_registers.beam: ../rtl/hipe_literals.hrl
$(EBIN)/hipe_rtl_to_epiphany.beam: ../rtl/hipe_rtl.hrl

$(TARGET_FILES): hipe_epiphany.hrl ../misc/hipe_consttab.hrl
