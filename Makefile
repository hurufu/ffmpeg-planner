.SHELLFLAGS := $(.SHELLFLAGS) -o pipefail

ACE_DIR     := texts
ACE_FILES   := $(wildcard $(ACE_DIR)/*.ace)
RUN_TARGETS := $(addprefix run-,$(patsubst $(ACE_DIR)/%.ace,%,$(ACE_FILES)))
FF          ?= ff

vpath %.ace $(ACE_DIR)
.PHONY: run run-% clean

run: $(RUN_TARGETS)

run-%: %.arg
	ffmpeg $(file < $<)

%.txt: ffmpeg.pddl %.pddl
	$(FF) -o $< -f $*.pddl | tee $@

%.scm: %.txt
	sed -rne 's/[step 0-9]{9}: (.*)/(\1)/p' <$< | tr '[:upper:]' '[:lower:]' >$@

%.arg: ffmpeg.pdb %.scm
	swipl -s $< -g "pio('$*.scm'),halt" >$@

%.pddl: ffmpeg-planner.pl fol.prolog %.ace
	swipl -s $< -g "debug,main('$(word 3,$^)'),halt" >$@

clean: F := $(wildcard *.scm *.arg *.txt $(patsubst $(ACE_DIR)/%.ace,%.pddl,$(ACE_FILES)))
clean:
	$(if $(strip $F),rm -- $F,)
