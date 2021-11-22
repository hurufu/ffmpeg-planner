.SHELLFLAGS := $(.SHELLFLAGS) -o pipefail
ACE_FILES := $(wildcard *.ace)

.PHONY: run run-% clean

run: $(addprefix run-,$(ACE_FILES:.ace=))

run-%: %.arg
	ffmpeg $(file < $<)

%.txt: ffmpeg.pddl %.pddl
	ff -o $< -f $*.pddl | tee $@

%.scm: %.txt
	sed -rne 's/[step 0-9]{9}: (.*)/(\1)/p' <$< | tr '[:upper:]' '[:lower:]' >$@

%.arg: ffmpeg.pdb %.scm
	swipl -s $< -g "pio('$*.scm'),halt" >$@

%.pddl: ffmpeg-planner.pl fol.prolog %.ace
	swipl -s $< -g "debug,main('$*.ace'),halt" >$@

clean: F := $(wildcard *.scm *.arg *.txt $(ACE_FILES:.ace=.pddl))
clean:
	$(if $(strip $F),rm -- $F,)
