.PHONY: run run-% clean debug-% ace

run: run-test02

run-%: %.arg
	ffmpeg $(file < $<)

%.scm: ffmpeg.pddl %.pddl
	ff -o $< -f $*.pddl | tee /dev/stderr | sed -rne 's/[step 0-9]{9}: (.*)/(\1)/p' | tr '[:upper:]' '[:lower:]' >$@

%.arg: ffmpeg.pdb %.scm
	swipl -s $< -g "pio('$*.scm'),halt" >$@

%.pddl: ffmpeg-planner.pl pnf_handling.pdb %.ace
	swipl -s $< -g "main('$*.ace'),halt" >$@

clean: F := $(wildcard *.scm *.arg)
clean:
	$(if $(strip $F),rm -- $F,)
