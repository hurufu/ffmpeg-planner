.PHONY: run run-% clean

run: run-test01

run-%: %.sh
	sh --verbose $<

%.scm: ffmpeg.pddl %.pddl
	ff -o $< -f $*.pddl | sed -rne 's/[step 0-9]{9}: (.*)/(\1)/p' | tr '[:upper:]' '[:lower:]' >$@

%.sh: ffmpeg.pdb %.scm
	swipl -s $< -g "pio('$*.scm'),halt" >$@

clean: F := $(wildcard *.scm *.sh)
clean:
	$(if $(strip $F),rm -- $F,)
