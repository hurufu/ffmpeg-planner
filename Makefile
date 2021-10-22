.PHONY: run run-% clean

run: run-test01

run-%: %.arg
	ffmpeg $(file < $<)

%.scm: ffmpeg.pddl %.pddl
	ff -o $< -f $*.pddl | sed -rne 's/[step 0-9]{9}: (.*)/(\1)/p' | tr '[:upper:]' '[:lower:]' >$@

%.arg: ffmpeg.pdb %.scm
	swipl -s $< -g "pio('$*.scm'),halt" >$@

%.pnf: %.ace
	ape -guess -file $< -cpnf | xpath -e 'concat("pnf(",string(//pnf/text()),").")' >$@
	swipl -s $@ -g 'pnf(P), print_term(pnf(P), [tab_width(0)]), format(".~n", []), halt.' | sponge $@

clean: F := $(wildcard *.scm *.arg *.pnf)
clean:
	$(if $(strip $F),rm -- $F,)
