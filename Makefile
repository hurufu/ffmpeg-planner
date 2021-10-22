.PHONY: run run-% clean debug-% ace

run: run-test01
ace: test01.pnf

run-%: %.arg
	ffmpeg $(file < $<)

%.scm: ffmpeg.pddl %.pddl
	ff -o $< -f $*.pddl | sed -rne 's/[step 0-9]{9}: (.*)/(\1)/p' | tr '[:upper:]' '[:lower:]' >$@

%.arg: ffmpeg.pdb %.scm
	swipl -s $< -g "pio('$*.scm'),halt" >$@

%.xml: %.ace
	ape -guess -file $< -cdrspp -cparaphrase -cpnf >$@
%.pnf: %.xml | debug-%
	xpath -e 'concat("pnf(",string(//pnf/text()),").")' $< >$@
	swipl -s $@ -g 'pnf(P), print_term(pnf(P), [tab_width(0)]), format(".~n", []), halt.' | sponge $@
debug-%: %.xml
	xmllint -format $< | pygmentize -l xml

clean: F := $(wildcard *.scm *.arg *.pnf *.xml)
clean:
	$(if $(strip $F),rm -- $F,)
