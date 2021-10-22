.PHONY: run run-% clean debug-% ace

run: run-test01
ace: test01.drs

run-%: %.arg
	ffmpeg $(file < $<)

%.scm: ffmpeg.pddl %.pddl
	ff -o $< -f $*.pddl | sed -rne 's/[step 0-9]{9}: (.*)/(\1)/p' | tr '[:upper:]' '[:lower:]' >$@

%.arg: ffmpeg.pdb %.scm
	swipl -s $< -g "pio('$*.scm'),halt" >$@

%.xml: lexicon.pdb %.ace
	ape -ulexfile $< -guess -file $*.ace -cdrs -cparaphrase >$@
%.drs: %.xml | debug-%
	xpath -e 'concat("drs(",string(//drs/text()),").")' $< >$@
	swipl -s $@ -g 'drs(P), print_term(P, [tab_width(0)]), format(".~n", []), halt.' | sponge $@
debug-%: %.xml
	xmllint -format $< | pygmentize -l xml

clean: F := $(wildcard *.scm *.arg *.drs *.xml)
clean:
	$(if $(strip $F),rm -- $F,)
