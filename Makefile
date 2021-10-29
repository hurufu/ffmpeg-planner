.PHONY: run run-% clean debug-% ace

run: run-test02
ace: test02.txt

run-%: %.arg
	ffmpeg $(file < $<)

%.scm: ffmpeg.pddl %.pddl
	ff -o $< -f $*.pddl | tee /dev/stderr | sed -rne 's/[step 0-9]{9}: (.*)/(\1)/p' | tr '[:upper:]' '[:lower:]' >$@

%.arg: ffmpeg.pdb %.scm
	swipl -s $< -g "pio('$*.scm'),halt" >$@

%.xml: %.ace
	ape -guess -file $*.ace -cpnf -cparaphrase >$@
%.pnf: %.xml | debug-%
	xpath -q -e 'concat("pnf(",string(//pnf/text()),").")' $< >$@
	swipl -s $@ -g 'pnf(P), print_term(pnf(P), [tab_width(0)]), format(".~n", []), halt.' | sponge $@
%.pddl: pnf_handling.pdb %.pnf
	swipl -s $< -g "main('$*.pnf'),halt." >$@
%.txt: %.ace
	acerules $< $@
debug-%: %.xml
	xmllint -format $< | pygmentize -l xml

clean: F := $(wildcard *.scm *.arg *.xml *.txt *.pnf)
clean:
	$(if $(strip $F),rm -- $F,)
