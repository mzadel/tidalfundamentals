
MK=/usr/bin/make

-include Makefile.diagrams
-include Makefile.replsessions

diagramexecutable=diagram
modules=Shared LinearDiagrams PatternAlgebraDiagrams SignalDiagrams CircularDiagrams DiagramTable

document=tidal

all: $(document).html

Makefile.diagrams Makefile.replsessions PatternExpressions.hs: $(document).txt
	pandoc -f markdown -t native --lua-filter getdiagrams.lua < $< > /dev/null

PatternExpressions.o: PatternExpressions.hs
	ghc --make -c -Wall -Wno-missing-signatures $<

$(diagramexecutable): $(diagramexecutable).hs PatternExpressions.o $(addsuffix .hs,$(modules))
	ghc --make -Wall $<

%.svg: $(diagramexecutable)
	./$< -S $(basename $@) -o $@

ghci-output-%.txt: | ghci-input-%.txt
	TERM=xterm script -q $@ ghci < $| > /dev/null

tidal-output-%.txt: | tidal-input-%.txt
	TERM=xterm script -q $@ ghci -ghci-script BootTidal.hs < $| > /dev/null

$(document).html: $(document).txt $(addsuffix .svg,$(diagrams)) $(replsessions)
	pandoc -f markdown -t html -s --toc --lua-filter renderghcisessions.lua < $< > $@

watch:
	while true; do $(MK) -q || $(MK); sleep 0.5; done

clean:
	git clean -f $(wildcard $(diagramexecutable)*)
	git clean -f $(foreach module,$(modules),$(wildcard $(module)*))
	git clean -f PatternExpressions.*
	git clean -fq *.svg
	git clean -f *.html
	git clean -f Makefile.diagrams Makefile.replsessions
	git clean -f whitelistexists
	git clean -fq ghci-input-*.txt tidal-input-*.txt

cleanall: clean
	git clean -fxq ghci-output-*.txt tidal-output-*.txt

