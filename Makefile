
MK=/usr/bin/make

-include Makefile.diagrams

diagramexecutable=diagram
modules=Shared LinearDiagrams PatternAlgebraDiagrams CircularDiagrams DiagramTable

document=tidal

all: $(document).html

Makefile.diagrams PatternExpressions.hs: $(document).txt
	pandoc -f markdown -t native --lua-filter getdiagrams.lua < $< > /dev/null

PatternExpressions.o: PatternExpressions.hs
	ghc --make -c -Wall -Wno-missing-signatures $<

$(diagramexecutable): $(diagramexecutable).hs PatternExpressions.o $(addsuffix .hs,$(modules))
	ghc --make -Wall $<

%.svg: $(diagramexecutable)
	./$< -S $(basename $@) -o $@

$(document).html: $(document).txt $(addsuffix .svg,$(diagrams))
	pandoc -f markdown -t html -s --lua-filter renderghcisessions.lua < $< > $@

watch:
	while true; do $(MK) -q || $(MK); sleep 0.5; done

clean:
	git clean -f $(wildcard $(diagramexecutable)*)
	git clean -f $(foreach module,$(modules),$(wildcard $(module)*))
	git clean -f PatternExpressions.*
	git clean -f *.svg
	git clean -f *.html
	git clean -f ghci.input ghci.output
	git clean -f Makefile.diagrams
	git clean -f whitelistexists

