
MK=/usr/bin/make

diagramexecutable=diagram
modules=TidalPatternDiagram DiagramTable PatternAlgebraDiagrams

document=tidal
diagrams=\
    mnocycle \
    basicpattern \
    tildeisarest \
    underscoreelongates \
    atelongates \
    repeateventasterisk \
    repeateventbang \
    squarebrackets \
    thedot \
    commaforparallel \
    polymetricbraces \
    polymetricbracesotherorder \
    polymetricdividebyeight \
    polymetricdividebyseven \
    anglebrackets \
    euclideanrhythm \
    slowfunction \
    slowoneandahalf \
    slowoneandahalfoneeighthticks \
    fastfunction \
    fastgapfunction \
    compressfunction \
    zoomfunction \
    revfunctioninput \
    revfunctionoutput \
    leftPlusExample1

all: $(document).html

$(diagramexecutable): $(diagramexecutable).hs $(addsuffix .hs,$(modules))
	ghc --make $<

%.svg: $(diagramexecutable)
	./$< -S $(basename $@) -o $@

$(document).html: $(document).txt $(addsuffix .svg,$(diagrams))
	pandoc -f markdown -t html -s < $< > $@

watch:
	while true; do $(MK) -q || $(MK); sleep 0.5; done

clean:
	git clean -f $(wildcard $(diagramexecutable)*)
	git clean -f $(foreach module,$(modules),$(wildcard $(module)*))
	git clean -f *.svg
	git clean -f *.html

