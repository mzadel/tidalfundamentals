
MK=/usr/bin/make

diagramexecutable=diagram
modules=TidalPatternDiagram DiagramTable

document=tidal
diagrams=\
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
    euclideanrhythm

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

