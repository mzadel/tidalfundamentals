
MK=/usr/bin/make

output=diagram
modules=TidalPatternDiagram DiagramTable
diagrams=\
    basicpattern \
    tildeisarest \
    underscoreelongates \
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

all: basicpattern.svg

$(output): $(output).hs $(addsuffix .hs,$(modules))
	ghc --make $<

%.svg: $(output)
	./$< -S $(basename $@) -o $@

tidal.html: tidal.txt $(addsuffix .svg,$(diagrams))
	pandoc --metadata title="tidal" -f markdown -t html -s < $< > $@

watch:
	while true; do $(MK) -q || $(MK); sleep 0.5; done

clean:
	git clean -f $(wildcard $(output)*)
	git clean -f $(foreach module,$(modules),$(wildcard $(module)*))
	git clean -f *.svg
	git clean -f *.html

