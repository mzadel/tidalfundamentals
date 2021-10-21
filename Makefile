
MK=/usr/bin/make

output=diagram
modules=TidalPatternDiagram

all: $(output).svg

$(output): $(output).hs $(addsuffix .hs,$(modules))
	ghc --make $<

$(output).svg: $(output)
	./$< -o $@
	open -a firefox $@

watch:
	while true; do $(MK) -q || $(MK); sleep 0.5; done

