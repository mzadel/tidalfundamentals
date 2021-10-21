
MK=/usr/bin/make

output=diagram
modules=TidalPatternDiagram

all: show

$(output): $(output).hs $(addsuffix .hs,$(modules))
	ghc --make $<

$(output).svg: $(output)
	./$< -o $@

show: $(output).svg
	open -a firefox $<

watch:
	while true; do $(MK) -q || $(MK); sleep 0.5; done

clean:
	git clean -f $(wildcard $(output)*)
	git clean -f $(foreach module,$(modules),$(wildcard $(module)*))

