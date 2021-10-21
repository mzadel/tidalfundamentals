
MK=/usr/bin/make

all: tidaldiagram.svg

%: %.hs
	ghc --make $<

%.svg: %
	./$< -o $@
	open -a firefox $@

watch:
	while true; do $(MK) -q || $(MK); sleep 0.5; done

