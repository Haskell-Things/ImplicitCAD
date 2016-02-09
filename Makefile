.PHONY: build install clean docs dist test examples tests

RTSOPTS=+RTS -N

RESOPTS=-r 10


# stl2ps, from stltools, available from https://github.com/rsmith-nl/stltools/tree/develop
stl2ps=/disk4/faikvm.com/stltools/stltools/stl2ps.py

# convert, from imagemagick
convert=convert

EXTOPENSCAD=dist/build/extopenscad/extopenscad

install: $(EXTOPENSCAD)
	./Setup install

clean: Setup
	./Setup clean
	rm -rf Examples/*.stl
	rm -rf Examples/*.svg
	rm -rf Examples/*.ps
	rm -rf Examples/*.png
	rm -rf tests/*.stl
	rm -rf Setup

docs: $(EXTOPENSCAD)
	./Setup haddock

dist: $(EXTOPENSCAD)
	./Setup sdist

test: $(EXTOPENSCAD)
	./Setup test

examples: $(EXTOPENSCAD)
	cd Examples && for each in `find ./ -name '*scad' -type f | sort`; do { time ../$(EXTOPENSCAD) $$each ${RTSOPTS}; } done

images:
	cd Examples && for each in `find ./ -name '*.stl' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; if [ -e $$filename.transform ] ; then echo ${stl2ps} $$each $$filename.ps `cat $$filename.transform`; else ${stl2ps} $$each $$filename.ps; fi; ${convert} $$filename.ps $$filename.png; } done

tests: $(EXTOPENSCAD)
	cd tests && for each in `find ./ -name '*scad' -type f | sort`; do { time ../$(EXTOPENSCAD) $$each ${RESOPTS} ${RTSOPTS}; } done

dist/build/extopenscad/extopenscad: Setup dist/setup-config
	./Setup build

dist/setup-config: Setup implicit.cabal
	cabal configure

Setup: Setup.*hs
	ghc -O2 -Wall --make Setup

