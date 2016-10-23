.PHONY: build install clean docs dist test examples tests

RTSOPTS=+RTS -N

RESOPTS=-r 10

#uncomment for profiling support.
#PROFILING= --enable-library-profiling --enable-executable-profiling

# stl2ps, from stltools, available from https://github.com/rsmith-nl/stltools/tree/develop
stl2ps=/disk4/faikvm.com/stltools/stltools/stl2ps.py

# convert, from imagemagick
convert=convert

EXTOPENSCAD=dist/build/extopenscad/extopenscad

install: $(EXTOPENSCAD)
	./Setup install

clean: Setup
	./Setup clean
	rm -f Examples/*.stl
	rm -f Examples/*.svg
	rm -f Examples/*.ps
	rm -f Examples/*.png
	rm -f Examples/example[0-9][0-9]
	rm -f tests/*.stl
	rm -f Setup Setup.hi Setup.o

distclean: clean
	rm -f `find ./ -name *~`
	rm -f `find ./ -name \#*\#`


docs: $(EXTOPENSCAD)
	./Setup haddock

dist: $(EXTOPENSCAD)
	./Setup sdist

test: $(EXTOPENSCAD)
	./Setup test

examples: $(EXTOPENSCAD)
	cd Examples && for each in `find ./ -name '*scad' -type f | sort`; do { time ../$(EXTOPENSCAD) $$each ${RTSOPTS}; } done
	cd Examples && for each in `find ./ -name '*.hs' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; ghc $$filename.hs -o $$filename; $$filename; } done

images:
	cd Examples && for each in `find ./ -name '*.stl' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; if [ -e $$filename.transform ] ; then echo ${stl2ps} $$each $$filename.ps `cat $$filename.transform`; else ${stl2ps} $$each $$filename.ps; fi; ${convert} $$filename.ps $$filename.png; } done

tests: $(EXTOPENSCAD)
	cd tests && for each in `find ./ -name '*scad' -type f | sort`; do { time ../$(EXTOPENSCAD) $$each ${RESOPTS} ${RTSOPTS}; } done

dist/build/extopenscad/extopenscad: Setup dist/setup-config
	cabal build

dist/setup-config: Setup implicit.cabal
	cabal install --only-dependencies
	cabal configure $(PROFILING)

Setup: Setup.*hs
	ghc -O2 -Wall --make Setup

