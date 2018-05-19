.PHONY: build install clean docs dist test examples tests

RTSOPTS=+RTS -N

RESOPTS=-r 50

#uncomment for profiling support.
#PROFILING= --enable-library-profiling --enable-executable-profiling

# stl2ps, from stltools, available from https://github.com/rsmith-nl/stltools/tree/develop
stl2ps=/disk4/faikvm.com/stltools/stltools/stl2ps.py

# convert, from imagemagick
convert=convert

EXTOPENSCAD=dist/build/extopenscad/extopenscad
TESTSUITE=dist/build/test-implicit/test-implicit
TARGETS=$(EXTOPENSCAD) $(TESTSUITE)

# FIXME: this used to be ./Setup install. what's going on?
install: $(TARGETS)
	cabal install

clean: Setup
	./Setup clean
	rm -f Examples/*.stl
	rm -f Examples/*.svg
	rm -f Examples/*.ps
	rm -f Examples/*.png
	rm -f Examples/example[0-9][0-9]
	rm -f Examples/*.hi
	rm -f Examples/*.o
	rm -f tests/*.stl
	rm -f Setup Setup.hi Setup.o
	rm -rf dist/*

distclean: clean
	rm -f `find ./ -name *~`
	rm -f `find ./ -name \#*\#`

nukeclean: distclean
	rm -rf ~/.cabal/ ~/.ghc/


docs: $(TARGETS)
	./Setup haddock

dist: $(TARGETS)
	./Setup sdist

#test: $(TARGETS)
#	./Setup test

examples: $(TARGETS)
	cd Examples && for each in `find ./ -name '*scad' -type f | sort`; do { valgrind --tool=cachegrind  --cachegrind-out-file=$$each.cachegrind.`date +%s` ../$(EXTOPENSCAD) $$each ${RTSOPTS}; } done
	cd Examples && for each in `find ./ -name '*.hs' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; ghc $$filename.hs -o $$filename; $$filename; } done

images:
	cd Examples && for each in `find ./ -name '*.stl' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; if [ -e $$filename.transform ] ; then echo ${stl2ps} $$each $$filename.ps `cat $$filename.transform`; else ${stl2ps} $$each $$filename.ps; fi; ${convert} $$filename.ps $$filename.png; } done

tests: $(TARGETS)
#	cd tests && for each in `find ./ -name '*scad' -type f | sort`; do { ../$(EXTOPENSCAD) $$each ${RESOPTS} ${RTSOPTS}; } done
	./dist/build/test-implicit/test-implicit

dist/build/extopenscad/extopenscad: Setup dist/setup-config
	cabal build

dist/build/test-implicit/test-implicit: Setup dist/setup-config
	cabal build

dist/setup-config: Setup implicit.cabal
	cabal update
	cabal install --only-dependencies --upgrade-dependencies
	cabal configure --enable-tests $(PROFILING)

Setup: Setup.*hs
	ghc -O2 -Wall --make Setup

