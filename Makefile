# ImplicitCAD Makefile. Build and test Implicitcad.


## Locations of binaries used when running tests, or generating the images to go along with our README.md.
# the location of stl2ps, from stltools, available from https://github.com/rsmith-nl/stltools/tree/develop
stl2ps=/disk4/faikvm.com/stltools/stltools/stl2ps.py
# the location of convert, from imagemagick
convert=convert
# the location of GHC, used to compile .hs examples.
GHC=ghc
# the location of the created extopenscad binary, for running shell based test cases.
EXTOPENSCAD=dist/build/extopenscad/extopenscad
# the location of the created test binary, for running haskell test cases.
TESTSUITE=dist/build/test-implicit/test-implicit
# the location of the documentation generator. for documenting (some of) the extopenscad languagi.
DOCGEN=dist/build/docgen/docgen

## options used when calling ImplicitCAD. for testing, and for image generation.
# enable multiple CPU usage.
RTSOPTS=+RTS -N
# the resolution to generate objects at. FIXME: what does this mean in human terms? 
RESOPTS=-r 50

#uncomment for profiling support. Note that you will need to recompile all of the libraries, as well.
#PROFILING= --enable-library-profiling --enable-executable-profiling

TARGETS=$(EXTOPENSCAD) $(TESTSUITE) $(DOCGEN)

# mark the below fake targets as unrean, so make will not get choked up if a file with one of these names is created.
.PHONY: build install clean distclean nukeclean docs dist examples tests

# build implicitcad binaries.
build: $(TARGETS)

# install implicitcad.
install: build
	cabal install

# cleanup from using the rules in this file.
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
	rm -rf docs/parser.md

# clean up before making a release.
distclean: clean
	rm -f `find ./ -name *~`
	rm -f `find ./ -name \#*\#`

# destroy the current user's cabal/ghc environment.
nukeclean: distclean
	rm -rf ~/.cabal/ ~/.ghc/

# Generate documentation.
docs: $(DOCGEN)
	./Setup haddock
	$(DOCGEN) > docs/iscad.md

# 
dist: $(TARGETS)
	./Setup sdist

# generate examples.
examples: $(EXTOPENSCAD)
	cd Examples && for each in `find ./ -name '*scad' -type f | sort`; do { valgrind --tool=cachegrind  --cachegrind-out-file=$$each.cachegrind.`date +%s` ../$(EXTOPENSCAD) $$each ${RTSOPTS}; } done
	cd Examples && for each in `find ./ -name '*.hs' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; $(GHC) $$filename.hs -o $$filename; $$filename; } done

images: examples
	cd Examples && for each in `find ./ -name '*.stl' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; if [ -e $$filename.transform ] ; then echo ${stl2ps} $$each $$filename.ps `cat $$filename.transform`; else ${stl2ps} $$each $$filename.ps; fi; ${convert} $$filename.ps $$filename.png; } done

tests: $(TESTSUITE)
#	cd tests && for each in `find ./ -name '*scad' -type f | sort`; do { ../$(EXTOPENSCAD) $$each ${RESOPTS} ${RTSOPTS}; } done
	./dist/build/test-implicit/test-implicit

# actually build a given binary.
dist/build/%: Setup dist/setup-config
	cabal build $(word 2,$(subst /, ,$*))

dist/setup-config: Setup implicit.cabal
	cabal update
	cabal install --only-dependencies --upgrade-dependencies
	cabal configure --enable-tests $(PROFILING)

Setup: Setup.*hs
	ghc -O2 -Wall --make Setup

