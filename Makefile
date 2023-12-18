# ImplicitCAD Makefile. Build and test Implicitcad.

## This is the makefile if you are running cabal-install 1.24 or later.

## Locations of binaries used when running tests, or generating the images to go along with our README.md.
# The location of stl2ps, from stltools, available from https://github.com/rsmith-nl/stltools/tree/develop
stl2ps=/disk4/faikvm.com/stltools/stltools/stl2ps.py
# The location of convert, from imagemagick
convert=convert
# The location of GHC, used to compile .hs examples.
GHC=ghc
GHCVERSION=$(shell ${GHC} --version | sed "s/.*version //")
CABAL=cabal
CABALVERSION=$(shell ${CABAL} --version | sed -n "s/.*install version \([0-9.]*\).*/\1/p")
IMPLICITVERSION=$(shell cat implicit.cabal | sed -n "s/Version[: ]*\([0-9]*.*\)/\1/p")
ARCHITECTURE=$(shell uname -m | sed "s/i[3-6]86/i386/" )
# FIXME: detect this on other OSes.
OS=linux
# new-style location root. must NOT have trailing slash
BUILDROOT=dist-newstyle/build/${ARCHITECTURE}-${OS}/ghc-${GHCVERSION}/implicit-${IMPLICITVERSION}
EXEBUILDROOT=${BUILDROOT}/x/
TESTBUILDROOT=${BUILDROOT}/t/
BENCHBUILDROOT=${BUILDROOT}/b/

exebin = ${EXEBUILDROOT}/$(1)/build/$(1)/$(1)
exedir = ${EXEBUILDROOT}/$(1)

# The location of the created extopenscad binary, for running shell based test cases.
EXTOPENSCAD=extopenscad
EXTOPENSCADBIN=$(call exebin,${EXTOPENSCAD})
EXTOPENSCADDIR=$(call exedir,${EXTOPENSCAD})
# The location of the implicitsnap binary, which listens for requests via http. The backend of the website.
IMPLICITSNAP=implicitsnap
IMPLICITSNAPBIN=$(call exebin,${IMPLICITSNAP})
IMPLICITSNAPDIR=$(call exedir,${IMPLICITSNAP})
# The location of the benchmark binary, for benchmarking some implicitcad internals.
BENCHMARK=Benchmark
BENCHMARKBIN=${BENCHBUILDROOT}/Benchmark/build/Benchmark/Benchmark
BENCHMARKDIR=${BENCHBUILDROOT}/Benchmark
# The location of the documentation generator. for documenting (some of) the extopenscad language.
DOCGEN=docgen
DOCGENBIN=$(call exebin,${DOCGEN})
DOCGENDIR=$(call exedir,${DOCGEN})
# The location of the parser benchmark binary, specifically for benchmarking implicitcad's parser.
PARSERBENCH=${BENCHBUILDROOT}/parser-bench/build/parser-bench/parser-bench
PARSERBENCHDIR=${BENCHBUILDROOT}/parser-bench
# The location of the created test binary, for running haskell test cases.
TESTSUITE=${TESTBUILDROOT}/test-implicit/build/test-implicit/test-implicit
TESTSUITEDIR=${TESTBUILDROOT}/test-implicit
# The location of it's source.
TESTFILES=$(shell find tests/ -name '*.hs')

## Options used when calling ImplicitCAD. for testing, and for image generation.
# Enable multiple CPU usage.
# Use the parallel garbage collector.
# spit out some performance statistics.
RTSOPTS=+RTS -N -qg -t
# The resolution to generate objects at. FIXME: what does this mean in human terms? 
RESOPTS=-r 50

SCADOPTS?=-q

# Uncomment for profiling support. Note that you will need to recompile all of the libraries, as well.
#PROFILING= --enable-profiling

## FIXME: escape this right
# Uncomment for valgrind on the examples.
#VALGRIND=valgrind --tool=cachegrind --cachegrind-out-file=$$each.cachegrind.`date +%s`

LIBDIR=Graphics
LIBFILES=$(shell find ${LIBDIR} -name '*.hs')
LIBBUILDS=$(shell find ${LIBDIR} -name '*.hi' -o -name '*.o')
LIBTARGET=${BUILDROOT}/build/Graphics/Implicit.o

# don't try to compile implicitsnap unless the flag for compiling it has been set.
MAYBEIMPLICITSNAPBIN=$(shell bash -c "[ -n \"$$([ -f cabal.project.local ] && cat cabal.project.local | sed -n '/flags: .*+implicitsnap.*/p')\" ] && echo ${IMPLICITSNAPBIN}" )

EXECTARGETS=$(EXTOPENSCADBIN) $(MAYBEIMPLICITSNAPBIN) $(BENCHMARKBIN) $(TESTSUITE) $(PARSERBENCH) $(DOCGENBIN)
EXECBUILDDIRS=$(EXTOPENSCADDIR) $(IMPLICITSNAPDIR) $(BENCHMARKDIR) $(DOCGENDIR)
TARGETS=$(EXECTARGETS) $(LIBTARGET)

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

# Options to GHC during compilation of our examples.
EXAMPLEOPTS=-package linear -package show-combinators -package lens -package blaze-svg -package data-default -package JuicyPixels

# Mark the below fake targets as unreal, so make will not get choked up if a file with one of these names is created.
.PHONY: build install clean distclean nukeclean docs dist examples tests benchmarks benchmarkdir

# Empty out the default suffix list, to make debugging output cleaner.
.SUFFIXES:

# Allow for us to (ab)use $$* in dependencies of rules.
.SECONDEXPANSION:

# Disable make's default builtin rules, to make debugging output cleaner.
MAKEFLAGS += --no-builtin-rules

# Build implicitcad binaries.
build: ensureBootstrap $(TARGETS)

# Ensure the haskell environment is sane
ensureBootstrap:
	@if [ "${CABALVERSION}" = "2.4.0.0" ] && [ ! -d ~/.cabal/store/ghc-${GHCVERSION}/package.db ] ; then { echo "please create ~/.cabal/store/ghc-${GHCVERSION}/package.db , to workaround a known cabal bug." ;  error 1 ; } fi

# Install implicitcad.
install: build
	cabal install

# Cleanup from using the rules in this file.
clean:
	rm -f Examples/*.stl
	rm -f Examples/*.svg
	rm -f Examples/*.ps
	rm -f Examples/*.png
	rm -f Examples/example[0-9][0-9]
	rm -f Examples/*.hi
	rm -f Examples/*.o
	rm -f Examples/example*.cachegrind.*
	rm -f tests/*.stl
	rm -rf docs/parser.md
	rm -f ${TARGETS}
	rm -f ${LIBBUILDS}
	rm -f benchmarks
	rm -rf ${EXECBUILDDIRS} ${PARSERBENCHDIR} ${TESTSUITEDIR}
	rm -f ${BUILDROOT}/build/libHS*
	rm -f ${BUILDROOT}/cache/registration

# Clean up before making a release.
distclean: clean
	cabal clean
	rm -rf dist
	rm -rf dist-newstyle
	rm -rf .stack-work
	rm -f cabal.project.local
	rm -f .ghc.environment.${ARCHITECTURE}-${OS}-${GHCVERSION}
	rm -f `find ./ -name "*~"`
	rm -f `find ./ -name "\#*\#"`

# Destroy the current user's cabal/ghc environment.
nukeclean: distclean
	rm -rf ~/.cabal/ ~/.ghc/

# Generate documentation.
docs: $(DOCGENBIN)
	cabal haddock --enable-documentation
	$(DOCGENBIN) > docs/escad.md

# Upload to hackage?
dist: $(TARGETS)
	cabal sdist

# Generate examples.
examples: $(EXTOPENSCADBIN)
	cd Examples && for each in `find ./ -name '*scad' -type f | sort`; do { echo $$each ; ../$(EXTOPENSCADBIN) $(SCADOPTS) $$each $(RTSOPTS); } done
	# NOTE: on debian, if this fails to find the linear package, run: 'apt install libghc-linear-dev libghc-show-combinators-dev libghc-blaze-svg-dev libghc-data-default-dev libghc-juicypixels-dev'
	cd Examples && for each in `find ./ -name '*.hs' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; cd ..; $(GHC) $(EXAMPLEOPTS) Examples/$$filename.hs -o Examples/$$filename; cd Examples; echo $$filename; $$filename +RTS -t ; } done

# Generate images from the examples, so we can upload the images to our website.
images: examples
	cd Examples && for each in `find ./ -name '*.stl' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; if [ -e $$filename.transform ] ; then echo ${stl2ps} $$each $$filename.ps `cat $$filename.transform`; else ${stl2ps} $$each $$filename.ps; fi; ${convert} $$filename.ps $$filename.png; } done

# Hspec parser tests.
tests: $(TESTSUITE) $(TESTFILES)
#	cd tests && for each in `find ./ -name '*scad' -type f | sort`; do { ../$(EXTOPENSCADBIN) $$each ${RESOPTS} ${RTSOPTS}; } done
	$(TESTSUITE)

benchmarkdir:
	[ ! -e benchmarks ] && bash -c 'rm -f benchmarks; ln -s `mktemp --tmpdir -d icad-XXX` benchmarks' || true

benchmarks: $(BENCHMARKBIN) benchmarkdir
	cd benchmarks && ${ROOT_DIR}/${BENCHMARKBIN}

# The ImplicitCAD library.
$(LIBTARGET): $(LIBFILES)
	cabal new-build implicit

# The parser test suite, since it's source is stored in a different location than the other binaries we build:
${TESTBUILDROOT}/test-implicit/build/test-implicit/test-implicit: $(TESTFILES) ${BUILDROOT}/setup-config $(LIBTARGET) $(LIBFILES)
	cabal new-build test-implicit

# Build a binary target with cabal.
${EXEBUILDROOT}/%: programs/$$(word 1,$$(subst /, ,%)).hs ${BUILDROOT}/setup-config $(LIBTARGET) $(LIBFILES)
	cabal new-build $(word 1,$(subst /, ,$*))
	touch $@

# Build a benchmark target with cabal.
${BENCHBUILDROOT}/%: programs/$$(word 1,$$(subst /, ,%)).hs ${BUILDROOT}/setup-config $(LIBTARGET) $(LIBFILES)
	cabal new-build $(word 1,$(subst /, ,$*))

# Prepare to build.
${BUILDROOT}/setup-config: implicit.cabal
	cabal new-update
	cabal new-install --only-dependencies --upgrade-dependencies --overwrite-policy=always $(PROFILING)
	cabal new-configure --enable-tests --enable-benchmarks $(PROFILING)
