.PHONY: build install clean docs dist test examples tests

RTSOPTS=+RTS -N

RESOPTS=-r 10


# stl2ps, from stltools, available from https://github.com/rsmith-nl/stltools/tree/develop
stl2ps=/disk4/faikvm.com/stltools/stltools/stl2ps.py

# convert, from imagemagick
convert=convert

install: dist/build/extopenscad/extopenscad
	./Setup install

clean: Setup
	./Setup clean
	rm -rf Examples/*.stl
	rm -rf Examples/*.svg
	rm -rf Tests/*.stl
	rm -rf Setup

docs: dist/build/extopenscad/extopenscad
	./Setup haddock

dist: dist/build/extopenscad/extopenscad
	./Setup sdist

test: dist/build/extopenscad/extopenscad
	./Setup test

examples: dist/build/extopenscad/extopenscad
	cd Examples && for each in `find ./ -name '*scad' -type f | sort`; do { time ../dist/build/extopenscad/extopenscad $$each ${RTSOPTS}; } done

images:
	cd Examples && for each in `find ./ -name '*.stl' -type f | sort`; do { filename=$(basename "$$each"); filename="$${filename%.*}"; if [ -e $$filename.transform ] ; then echo ${stl2ps} $$each $$filename.ps `cat $$filename.transform`; else ${stl2ps} $$each $$filename.ps; fi; ${convert} $$filename.ps $$filename.png; } done

tests: dist/build/extopenscad/extopenscad
	cd Tests && for each in `find ./ -name '*scad' -type f | sort`; do { time ../dist/build/extopenscad/extopenscad $$each ${RESOPTS} ${RTSOPTS}; } done

dist/build/extopenscad/extopenscad: Setup dist/setup-config
	./Setup build

dist/setup-config: Setup implicit.cabal
	cabal configure

Setup: Setup.*hs
	ghc -O2 -Wall --make Setup

