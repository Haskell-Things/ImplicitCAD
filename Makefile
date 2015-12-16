.PHONY: build install clean docs dist test examples tests

RTSOPTS=+RTS -N

RESOPTS=-r 10

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

tests: dist/build/extopenscad/extopenscad
	cd Tests && for each in `find ./ -name '*scad' -type f | sort`; do { time ../dist/build/extopenscad/extopenscad $$each ${RESOPTS} ${RTSOPTS}; } done

dist/build/extopenscad/extopenscad: Setup dist/setup-config
	./Setup build

dist/setup-config: Setup implicit.cabal
	cabal configure

Setup: Setup.*hs
	ghc -O2 -Wall --make Setup

