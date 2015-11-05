.PHONY: config build install clean docs dist test examples tests

RTSOPTS=+RTS -N

build config install docs clean dist test: Setup

build: dist/setup-config
	./Setup build

dist/setup-config: Setup *.cabal
	./Setup configure --prefix=$$PREFIX

install: build
	./Setup install

config: dist/setup-config

docs: config
	./Setup haddock

clean:
	./Setup clean

dist: build
	./Setup sdist

test: config
	./Setup test

examples: build
	cd Examples && for each in `find ./ -name '*scad' -type f | sort`; do { ../dist/build/extopenscad/extopenscad $$each ${RTSOPTS}; } done

tests: build
	cd Tests && for each in `find ./ -name '*scad' -type f | sort`; do { ../dist/build/extopenscad/extopenscad $$each ${RTSOPTS} ; } done


Setup: Setup.*hs
	ghc -O2 -Wall --make Setup

