.PHONY: config build install clean docs dist test

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

Setup: Setup.*hs
	ghc -O2 -Wall --make Setup

