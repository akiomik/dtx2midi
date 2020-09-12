.PHONY: build test lint fix

build:
	stack build

test:
	stack test

lint:
	hlint .

fix:
	ormolu --mode inplace `find . -name '*.hs'`
