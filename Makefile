#
# This Makefile is not called from Opam but only used for
# convenience during development
#

DUNE 	= dune
JOBS    = $(shell getconf _NPROCESSORS_ONLN)
PROFILE = release

.PHONY: build check test clean format

build:
	$(DUNE) build -j $(JOBS) --profile=$(PROFILE)

check:
	dune build @check -j $(JOBS)

test:
	$(DUNE) runtest

deps:
	opam install . --deps-only

clean:
	$(DUNE) clean

utop:
	$(DUNE) utop

format:
	$(DUNE) build --auto-promote @fmt
	dune format-dune-file dune-project > $$$$ && mv $$$$ dune-project

# vim:ts=8:noet:
