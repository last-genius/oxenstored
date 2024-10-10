DUNE 	= dune
JOBS    = $(shell getconf _NPROCESSORS_ONLN)
PROFILE = release

.PHONY: build check test clean format install

build:
	$(DUNE) build -j $(JOBS) --profile=$(PROFILE)

install: build
	$(DUNE) install oxenstored

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
	git ls-files '**/*.[ch]' | xargs -n1 indent -nut -i8
