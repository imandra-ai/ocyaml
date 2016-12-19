all: lint build test

lint:
	topkg lint

build:
	topkg build

clean:
	topkg clean

.PHONY: test
test:
	topkg test

install-deps:
	opam pin add . --no-action --yes
	opam depext ocyaml
	opam install ocyaml --deps-only

install: install-deps
	opam install ocyaml

uninstall:
	opam remove ocyaml
