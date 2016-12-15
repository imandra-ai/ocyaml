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

install:
	opam pin add . --no-action
	opam depext ocyaml
	opam install ocyaml

uninstall:
	opam remove ocyaml
