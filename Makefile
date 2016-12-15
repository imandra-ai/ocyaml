all: lint build

lint:
	topkg lint

build:
	topkg build

clean:
	topkg clean

install:
	opam pin add . --no-action
	opam depext ocyaml
	opam install ocyaml

uninstall:
	opam remove ocyaml
