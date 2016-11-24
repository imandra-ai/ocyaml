artifacts = *.a *.cma *.cmi *.cmo *.cmx *.cmxa *.o *.so

all: ocyaml.cmi ocyaml.cma

ocyaml.cma: ocyaml.cmo ocyaml.o
	ocamlmklib -o ocyaml -lyaml $^

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

%.o: %.c
	ocamlc -c $<

clean:
	rm $(wildcard $(artifacts))

install:
	ocamlfind install ocyaml META $(wildcard $(artifacts))

uninstall:
	ocamlfind remove ocyaml
