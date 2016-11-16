all: ocyaml.cmi ocyaml.cma 

ocyaml.cma: ocyaml.cmo ocyaml.o 
	ocamlmklib -o ocyaml ocyaml.cmo ocyaml.o -lyaml

.SUFFIXES: .ml .mli .cmi .cmo .cmx .o

.ml.cmo: 
	ocamlc -c $< 

.mli.cmi:
	ocamlc -c $< 

.c.o:
	ocamlc -c $< 

clean:
	rm *.cmo *.cmi
