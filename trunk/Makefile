all: ppm.cmi ppm.cmo traitement.cmi traitement.cmo interface.cmi interface.cmo
	ocamlc graphics.cma ppm.cmo traitement.cmo interface.cmo main.ml -o main

interface.cmi:
	ocamlc -i interface.ml > interface.mli
	ocamlc -c interface.mli

interface.cmo:
	ocamlc -c interface.ml

traitement.cmi:
	ocamlc -i traitement.ml > traitement.mli
	ocamlc -c traitement.mli

traitement.cmo:
	ocamlc -c traitement.ml

ppm.cmo:
	ocamlc ppm.ml -c

ppm.cmi:
	ocamlc -i ppm.ml > ppm.mli
	ocamlc ppm.mli -c

clean:
	rm *.cmi *.cmo

and: clean

so: all

on:
	./main
