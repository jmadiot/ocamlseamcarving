all: ppm.cmi ppm.cmo traitement.cmi traitement.cmo interface.cmi interface.cmo
	ocamlc graphics.cma ppm.cmo traitement.cmo interface.cmo main.ml -o main

interface.cmi:
	ocamlc -c interface.mli

interface.cmo: ppm.cmi
	ocamlc -c interface.ml

traitement.cmi:
	ocamlc -c traitement.mli

traitement.cmo:
	ocamlc -c traitement.ml

ppm.cmo:
	ocamlc ppm.ml -c

ppm.cmi:
	ocamlc ppm.mli -c

clean:
	rm *.cmi *.cmo

and: clean

so: all

on:
	./main
