all: ppm.cmi ppm.cmo traitement.cmi traitement.cmo
	ocamlc graphics.cma ppm.cmo traitement.cmo main.ml -o main

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
