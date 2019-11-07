
all: test

test: rank_support.cmo rank_support.cmi select_support.cmo select_support.cmi test_rank.cmo test_rank.cmi
	ocamlfind ocamlc -g -linkpkg -package Bitv,OUnit2 rank_support.cmo select_support.cmo test_rank.cmo -o test

test_rank.cmo: test_rank.ml rank_support.ml select_support.ml
	ocamlfind ocamlc -g -c -linkpkg -package Bitv,OUnit2 test_rank.ml

rank_support.cmo: rank_support.ml
	ocamlfind ocamlc -g -c -linkpkg -package Bitv rank_support.ml

select_support.cmo: select_support.ml rank_support.ml
	ocamlfind ocamlc -g -c -linkpkg -package Bitv select_support.ml

.PHONY: clean

clean:
	rm *.cmo *.cmi
