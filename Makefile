
all: test

test: rank_support.cmo rank_support.cmi select_support.cmo select_support.cmi test.cmo test.cmi
	ocamlfind ocamlc -g -linkpkg -package Bitv,OUnit2 rank_support.cmo select_support.cmo test.cmo -o test

test.cmo: test.ml rank/rank_support.mli select/select_support.mli
	ocamlfind ocamlc -g -c -linkpkg -package Bitv,OUnit2 test.ml

rank_support.cmi: rank/rank_support.mli
	ocamlfind ocamlc -g -c -linkpkg -package Bitv rank/rank_support.mli -o $@

rank_support.cmo: rank/rank_support.ml rank_support.cmi
	ocamlfind ocamlc -g -c -linkpkg -package Bitv rank/rank_support.ml -o $@

select_support.cmi: select/select_support.mli rank/rank_support.mli
	ocamlfind ocamlc -g -c -linkpkg -package Bitv select/select_support.mli -o $@

select_support.cmo: select/select_support.ml rank/rank_support.mli select_support.cmi
	ocamlfind ocamlc -g -c -linkpkg -package Bitv select/select_support.ml -o $@

.PHONY: clean

clean:
	rm -rf *.cmo *.cmi
