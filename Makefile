all: test

test: rank_support.cmi rank_support.cmx select_support.cmi select_support.cmx test.cmx test.cmi
	ocamlfind ocamlopt -g -linkpkg -package Bitv,OUnit2 rank_support.cmx select_support.cmx test.cmx -o $@

test.cmx: test.ml rank/rank_support.mli select/select_support.mli
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv,OUnit2 test.ml

rank_support.cmi: rank/rank_support.mli
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv rank/rank_support.mli -o $@

rank_support.cmx: rank/rank_support.ml rank_support.cmi
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv rank/rank_support.ml -o $@

select_support.cmi: select/select_support.mli rank/rank_support.mli
	ocamlfind ocamlc -g -c -linkpkg -package Bitv select/select_support.mli -o $@

select_support.cmx: select/select_support.ml rank/rank_support.mli select_support.cmi
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv select/select_support.ml -o $@

.PHONY: clean

clean:
	rm -rf *.cmo *.cmi *.cmx *.o test.exe
