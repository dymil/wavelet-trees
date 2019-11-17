all: test

test: rank_support.cmx select_support.cmx test.cmx
	ocamlfind ocamlopt -g -linkpkg -package Bitv,OUnit2 $^ -o $@

test.cmx: test.ml rank/rank_support.mli select/select_support.mli
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv,OUnit2 $<

rank_support.cmi: rank/rank_support.mli
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

rank_support.cmx: rank/rank_support.ml rank_support.cmi
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

select_support.cmi: select/select_support.mli rank/rank_support.mli
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

select_support.cmx: select/select_support.ml rank_support.cmi select_support.cmi
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

.PHONY: clean

clean:
	rm -rf *.cmo *.cmi *.cmx *.o test.exe
