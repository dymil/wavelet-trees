all: wt

test: rank_support.cmx select_support.cmx wavelet_tree.cmx test.cmx
	ocamlfind ocamlopt -g -linkpkg -package Bitv,OUnit2 $^ -o $@

test.cmx: test.ml rank_support.cmx select_support.cmx wavelet_tree.cmx
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv,OUnit2 $<

rank_support.cmi: rank/rank_support.mli
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

rank_support.cmx: rank/rank_support.ml rank_support.cmi
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

select_support.cmi: select/select_support.mli rank/rank_support.mli
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

select_support.cmx: select/select_support.ml rank_support.cmx select_support.cmi
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

wavelet_tree.cmi: wavelet_tree/wavelet_tree.mli
	ocamlfind ocamlopt -g -c $< -o $@

wavelet_tree.cmx: wavelet_tree/wavelet_tree.ml rank_support.cmx select_support.cmx wavelet_tree.cmi
	ocamlfind ocamlopt -g -c -linkpkg -package Bitv $< -o $@

wt: rank_support.cmx select_support.cmx wavelet_tree.cmx wt.ml
	ocamlfind ocamlopt -g -linkpkg -package Bitv str.cmxa $^ -o $@

.PHONY: clean

clean:
	rm -rf *.cmo *.cmi *.cmx *.o test.exe
