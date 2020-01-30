main: types.cmx precond.cmx
	ocamlopt -o $@ $^
%.cmx: %.ml
	ocamlopt -c $@ $^
