LIBS_NAME=types
LIBS=$(patsubst %,%.cmx,$(LIBS_NAME))

main: $(LIBS) precond.cmx
	ocamlopt -o $@ $^

%.cmx: %.ml
	ocamlopt -o $@ -c $<

%.cmi: %.mli
	ocamlopt -o $@ -c $<

-include .depend
.depend: $(wildcard *.ml) $(wildcard *.mli)
	ocamldep -native $^ > .depend

.PHONY: clean
clean:
	-@rm -v *.cmx *.cmi *.o .depend main
