LIBS_NAME=program symbolicHeap entailment biabduction precond
LIBS=$(patsubst %,%.cmx,$(LIBS_NAME))

main: $(LIBS) main.cmx
	ocamlopt -o $@ $^

%.cmx: %.ml
	ocamlopt -c $<

%.cmi: %.mli
	ocamlopt -c $<

-include .depend
.depend: $(wildcard *.ml) $(wildcard *.mli)
	ocamldep -native $^ > .depend

.PHONY: clean
clean:
	-@rm -v *.cmx *.cmi *.o .depend main
