compute : main.ml probas.cmo
	ocamlc -o compute construction.cmo compute.cmo compare.cmo probas.cmo main.ml

probas.cmo : probas.ml probas.cmi compare.cmo
	ocamlc -c probas.ml

probas.cmi : compare.cmo probas.mli
	ocamlc probas.mli

compare.cmo : compare.ml compare.cmi compute.cmo
	ocamlc -c compare.ml

compare.cmi : compute.cmo compare.mli
	ocamlc compare.mli

compute.cmo : compute.ml compute.cmi construction.cmo
	ocamlc -c compute.ml

compute.cmi : construction.cmo compute.mli
	ocamlc compute.mli

construction.cmo : construction.ml construction.cmi
	ocamlc -c construction.ml

construction.cmi : construction.mli
	ocamlc construction.mli

computeOpt : main.ml probas.cmx
	ocamlopt -o computeOpt construction.cmx compute.cmx compare.cmx probas.cmx main.ml

probas.cmx : probas.ml probas.cmi compare.cmx
	ocamlopt -c probas.ml

compare.cmx : compare.ml compare.cmi compute.cmx
	ocamlopt -c compare.ml

compute.cmx : compute.ml compute.cmi construction.cmx
	ocamlopt -c compute.ml

construction.cmx : construction.ml construction.cmi
	ocamlopt -c construction.ml

clean :
	rm -f *.cmi *.cmo *.cmx *~ *.o
