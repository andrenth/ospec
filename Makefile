all:
	ocamlc -dtypes -a -o spec.cma -pp camlp4orf -I +camlp4 spec.ml
	ocamlmktop -o ospec str.cma -I +camlp4 camlp4o.cma spec.cmo

clean:
	rm -f *.annot *.cma *.cmi *.cmo ospec
