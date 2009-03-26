all:
	ocamlc -dtypes -a -o spec.cma -pp camlp4orf -I +camlp4 spec.ml
	ocamlc -o ospec -I +camlp4 toplevellib.cma camlp4o.cma str.cma spec.cma \
		        ospec.ml

clean:
	rm -f *.annot *.cma *.cmi *.cmo ospec
