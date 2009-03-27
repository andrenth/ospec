all:
	ocamlc -dtypes -a -o pa_spec.cma -pp camlp4orf -I +camlp4 pa_spec.ml
	ocamlc -o ospec dynlink.cma -I +camlp4 toplevellib.cma camlp4o.cma str.cma \
		     pa_spec.cma ospec.ml

clean:
	rm -f *.annot *.cma *.cmi *.cmo ospec
