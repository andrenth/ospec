PREFIX = $(shell ocamlfind ocamlc -where | sed -e 's|/lib.*||')

all:
	ocamlfind ocamlc -dtypes -c spec_types.ml
	ocamlfind ocamlc -dtypes -syntax camlp4o \
		-package camlp4.extend,camlp4.quotations spec_types.cmo -c pa_spec.ml
	ocamlfind ocamlc -dtypes -c spec_types.ml
	ocamlfind ocamlc -dtypes -c spec.mli
	ocamlfind ocamlc -dtypes -c spec.ml
	ocamlfind ocamlc -dtypes -c report.mli
	ocamlfind ocamlc -dtypes -c report.ml
	ocamlfind ocamlc -dtypes -c helpers.ml
	ocamlfind ocamlc -dtypes -o ospec dynlink.cma -I +camlp4 -package findlib \
		toplevellib.cma camlp4o.cma str.cma spec.cmo report.cmo helpers.cmo \
		pa_spec.cmo ospec.ml -linkpkg

install:
	ocamlfind install ospec META pa_spec.cmo pa_spec.cmi spec.cmo spec.cmi \
		helpers.cmo helpers.cmi report.cmo report.cmi spec_types.cmo spec_types.cmi
	install -m 755 ospec $(PREFIX)/bin

uninstall:
	ocamlfind remove ospec
	rm -f $(PREFIX)/bin/ospec

clean:
	rm -f *.annot *.cmo *.cmi *.cmo ospec
