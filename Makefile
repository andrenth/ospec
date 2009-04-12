PREFIX = $(shell ocamlfind ocamlc -where | sed -e 's|/lib.*||')
VERSION = $(shell ocamlc -version | cut -d . -f 1-2)

ifeq ($(VERSION), 3.10)
	MACRO = -pp "camlp4o Camlp4MacroParser.cmo -D OCAML_BUG"
else
	MACRO = -pp "camlp4o Camlp4MacroParser.cmo"
endif

all:
	ocamlfind ocamlc -dtypes -syntax camlp4o \
		-package camlp4.extend,camlp4.quotations -c pa_spec.ml
	ocamlfind ocamlc -dtypes -c spec.mli
	ocamlfind ocamlc -dtypes -c spec.ml
	ocamlfind ocamlc -dtypes -c report.mli
	ocamlfind ocamlc -dtypes -c report.ml
	ocamlfind ocamlc -dtypes -c helpers.ml
	ocamlfind ocamlc -dtypes -c gen.ml
	ocamlfind ocamlc -dtypes -c prop.mli
	ocamlfind ocamlc -dtypes -c prop.ml
	ocamlfind ocamlc -dtypes -o ospec dynlink.cma -I +camlp4 -package findlib \
		toplevellib.cma camlp4o.cma str.cma spec.cmo report.cmo helpers.cmo \
		gen.cmo prop.cmo pa_spec.cmo ospec.ml -linkpkg $(MACRO)

install:
	ocamlfind install ospec META pa_spec.cmo pa_spec.cmi spec.cmo spec.cmi \
		helpers.cmo helpers.cmi report.cmo report.cmi gen.cmo gen.cmi prop.cmo \
		prop.cmi
	install -m 755 ospec $(PREFIX)/bin

uninstall:
	ocamlfind remove ospec
	rm -f $(PREFIX)/bin/ospec

clean:
	rm -f *.annot *.cmo *.cmi *.cmo ospec
