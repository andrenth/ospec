install:
	ocamlfind install ospec src/META _build/src/pa_spec.cmo \
		_build/src/pa_spec.cmi _build/src/spec.cmo _build/src/spec.cmi \
		_build/src/helpers.cmo _build/src/helpers.cmi _build/src/report.cmo \
		_build/src/report.cmi _build/src/gen.cmo _build/src/gen.cmi \
		_build/src/prop.cmo _build/src/prop.cmi
	install -m 755 _build/src/ospec $(EXEC_PREFIX)/bin

uninstall:
	ocamlfind remove ospec
	rm $(EXEC_PREFIX)/bin/ospec
