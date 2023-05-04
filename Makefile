LIB = ast
TEST = calc

all:
	ocamlbuild -use-menhir main.native
	cat test.txt | ./main.native

path:
	ocamlbuild -use-menhir main.native
	sudo cp main.native /usr/local/bin/camelrambeta_pipe
	sudo cp exec_camelrambeta /usr/local/bin/camelrambeta

.SILENT:
clear:
	ocamlbuild -clean

.SILENT:
test: clear all
	echo "#directory \"_build\";;" > init_utop.ml
	for l in $(LIB) ; do \
  		ocamlc -c $$l.ml -o _build/$$l.cmo -I _build/ ; \
		echo "#load \"$$l.cmo\";;" >> init_utop.ml ; \
	done
	for t in $(TEST) ; do \
  		ocamlc -c $$t.ml -o _build/$$t.cmo -I _build/ ; \
		echo "#use \"$$t.ml\";;" >> init_utop.ml ; \
	done
	utop -init init_utop.ml
	rm -f init_utop.ml

unit:
	ocamlbuild -use-menhir main.native > /dev/null
	ocamlbuild -clean > /dev/null
	ocamlbuild tests.native > /dev/null
ifneq ("$(wildcard $(tests.native))", "")
	@echo "ERREUR : ocamlbuild n'a pas réussi à compiler tests, essayez 'make test LIB=\"ast variable function general calc\" TEST=tests'"
else
	./tests.native
	rm tests.native
endif



