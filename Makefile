LIB = ast
TEST = calc

all:
	ocamlbuild -use-menhir main.native
	cat test.txt | ./main.native

path:
	ocamlbuild -use-menhir main.native
	sudo cp main.native /usr/local/bin/camelrambeta

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


