all:
	ocamlbuild -use-menhir main.native
	cat test.txt | ./main.native