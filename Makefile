build:
	dune build

utop:
	dune utop bin

.PHONY: test
test:
	dune exec test/main.exe

clean:
	dune clean