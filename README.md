# Camelram Bêta Compiler #

## Intro ##
This compiler aims to compile parts of LaTeX. There are already : 
- basic operations on integers : `+ - *`
- approximation of integrals (soon) : `\int`

See the to-do list in the [issues](https://github.com/Camelram-Beta/compiler/issues).

The compiler contains the parser, the lexer, the creation of the AST and some semantic. We use 
[Menhir](http://gallium.inria.fr/~fpottier/menhir/) which is a descendant of 
[ocamlyacc](https://v2.ocaml.org/manual/lexyacc.html). The project will soon use the more recent and faster build 
tool [Dune](https://dune.build/) (instead of [ocamlbuild](https://github.com/ocaml/ocamlbuild)).

## Use ##

When compiled with make, it can be used simply with `exec` : 
```shell
./exec "3+2"
```
For now, the program only takes an argument and evaluate it.

Or by piping to `main.native` : 
```shell
echo "3+2" | ./main.native 
```


## Test ##
### Install opam 
#### Ubuntu
```sh
add-apt-repository ppa:avsm/ppa
apt update
apt install opam

eval $(opam env)

opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
```

#### Archlinux 
```sh
pacman -S opam

eval $(opam env)

opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
```
### Build with make
⚠️ It seems there is an issue with the permissions, make sure to build the project on a linux partition ⚠️
```sh 
make
```

### Build the project with ocamlbuild
⚠️ It seems there is an issue with the permissions, make sure to build the project on a linux partition ⚠️
```sh
ocamlbuild -use-menhir main.native 
```

Test it :
```sh 
cat test.txt | ./main.native
```

### Debug 
#### Debug Menhir 
```sh 
menhir path/to/file/parser.mly --explain
```
