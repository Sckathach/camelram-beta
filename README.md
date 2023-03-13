# Camelram Bêta

## Test
### Install opam 
#### Ubuntu
```sh
add-apt-repository ppa:avsm/ppa
apt update
apt install opam

eval $(opam env)

opam install dune merlin ocaml-lsp-server odoc ocamlformat utop dune-release
```

### Build the project with ocamlbuild
```sh
ocamlbuild -use-menhir main.native 
```

### Test it 
```sh 
cat test.txt | ./main.native
```
