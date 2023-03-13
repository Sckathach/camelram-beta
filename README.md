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

#### Test it 
```sh 
cat test.txt | ./main.native
```
