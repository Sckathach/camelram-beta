# Camelram Bêta Compiler #

## Intro ##
This compiler aims to compile parts of LaTeX. There is already : 
- basic operations on integers/floats : `+ - * / ^`
- basic operations on floats : `\exp, \log, \cos,\sin, \tan, \acos, \asin, \atan, \cosh, \sinh, \tanh, \ceil, \floor, 
\round`
- approximation of integrals : `\int`
- local variables : `let x = 2^64 in`
- derivation : `formal \diff{x}{x^2 + \sin{x}}`

Examples:

$$ \displaystyle{\log(e^2)^3-2^{\pi}} $$
```sh 
camelrambeta "\log(\exp(2))^3-2^(\pi)" 
> -0.824978
```
$$\displaystyle{\int_0^{\displaystyle{\frac{1}{\pi}\int_{-\infty}^{+\infty}{\frac{sin(x)}{x}}dx}}{\int_{y+1}^0{\frac{1-e^{zy}}{z}}dz}dy}$$
```sh 
 camelrambeta "let a = \int_{-\infty}^{\infty}{\sin(x)/x}/\pi in \int_0^a{\int_{y+1}^0{(1-\exp(z*y))/z}d(z)}d(y)"   
> 1.228285
```

Example of derivation: 
```sh 
camelrambeta "formal \diff{x}{x^2}"
> 2 * x 
```

See the to-do list in the [issues](https://github.com/Camelram-Beta/compiler/issues).

The compiler contains the parser, the lexer, the creation of the AST and some semantic. We use 
[Menhir](http://gallium.inria.fr/~fpottier/menhir/) which is a descendant of 
[ocamlyacc](https://v2.ocaml.org/manual/lexyacc.html). The project will soon use the more recent and faster build 
tool [Dune](https://dune.build/) (instead of [ocamlbuild](https://github.com/ocaml/ocamlbuild)).

## Use ##

When compiled with `make path`, it can be used simply with `camelrambeta` : 
```shell
camelrambeta "3+2"
> 5 
```

When compiled with `make` or `make path` arguments can be piped into `main.native` : 
```shell
echo "3+2" | ./main.native 
> 5
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
```sh 
make
```

##### Build and create an executable on path (needs sudo)
This will create the executable `camelrambeta` in `/usr/local/bin/`:
```sh 
make path
```

#### Clear build files
``` 
make clear
```

### Build the project with ocamlbuild
```sh
ocamlbuild -use-menhir main.native 
```

Test it :
```sh 
cat test.txt | ./main.native
```

### Run unit tests
```sh
make unit
```

Create unit tests in tests.ml:
```ocaml
let tests = [
    test (vars (EBop(BAdd, EVar "x", EInt 3))) ["x"];
    test (ceil (VFloat 3.14)) (VFloat 3.)
]
```

### Test with Utop
You can test your functions directly in utop by using:
```sh 
make test 
```
You can specify which library is needed and which file you are testing with the `LIB` and `TEST` flag. Example: testing 
`playground.ml`:
```ocaml
playground.ml

open Ast 
open Calc

let expr = EBinOp(BopAdd, EBinOp(BopMul, EVar("x"), EInt(3)), EInt(4));;
```
```sh 
make test LIB="ast calc" TEST="playground"
```
⚠️ This doesn't work with the `main.ml` file as the compilation process is different (need for lexer/parser) ⚠️

### Debug 
#### Debug Menhir 
```sh 
menhir path/to/file/parser.mly --explain
```
