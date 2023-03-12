(lang dune 3.4)
(using menhir)

(name camelries)

(generate_opam_files true)

(source
 (github Camelram-Beta/camelram-beta))

(authors "Sckathach")


(package
 (name camelries)
 (synopsis "Developemment project for PRO3600 course")
 (description "Implementation of a formal calculator in OCaml/Java")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
