(lang dune 3.10)

(name TinyCaml)

(generate_opam_files true)

(source
 (github abdufelsayed/TinyCaml))

(authors "Abdullah Fathy Elsayed")

(maintainers "Abdullah Fathy Elsayed")

(license LGPL-3.0-or-later)

(using menhir 2.1)

(package
 (name TinyCaml)
 (synopsis "Tiny version of OCaml")
 (description "TinyCaml is just a subset of OCaml")
 (depends ocaml dune fmt core)
 (tags
  (pl
   ocaml
   "type-theory"
   "lambda-calculus"
   "hindley-milner"
   "type-inference"
   "functional-programming"
   "static-typing")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
