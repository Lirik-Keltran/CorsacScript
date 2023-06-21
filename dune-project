(lang dune 3.5)

(name paw)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name paw)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ppx_deriving)
 (tags
  (topics "to describe" your project)))

(using menhir 2.0)
