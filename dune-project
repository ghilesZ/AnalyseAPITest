(lang dune 2.1)

(name collect)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name collect)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ocaml-migrate-parsetree)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
