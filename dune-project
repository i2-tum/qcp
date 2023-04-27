(lang dune 3.0)

(using menhir 2.1)

(name qcprop)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name qcprop)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml (= 4.14.0)) (dune (= 3.7.0)) (core (= v0.15.1)) (menhir (= 20220210)) (yojson (= 2.0.2)) (zarith (= 1.12)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
