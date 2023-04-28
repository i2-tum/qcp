all: ocaml build

ocaml:
	opam init -y -a --bare
	opam update
	opam switch -y create . --deps-only ocaml.4.14.1

build:
	dune build

devel:
	opam update
	opam install merlin ocaml-lsp-server odoc ocamlformat utop
