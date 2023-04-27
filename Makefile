all: ocaml python build

ocaml:
	opam init -y -a --bare
	opam update
	opam switch -y create . --deps-only ocaml.4.14.0

build:
	dune build

devel:
	opam update
	opam install merlin ocaml-lsp-server odoc ocamlformat utop

python:
	# Required for RPO
	pyenv install -s 3.7.16
	pyenv virtualenv -f 3.7.16 env3.7
	eval "$$(pyenv init -)"; \
	eval "$$(pyenv virtualenv-init -)"; \
	pyenv activate env3.7; \
	pip install --upgrade pip; \
	pip install -r scripts/lib/rpo/requirements.txt; \
	pyenv deactivate

	pyenv install -s 3.10.10
	pyenv virtualenv -f 3.10.10 env3.10
	eval "$$(pyenv init -)"; \
	eval "$$(pyenv virtualenv-init -)"; \
	pyenv activate env3.10; \
	pip install --upgrade pip; \
	pip install -r requirements.txt

R:
	Rscript packages.R
