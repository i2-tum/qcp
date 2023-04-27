#!/usr/bin/env bash

# This shell script will generate the parser (in-place, i.e. not in the directory _build/) and report any 
# conflicts inherent in the specification of the parser. Detailed descriptions of the conflicts (if any), can be found
# in the generated `parser.conflicts` file.

# build and compile tokens
menhir --only-tokens ../tokens/tokens.mly
ocamlc -c ../tokens/tokens.mli
ocamlc -c ../../abs/abs.ml
# build and compile parser
menhir parser.mly \
    ../tokens/tokens.mly \
    --explain \
    --external-tokens Tokens \
    --base parser \
    --infer \
    --ocamlc "ocamlc -I ../tokens/ -I ../../abs/"

# cleanup, remove all generated files except the `parser.conflicts` file
rm -f parser.mli
rm -f parser.ml
rm -f ../tokens/tokens.mli
rm -f ../tokens/tokens.ml
rm -f ../tokens/tokens.cmi
rm -f ../../abs/abs.cmo
rm -f ../../abs/abs.cmi