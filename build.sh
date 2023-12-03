#!/usr/bin/env sh

set -euxo pipefail

warnings="-w A-40-42-70-30 -error-style short"

# Build the library
ocamlopt $warnings src/pocket_http.mli -o src/pocket_http.cmi
ocamlopt $warnings src/pocket_http.ml -a -cmi-file src/pocket_http.cmi -o src/pocket_http.cmxa

# Build the tests
ocamlopt $warnings -I src src/pocket_http.cmx test/parse.ml -o test/parse
ocamlopt $warnings -I +unix -I src src/pocket_http.cmx test/server.ml -o test/server

# Run the tests
test/run_tests.sh
