#!/usr/bin/env sh

set -euxo pipefail

warnings="-w A-40-42-70-30 -error-style short"

# Build the library
ocamlopt $warnings src/pocket_http.mli -o src/pocket_http.cmi
ocamlopt $warnings src/pocket_http.ml -a -cmi-file src/pocket_http.cmi -o src/pocket_http.cmxa
