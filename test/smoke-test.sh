#!/usr/bin/env bash
# Smoke test: run each available shim/frontend on a small example, validate output
set -euo pipefail

PASS=0; FAIL=0; SKIP=0
CABAL="${CABAL:-$HOME/.ghcup/bin/cabal-3.16.1.0}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXAMPLES="$SCRIPT_DIR/examples"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

run_test() {
  local name="$1" exe="$2" file="$3" compiler="${4:-}"
  if [ -n "$compiler" ] && ! command -v "$compiler" &>/dev/null; then
    echo "SKIP $name ($compiler not found)"
    ((SKIP++))
    return
  fi
  local tmpfile
  tmpfile=$(mktemp)
  if $CABAL -v0 run "$exe" -- "$file" >"$tmpfile" 2>/dev/null; then
    if $CABAL -v0 run organ-validate -- "$tmpfile" 2>/dev/null; then
      echo "PASS $name"
      ((PASS++))
    else
      echo "FAIL $name (validation failed)"
      # Show validation errors for debugging
      $CABAL -v0 run organ-validate -- "$tmpfile" 2>&1 || true
      ((FAIL++))
    fi
  else
    echo "FAIL $name (shim exited non-zero)"
    ((FAIL++))
  fi
  rm -f "$tmpfile"
}

cd "$PROJECT_DIR"

echo "=== Organ Bank Smoke Tests ==="
echo ""

# --- Frontends (pure parsers, no external compiler needed) ---

run_test "sml-frontend/hello.sml"      sml-organ-fe     "$EXAMPLES/hello.sml"
run_test "erlang-frontend/hello.erl"    erlang-organ-fe  "$EXAMPLES/hello.erl"
run_test "prolog-frontend/hello.pl"     prolog-organ-fe  "$EXAMPLES/hello.pl"
run_test "scheme-frontend/hello.scm"    scheme-organ-fe  "$EXAMPLES/hello.scm"
run_test "lua-frontend/hello.lua"       lua-organ-fe     "$EXAMPLES/hello.lua"
run_test "forth-frontend/hello.fth"     forth-organ-fe   "$EXAMPLES/hello.fth"

# --- Shims (need external compilers) ---

run_test "c-shim/hello.c"              c-organ          "$EXAMPLES/hello.c"        clang
run_test "cpp-shim/hello.cpp"           cpp-organ        "$EXAMPLES/hello.cpp"      clang++
run_test "fortran-shim/hello.f90"       fortran-organ    "$EXAMPLES/hello.f90"      gfortran
run_test "ada-shim/hello.adb"           ada-organ        "$EXAMPLES/hello.adb"      gnat
run_test "ocaml-shim/hello.ml"          ocaml-organ      "$EXAMPLES/hello.ml"       ocamlopt
run_test "sml-shim/hello.sml"           sml-organ        "$EXAMPLES/hello.sml"      mlton
run_test "koka-shim/hello.kk"           koka-organ       "$EXAMPLES/hello.kk"       koka
run_test "erlc-shim/hello.erl"          erlc-organ       "$EXAMPLES/hello.erl"      erlc
run_test "mmc-shim/hello.pl"            mmc-organ        "$EXAMPLES/hello.pl"       mmc
run_test "prolog-shim/hello.pl"         prolog-organ     "$EXAMPLES/hello.pl"       gplc
run_test "scheme-shim/hello.scm"        scheme-organ     "$EXAMPLES/hello.scm"      guile
run_test "idris2-shim/hello.idr"        idris2-organ     "$EXAMPLES/hello.idr"      idris2
run_test "purs-shim/hello.purs"         purs-organ       "$EXAMPLES/hello.purs"     purs
run_test "agda-shim/hello.agda"         agda-organ       "$EXAMPLES/hello.agda"     agda
run_test "fsharp-shim/hello.fsx"        fsharp-organ     "$EXAMPLES/hello.fsx"      dotnet
run_test "scala3-shim/hello.scala"      scala3-organ     "$EXAMPLES/hello.scala"    scala
run_test "swift-shim/hello.swift"       swift-organ      "$EXAMPLES/hello.swift"    swiftc

echo ""
echo "=== Results ==="
echo "PASS: $PASS  FAIL: $FAIL  SKIP: $SKIP"
echo ""

if [ "$FAIL" -gt 0 ]; then
  exit 1
else
  exit 0
fi
