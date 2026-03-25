#!/usr/bin/env bash
# Integration test: run each frontend, validate output, pretty-print, round-trip,
# and optionally feed to Frankenstein.
set -euo pipefail

PASS=0; FAIL=0; SKIP=0
CABAL="${CABAL:-$HOME/.ghcup/bin/cabal-3.16.1.0}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXAMPLES="$SCRIPT_DIR/examples"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

HAS_FRANKENSTEIN=0
if command -v frankenstein &>/dev/null; then
  HAS_FRANKENSTEIN=1
  echo "Frankenstein detected on PATH -- will test pipeline"
else
  echo "Frankenstein not on PATH -- skipping Frankenstein pipeline tests"
fi

pass() { echo "PASS $1"; PASS=$((PASS + 1)); }
fail() { echo "FAIL $1 ($2)"; FAIL=$((FAIL + 1)); }
skip() { echo "SKIP $1 ($2)"; SKIP=$((SKIP + 1)); }

# run_integration <test-name> <frontend-exe> <example-file>
run_integration() {
  local name="$1" exe="$2" file="$3"

  # Step 1: Run frontend, capture JSON output
  local tmpjson
  tmpjson=$(mktemp --suffix=.json)
  if ! $CABAL -v0 run "$exe" -- "$file" >"$tmpjson" 2>/dev/null; then
    fail "$name/frontend" "frontend exited non-zero"
    rm -f "$tmpjson"
    return
  fi
  pass "$name/frontend"

  # Step 2: Validate JSON
  if ! $CABAL -v0 run organ-validate -- "$tmpjson" 2>/dev/null; then
    fail "$name/validate" "validation failed"
    $CABAL -v0 run organ-validate -- "$tmpjson" 2>&1 || true
    rm -f "$tmpjson"
    return
  fi
  pass "$name/validate"

  # Step 3: Pretty-print and check non-empty output
  local tmppretty
  tmppretty=$(mktemp --suffix=.txt)
  if ! $CABAL -v0 run organ-validate -- --pretty "$tmpjson" >"$tmppretty" 2>/dev/null; then
    fail "$name/pretty" "pretty-print exited non-zero"
    rm -f "$tmpjson" "$tmppretty"
    return
  fi
  if [ ! -s "$tmppretty" ]; then
    fail "$name/pretty" "pretty-print output is empty"
    rm -f "$tmpjson" "$tmppretty"
    return
  fi
  pass "$name/pretty"
  rm -f "$tmppretty"

  # Step 4: Round-trip: JSON -> organ-validate (stdin) -> check exit 0
  if ! $CABAL -v0 run organ-validate -- <"$tmpjson" 2>/dev/null; then
    fail "$name/roundtrip" "stdin round-trip validation failed"
    rm -f "$tmpjson"
    return
  fi
  pass "$name/roundtrip"

  # Step 5: Frankenstein pipeline (optional)
  if [ "$HAS_FRANKENSTEIN" -eq 1 ]; then
    if frankenstein --from-organ-ir "$tmpjson" --emit-core >/dev/null 2>&1; then
      pass "$name/frankenstein"
    else
      fail "$name/frankenstein" "frankenstein rejected OrganIR"
    fi
  else
    skip "$name/frankenstein" "frankenstein not available"
  fi

  rm -f "$tmpjson"
}

cd "$PROJECT_DIR"

echo ""
echo "=== Organ Bank Integration Tests ==="
echo ""

# --- All 6 pure frontends ---
run_integration "sml"     sml-organ-fe     "$EXAMPLES/hello.sml"
run_integration "erlang"  erlang-organ-fe  "$EXAMPLES/hello.erl"
run_integration "prolog"  prolog-organ-fe  "$EXAMPLES/hello.pl"
run_integration "scheme"  scheme-organ-fe  "$EXAMPLES/hello.scm"
run_integration "lua"     lua-organ-fe     "$EXAMPLES/hello.lua"
run_integration "forth"   forth-organ-fe   "$EXAMPLES/hello.fth"

echo ""
echo "=== Results ==="
echo "PASS: $PASS  FAIL: $FAIL  SKIP: $SKIP"
echo ""

if [ "$FAIL" -gt 0 ]; then
  exit 1
else
  exit 0
fi
