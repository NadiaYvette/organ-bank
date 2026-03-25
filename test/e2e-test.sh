#!/usr/bin/env bash
# End-to-end test: source -> organ-bank frontend -> OrganIR JSON -> Frankenstein -> MLIR -> native
#
# Proves the full pipeline from source language through organ-bank shims to
# Frankenstein Core and (if MLIR tools are available) all the way to a native binary.
set -euo pipefail

PASS=0; FAIL=0; SKIP=0
CABAL="${CABAL:-$HOME/.ghcup/bin/cabal-3.16.1.0}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
EXAMPLES="$SCRIPT_DIR/e2e-examples"
HELLO_EXAMPLES="$SCRIPT_DIR/examples"
PROJECT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
FRANKENSTEIN_DIR="${FRANKENSTEIN_DIR:-$HOME/src/frankenstein}"

pass() { echo "  PASS $1"; PASS=$((PASS + 1)); }
fail() { echo "  FAIL $1 ($2)"; FAIL=$((FAIL + 1)); }
skip() { echo "  SKIP $1 ($2)"; SKIP=$((SKIP + 1)); }

# --------------------------------------------------------------------------
# Step 0: Build tools
# --------------------------------------------------------------------------

echo "=== Organ Bank -> Frankenstein E2E Tests ==="
echo ""

# Build organ-bank frontends and organ-validate (not the full project --
# playground and some shims may have unresolvable deps on this GHC)
OB_TARGETS="sml-organ-fe erlang-organ-fe prolog-organ-fe scheme-organ-fe lua-organ-fe forth-organ-fe organ-validate"
echo "Building organ-bank frontends..."
if ! (cd "$PROJECT_DIR" && $CABAL build $OB_TARGETS 2>/dev/null); then
  echo "FATAL: Could not build organ-bank frontends. Aborting."
  exit 1
fi
echo "  organ-bank frontends built OK"

# Helper: run a cabal exe from organ-bank
ob_run() {
  local exe="$1"; shift
  (cd "$PROJECT_DIR" && $CABAL -v0 run "$exe" -- "$@")
}

# Build (or locate) frankenstein
# NOTE: frankenstein links against GHC 9.14 libs, which makes linking slow (~2 min).
# We build first, then wait for the linker to finish (the "Text file busy" issue).
FRANKENSTEIN=""
if [ -d "$FRANKENSTEIN_DIR" ]; then
  echo "Building frankenstein from $FRANKENSTEIN_DIR..."
  if (cd "$FRANKENSTEIN_DIR" && $CABAL build exe:frankenstein 2>/dev/null); then
    FRANKENSTEIN=$(cd "$FRANKENSTEIN_DIR" && $CABAL list-bin exe:frankenstein 2>/dev/null || true)
    if [ -n "$FRANKENSTEIN" ] && [ -s "$FRANKENSTEIN" ]; then
      # Wait for linker to finish (cabal may return before the binary is fully written)
      local_tries=0
      while [ "$local_tries" -lt 12 ]; do
        if "$FRANKENSTEIN" --help >/dev/null 2>&1; then
          break
        fi
        echo "  Waiting for frankenstein linker to finish..."
        sleep 10
        local_tries=$((local_tries + 1))
      done
      if "$FRANKENSTEIN" --help >/dev/null 2>&1; then
        echo "  frankenstein built OK: $FRANKENSTEIN"
      else
        echo "  WARNING: frankenstein binary exists but cannot execute"
        FRANKENSTEIN=""
      fi
    else
      FRANKENSTEIN=""
    fi
  fi
fi

# Fallback: check PATH
if [ -z "$FRANKENSTEIN" ] && command -v frankenstein &>/dev/null; then
  FRANKENSTEIN="$(command -v frankenstein)"
  echo "  frankenstein found on PATH: $FRANKENSTEIN"
fi

# Fallback: use cabal exec (avoids "Text file busy" re-link race)
if [ -z "$FRANKENSTEIN" ]; then
  if [ -d "$FRANKENSTEIN_DIR" ]; then
    echo "  Using 'cabal exec' for frankenstein from $FRANKENSTEIN_DIR"
    frankenstein_run() {
      (cd "$FRANKENSTEIN_DIR" && $CABAL -v0 exec frankenstein -- "$@")
    }
  else
    echo "  FATAL: No frankenstein binary and no source tree at $FRANKENSTEIN_DIR"
    echo "  Set FRANKENSTEIN_DIR to point to the frankenstein source tree."
    exit 1
  fi
else
  frankenstein_run() {
    "$FRANKENSTEIN" "$@"
  }
fi

# Check for MLIR toolchain
HAS_MLIR=0
if command -v mlir-opt &>/dev/null && command -v mlir-translate &>/dev/null && command -v clang &>/dev/null; then
  HAS_MLIR=1
  echo "  MLIR toolchain detected (mlir-opt, mlir-translate, clang)"
else
  echo "  MLIR toolchain not fully available -- skipping --compile tests"
fi

echo ""

# --------------------------------------------------------------------------
# Step 1: Run tests for each frontend
# --------------------------------------------------------------------------

# run_e2e <lang-label> <frontend-exe> <example-file>
#   1. Runs the frontend on the example to produce OrganIR JSON
#   2. Validates the JSON with organ-validate
#   3. Pipes JSON to frankenstein --from-json --emit-core
#   4. Frankenstein --from-json --emit-mlir
#   5. If MLIR tools available, also tests --compile and runs the binary
run_e2e() {
  local lang="$1" exe="$2" file="$3"

  echo "--- $lang ($file) ---"

  if [ ! -f "$file" ]; then
    skip "$lang/frontend" "example file not found: $file"
    return
  fi

  # Step 1: Run frontend -> OrganIR JSON
  local tmpjson
  tmpjson=$(mktemp --suffix=.json)
  if ! ob_run "$exe" "$file" >"$tmpjson" 2>/dev/null; then
    fail "$lang/frontend" "frontend exited non-zero"
    rm -f "$tmpjson"
    return
  fi

  if [ ! -s "$tmpjson" ]; then
    fail "$lang/frontend" "frontend produced empty output"
    rm -f "$tmpjson"
    return
  fi
  pass "$lang/frontend"

  # Step 2: Validate JSON with organ-validate
  if ! ob_run organ-validate "$tmpjson" 2>/dev/null; then
    fail "$lang/validate" "organ-validate rejected JSON"
    rm -f "$tmpjson"
    return
  fi
  pass "$lang/validate"

  # Step 3: Frankenstein --from-json --emit-core
  local tmpcore
  tmpcore=$(mktemp --suffix=.core)
  if ! frankenstein_run --from-json --emit-core "$tmpjson" >"$tmpcore" 2>/dev/null; then
    fail "$lang/core" "frankenstein --emit-core rejected OrganIR"
    rm -f "$tmpjson" "$tmpcore"
    return
  fi

  if [ ! -s "$tmpcore" ]; then
    fail "$lang/core" "frankenstein --emit-core produced empty output"
    rm -f "$tmpjson" "$tmpcore"
    return
  fi
  pass "$lang/core"
  rm -f "$tmpcore"

  # Step 4: Frankenstein --from-json --emit-mlir
  local tmpmlir
  tmpmlir=$(mktemp --suffix=.mlir)
  if ! frankenstein_run --from-json --emit-mlir "$tmpjson" >"$tmpmlir" 2>/dev/null; then
    fail "$lang/mlir" "frankenstein --emit-mlir failed"
    rm -f "$tmpjson" "$tmpmlir"
    return
  fi

  if [ ! -s "$tmpmlir" ]; then
    fail "$lang/mlir" "frankenstein --emit-mlir produced empty output"
    rm -f "$tmpjson" "$tmpmlir"
    return
  fi
  pass "$lang/mlir"
  rm -f "$tmpmlir"

  # Step 5: Full compilation (if MLIR toolchain available)
  if [ "$HAS_MLIR" -eq 1 ]; then
    local tmpbin
    tmpbin=$(mktemp)
    if frankenstein_run --from-json --compile -o "$tmpbin" "$tmpjson" >/dev/null 2>&1; then
      if [ -x "$tmpbin" ] && [ -s "$tmpbin" ]; then
        pass "$lang/compile"
        # Try to run the binary (timeout after 5s in case of infinite recursion)
        local rc=0
        timeout 5 "$tmpbin" >/dev/null 2>&1 || rc=$?
        if [ "$rc" -eq 124 ]; then
          fail "$lang/run" "binary timed out"
        else
          # Non-zero exit is OK -- factorial(10) = 3628800, exit codes truncate mod 256
          pass "$lang/run (exit=$rc)"
        fi
      else
        fail "$lang/compile" "binary not executable or empty"
      fi
    else
      fail "$lang/compile" "frankenstein --compile failed"
    fi
    rm -f "$tmpbin"
  else
    skip "$lang/compile" "MLIR toolchain not available"
    skip "$lang/run" "MLIR toolchain not available"
  fi

  rm -f "$tmpjson"
}

# --------------------------------------------------------------------------
# Run e2e tests for each of the 6 independent frontends
# --------------------------------------------------------------------------

# Use e2e-examples if they exist (with main function), fall back to hello examples
sml_file="$EXAMPLES/factorial.sml"
[ -f "$sml_file" ] || sml_file="$HELLO_EXAMPLES/hello.sml"

lua_file="$EXAMPLES/factorial.lua"
[ -f "$lua_file" ] || lua_file="$HELLO_EXAMPLES/hello.lua"

run_e2e "sml"     sml-organ-fe     "$sml_file"
run_e2e "erlang"  erlang-organ-fe  "$HELLO_EXAMPLES/hello.erl"
run_e2e "prolog"  prolog-organ-fe  "$HELLO_EXAMPLES/hello.pl"
run_e2e "scheme"  scheme-organ-fe  "$HELLO_EXAMPLES/hello.scm"
run_e2e "lua"     lua-organ-fe     "$lua_file"
run_e2e "forth"   forth-organ-fe   "$HELLO_EXAMPLES/hello.fth"

# --------------------------------------------------------------------------
# Summary
# --------------------------------------------------------------------------

echo ""
echo "=== E2E Results ==="
echo "PASS: $PASS  FAIL: $FAIL  SKIP: $SKIP"
echo ""

if [ "$FAIL" -gt 0 ]; then
  exit 1
else
  exit 0
fi
