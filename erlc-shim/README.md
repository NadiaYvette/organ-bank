# erlc-shim

Core Erlang extraction shim for OrganIR. Runs `erlc +to_core` on an Erlang
source file, parses the Core Erlang text output, and emits OrganIR JSON on
stdout.

## Prerequisites

- Erlang/OTP installed (`erlc` on PATH)
- GHC >= 9.14, cabal >= 3.10

## Build

```
cabal build
```

## Usage

```
cabal run erlc-organ -- path/to/module.erl
```

This will:

1. Run `erlc +to_core module.erl` to produce `module.core`
2. Parse the Core Erlang to extract function definitions (name, arity)
3. Scan function bodies for IO indicators to determine effects
4. Emit OrganIR JSON on stdout

## Effect mapping

- Functions containing IO-related calls (`io:`, `file:`, send/receive,
  process dictionary ops, spawn, etc.) get the `io` effect
- All other functions are marked as pure (empty effect row)
- All types are `any` since Erlang is dynamically typed

## OrganIR schema

Output follows OrganIR schema version 1.0.0 with `source_language: "erlang"`.
