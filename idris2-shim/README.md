# idris2-shim

Extracts Idris 2 post-erasure case trees and emits OrganIR JSON.

## How it works

1. Runs `idris2 --dumpcases` to compile and dump post-erasure case trees
2. Parses the text output (lambda-case tree format)
3. Extracts function definitions with arity and case structure
4. Emits OrganIR JSON on stdout

## Requirements

- Idris 2 compiler on PATH as `idris2`

## Build & Run

```bash
cabal build
cabal run idris2-organ -- Factorial.idr
```

## Notes

Idris 2's `--dumpcases` output is a post-erasure representation where:
- Quantities have been erased (0-use parameters removed)
- Type arguments eliminated
- Case trees fully elaborated

This is ideal for OrganIR extraction since we want post-erasure,
operationally relevant code. Dependent types are intentionally outside
the interop type fragment (Tier 4 exclusion).
