# ghc-shim

Extracts GHC Core from Haskell source files and emits OrganIR JSON.

## How it works

1. Uses the GHC API (`ghc` library package) to compile through desugaring
2. Runs at optimization level 1 to get demand/strictness analysis
3. Walks the `CoreProgram` (`mg_binds` from `ModGuts`)
4. Emits OrganIR JSON on stdout

## Requirements

- GHC 9.14.1
- cabal-install 3.14+

## Build & Run

```bash
cabal build
cabal run ghc-organ -- Factorial.hs
```
