# agda-shim

Agda Treeless IR extraction shim for organ-bank.

## How it works

1. Runs `agda --compile --ghc-dont-call-ghc <file.agda>` which invokes the
   MAlonzo backend but stops before calling GHC. This produces `.hs` files
   under `MAlonzo/Code/` that closely mirror Agda's internal Treeless IR.

2. Parses the generated Haskell to extract function definitions, case
   expressions, let bindings, primitive operations, and constructor
   applications.

3. Emits OrganIR JSON on stdout.

4. Cleans up the generated `MAlonzo/` directory.

## Build

```
cabal build
```

## Usage

```
agda-organ path/to/File.agda
```

## MAlonzo patterns recognised

- `d_functionName_N` -- function definitions
- `case coe v0 of` -- case analysis (coe wrappers stripped)
- `subInt`, `addInt`, `mulInt` -- primitive integer operations
- Constructor patterns in case alternatives
- `erased` -- mapped to unreachable
