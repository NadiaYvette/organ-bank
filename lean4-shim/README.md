# lean4-shim

Extracts Lean 4 definitions and emits OrganIR JSON.

## How it works

1. Parses Lean 4 source files for top-level `def` and `theorem` declarations
2. (Future) Uses the Lean 4 compiler API to access LCNF (Lambda-Compiled Normal Form)
3. Emits OrganIR JSON on stdout

## Requirements

- Lean 4 (v4.16.0+) with Lake build system

## Build & Run

```bash
lake build
lake exe lean4-organ Factorial.lean
```

## Notes

The current implementation is a text-level parser for `def`/`theorem` declarations.
The full implementation would use Lean 4's compiler internals to access LCNF,
which is the post-erasure IR where:
- Proofs and types are erased
- `do` notation is desugared to monadic binds
- Inductive types are compiled to constructors with runtime-relevant fields only

LCNF is ideal for OrganIR extraction because dependent types have already been
erased, leaving only the operationally relevant code.

## Architecture for Full Implementation

```
Lean source → Lean compiler frontend → LCNF
                                          ↓
                              lean4-organ plugin
                                          ↓
                                   OrganIR JSON
```

A compiler plugin would register a pass after LCNF generation that walks
`Lean.Compiler.LCNF.Decl` values and emits OrganIR.
