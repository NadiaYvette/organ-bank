# lean4-shim

Extracts Lean 4 LCNF (Lambda-Compiled Normal Form) and emits OrganIR JSON.

## How it works

1. Uses `Lean.Elab.Frontend` to drive the full Lean 4 compilation pipeline
   (parsing, elaboration, type checking, LCNF compilation)
2. Accesses `Lean.Compiler.LCNF.baseExt` to read post-erasure LCNF declarations
3. Walks the LCNF AST (`Code`, `LetValue`, `AltCore`, etc.) and converts to OrganIR
4. Emits OrganIR JSON on stdout

## LCNF → OrganIR Mapping

| LCNF construct | OrganIR construct |
|----------------|-------------------|
| `Code.let decl k` | `elet` (nested let-binding chain) |
| `Code.fun decl k` | `elet` binding a `elam` (local function) |
| `Code.jp decl k` | `elet` binding a `elam` (join point as continuation) |
| `Code.jmp fv args` | `eapp` (jump = tail call to join point) |
| `Code.cases c` | `ecase` with `pat_con`/`pat_wild` branches |
| `Code.return fv` | `evar` (return = reference to value) |
| `Code.unreach` | placeholder (unreachable/absurd) |
| `LetValue.value (natVal n)` | `elit` int |
| `LetValue.value (strVal s)` | `elit` string |
| `LetValue.const name args` | `eapp`/`econ` (constructor or function call) |
| `LetValue.proj ty idx s` | `eapp` of synthetic projection function |
| `LetValue.fvar fv args` | `eapp` (local function call) |
| `Param.borrow = true` | multiplicity "affine" |
| `Param.borrow = false` | multiplicity "many" |
| `DeclValue.extern` | sort "external" |

## Requirements

- Lean 4 (v4.16.0+) with Lake build system
- The Lean compiler libraries (included with standard Lean installation)

## Build & Run

```bash
lake build
lake exe lean4-organ Factorial.lean
```

## Architecture

```
                    Lean 4 compiler pipeline
                    ========================
Lean source → Parsing → Elaboration → LCNF compilation
                                            ↓
                              baseExt (PersistentEnvExtension)
                                            ↓
                              lean4-organ (this tool)
                                  ↓              ↓
                          LcnfExtract.lean   OrganIR.lean
                          (LCNF → OrganIR)   (JSON types)
                                  ↓
                           OrganIR JSON on stdout
```

This is a genuine compiler plugin, not a text parser. It accesses the same
post-erasure IR that Lean 4 uses for its own code generation, meaning:
- Proofs and types have been erased (quantity-aware erasure)
- `do` notation is desugared to monadic binds
- Inductive types are compiled to constructors with runtime-relevant fields only
- Optimizations (CSE, simp, specialization) have been applied

## Notes on LCNF

LCNF is Lean 4's ANF (A-Normal Form) IR. Every intermediate value is
let-bound with a unique `FVarId`. The `Code` type is a chain:

```
let x₁ := v₁;
let x₂ := v₂;
...
cases xₙ
  | ctor₁ p₁ p₂ => let y₁ := w₁; return y₁
  | ctor₂ p₃    => jmp joinPoint p₃
```

This maps naturally to OrganIR's expression trees via nested `elet`/`ecase`.
