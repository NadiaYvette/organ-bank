# OrganIR Example Gallery

One OrganIR JSON file per supported source language, each encoding a factorial function
(or equivalent idiomatic computation). These serve as reference for shim authors showing
what realistic OrganIR output looks like for each language.

## Files

| File | Language | Notable OrganIR features |
|------|----------|------------------------|
| `haskell.json` | Haskell | ECase, EApp, ELam, TFn with GHC unboxed `Int#` types |
| `rust.json` | Rust | Affine multiplicity, ERetain for refcount |
| `mercury.json` | Mercury | Effect rows (exn from semidet), EPerform for raise |
| `koka.json` | Koka | Effect rows (div, exn), EPerform |
| `idris2.json` | Idris 2 | TAny (post-erasure), untyped lambda params |
| `lean4.json` | Lean 4 | ELet chains (LCNF style), typed bindings |
| `ocaml.json` | OCaml | ELet/ECase (Lambda IR style) |
| `purescript.json` | PureScript | ECase/EApp (CoreFn style) |
| `erlang.json` | Erlang | Pure effect, explicit arity field |
| `swift.json` | Swift | SIL-style ELet basic blocks |
| `agda.json` | Agda | Constructor patterns (zero/suc), EUnreachable branch |
| `fsharp.json` | F# | ECase pattern matching |
| `scala3.json` | Scala 3 | TAny (post-erasure) |
| `julia.json` | Julia | Typed with Core.Int64 |
| `zig.json` | Zig | Typed with std.u64 |
| `c.json` | C | TCon int32 types |
| `cpp.json` | C++ | Demangled names (`factorial::factorial(long)`) |
| `fortran.json` | Fortran | GIMPLE-style ELet temporaries |
| `ada.json` | Ada | Typed with Standard.Integer |
| `sml.json` | Standard ML | Hindley-Milner types, typed pat_var |
| `common-lisp.json` | Common Lisp | TAny (dynamically typed) |
| `scheme.json` | Scheme | TAny (dynamically typed) |
| `prolog.json` | Prolog | Accumulator style, arity 2 |
| `lua.json` | Lua | TAny (dynamically typed) |
| `forth.json` | Forth | Stack-based decomposed to ELet bindings |

## Validation

All files should pass `organ-validate`:

```sh
for f in spec/examples/*.json; do organ-validate "$f" && echo "OK: $f"; done
```
