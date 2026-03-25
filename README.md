# Organ Bank

**Stable compiler IR extraction for polyglot compilation.**

Organ Bank harvests intermediate representations from compiler corpses.
Each "organ" is a shim that wraps a real compiler's internals, extracts its
typed IR after frontend analysis, and emits a common JSON interchange format
called **OrganIR**. Frankenstein then stitches these organs together into a
single compiled program.

## The Metaphor

Frankenstein is a polyglot compiler that consumes code written in Haskell,
Rust, Mercury, Idris 2, Lean 4, OCaml, Erlang/Elixir, PureScript, Koka,
Swift, Agda, F#, Scala 3, Julia, Zig, C, C++, Fortran, Ada, Standard ML,
Common Lisp, Scheme, and Prolog. Rather than writing twenty-three parsers
and twenty-three type checkers, Frankenstein reuses the real compilers as donors.
Organ Bank is the harvesting facility: it extracts the typed, analyzed IR
from each compiler's guts and packages it in a uniform format that
Frankenstein's MLIR backend can consume.

## Architecture

```
  Source file (.hs, .rs, .m, .idr, .lean, .erl, .purs, .ml, .kk, .swift,
              .agda, .fsx, .scala, .jl, .zig, .c, .cpp, .f90, .adb,
              .sml, .lisp, .scm, .pl)
       |
       v
  [Real Compiler Frontend]   <-- ghc, rustc, mmc, idris2, lean4, erlc,
       |                         purs, ocamlopt, koka, swiftc, agda,
       |                         dotnet-fsi, scalac, julia, zig, clang,
       |                         gfortran, gnat, mlton, sbcl, guile, gplc
       v
  [Organ Bank Shim]          <-- this project
       |
       v
  OrganIR JSON on stdout     <-- spec/organ-ir.schema.json
       |
       v
  [Frankenstein]              <-- github.com/NadiaYvette/frankenstein
       |
       v
  MLIR -> LLVM -> native binary
```

Each shim is a standalone project in its own subdirectory. Shims are
invoked as command-line tools: they take a source file path and emit
OrganIR JSON on stdout.

## OrganIR Format

OrganIR is a JSON format that captures:

- **Module metadata**: source language, compiler version, file path
- **Definitions**: functions with typed parameters, bodies as expression trees
- **Types**: using a tiered interop type system (see `spec/interop-types.md`)
- **Effects**: algebraic effect rows from Koka/Mercury, IO effects, exception effects
- **Exports**: which definitions are visible to other modules
- **Multiplicity**: affine/linear annotations from ownership systems (Rust, Swift, Mercury)

See `spec/organ-ir-example.json` for a concrete example and
`spec/organ-ir.schema.json` for the full JSON Schema.

## organ-ir Library

The `organ-ir` Haskell library provides shared types, builders, and tools
used by all shims. It has no dependencies beyond `base` and `text`.

| Module | Purpose |
|--------|---------|
| `OrganIR.Types` | Core data types: `OrganIR`, `Module`, `Definition`, `Expr`, `Ty` (25 source languages, 20+ expression variants) |
| `OrganIR.Build` | Smart constructors: `simpleOrganIR`, `organIRWithExports`, `funDef`, `valDef`, `localName`, `qualName` |
| `OrganIR.Json` | `renderOrganIR :: OrganIR -> Text` -- efficient hand-rolled JSON emission (no aeson dependency) |
| `OrganIR.Parse` | `parseOrganIR :: Text -> Either String OrganIR` -- hand-rolled JSON parser |
| `OrganIR.Validate` | `validateOrganIR :: OrganIR -> [Warning]` -- structural checks, placeholder detection, arity/type consistency |
| `OrganIR.Pretty` | `ppOrganIR :: OrganIR -> Text` -- human-readable IR output with indentation, effect rows, data types, effect declarations |

### organ-validate CLI

```bash
# Validate OrganIR JSON
organ-validate input.json

# Validate and pretty-print
organ-validate --pretty input.json

# Pipe from a shim
ghc-organ Hello.hs | organ-validate --pretty
```

## Independent Frontends

In addition to compiler shims, Organ Bank includes 6 independent frontends
written from scratch in Haskell:

| Frontend | Language | Features |
|----------|----------|----------|
| sml-frontend | Standard ML | Lexer, parser, Hindley-Milner type inference, pattern match compilation |
| erlang-frontend | Erlang | Lexer, operator precedence parser, export tracking |
| scheme-frontend | R7RS Scheme | Lexer, reader, macro desugaring |
| prolog-frontend | ISO Prolog | Lexer, term reader, clause grouping |
| lua-frontend | Lua 5.4 | Lexer, recursive-descent parser |
| forth-frontend | ANS Forth | Word lexer, structured definition parser |

## Shim Status

| Shim | Language | Compiler IR | Expression Translation | Type Translation |
|------|----------|-------------|----------------------|-----------------|
| ghc-shim | Haskell | GHC Core | Full (Var, Lit, App, Lam, Let, Case, TypeApp, TypeLam) | Full (ForAllTy, FunTy, TyConApp, dictionary erasure) |
| rustc-shim | Rust | MIR | Full (basic blocks, statements, terminators via serde_json) | Function signatures via fn_sig |
| mmc-shim | Mercury | HLDS | HLDS clauses as structured text | Modes with Affine multiplicity |
| idris2-shim | Idris 2 | Case trees | Case tree text as structured expressions | TAny |
| lean4-shim | Lean 4 | LCNF | Full (let, fun, join points, cases) | TAny (post-erasure) |
| erlc-shim | Erlang | Core Erlang | Function clauses with patterns | Pure effect annotation |
| purs-shim | PureScript | CoreFn | Full (Literal, Var, App, Abs, Let, Case, Constructor) | From CoreFn JSON |
| ocaml-shim | OCaml | Lambda | Lambda expressions with full translation | OCaml type strings |
| koka-shim | Koka | Core | Core expressions with effect rows | Koka type strings with namespaced effects |
| swift-shim | Swift | SIL | SIL basic blocks as structured IR | SIL type strings |
| agda-shim | Agda | Treeless | Treeless expressions | TAny |
| fsharp-shim | F# | Typed Tree | Typed tree expressions | F# type strings |
| scala3-shim | Scala 3 | Erasure Trees | Full expression parsing with fallback | Scala type strings |
| julia-shim | Julia | Typed IR | Delegated to Julia script | Delegated |
| zig-shim | Zig | ZIR | Delegated to Zig tool | Untyped (pre-semantic) |
| c-shim | C | LLVM IR | LLVM IR blocks as structured text | TAny |
| cpp-shim | C++ | LLVM IR | LLVM IR blocks with C++ demangling | TAny |
| fortran-shim | Fortran | GIMPLE | GIMPLE blocks as structured text | TAny |
| ada-shim | Ada | GIMPLE | GIMPLE blocks as structured text | TAny |
| sml-shim | Standard ML | Type basis | Source parsing with MLton types | MLton type basis |
| cl-shim | Common Lisp | Disassembly | Delegated to Lisp script (validated) | Delegated |
| scheme-shim | Scheme | Tree-IL | Delegated to Guile script (validated) | Delegated |
| prolog-shim | Prolog | WAM | WAM bytecode as structured text | TAny |

## Building

### All Haskell packages at once

```bash
# Main project (22 packages, requires GHC 9.14+)
cabal build all

# GHC shim (separate project file, requires GHC 9.14.1)
cabal --project-file=cabal-ghc914.project build ghc-shim

# Rust shim
cd rustc-shim && cargo build

# Lean 4 shim
cd lean4-shim && lake build

# Zig extraction tool
cd zig-shim/zig-tool && zig build
```

### Running tests

```bash
# Run 114 round-trip tests covering all OrganIR variants
cabal test all

# Run smoke tests (requires compilers on PATH)
./test/smoke-test.sh

# Validate a shim's output
ghc-organ Hello.hs | organ-validate --pretty
```

## Relationship to Frankenstein

Organ Bank is a companion project to the [Frankenstein polyglot compiler](https://github.com/NadiaYvette/frankenstein).
Frankenstein currently has extraction code embedded in its own source tree
(`GhcBridge/`, `RustBridge/`, `MercuryBridge/`). Organ Bank factors this
out into standalone, reusable shims with a stable JSON interface, so that:

1. Shims can be versioned and tested independently of Frankenstein.
2. Other projects can consume OrganIR without depending on Frankenstein.
3. Adding a new source language means writing one new shim, not modifying
   the compiler core.

## License

GPL-2.0-only. This is forced by Mercury (one of the donor compilers).
All other donor compilers have compatible licenses.
