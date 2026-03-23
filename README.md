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
Swift, Agda, F#, Scala 3, Julia, and Zig. Rather than writing fifteen parsers
and fifteen type checkers, Frankenstein reuses the real compilers as donors.
Organ Bank is the harvesting facility: it extracts the typed, analyzed IR
from each compiler's guts and packages it in a uniform format that
Frankenstein's MLIR backend can consume.

The organs:

- **ghc-shim** -- GHC Core with demand/strictness annotations
- **rustc-shim** -- MIR with ownership and borrow information
- **mmc-shim** -- Mercury HLDS with mode/determinism analysis
- **idris2-shim** -- Idris 2 case trees from `--dumpcases`
- **lean4-shim** -- Lean 4 LCNF from a real compiler plugin
- **erlc-shim** -- Core Erlang (covers Erlang + Elixir + Gleam)
- **purs-shim** -- PureScript CoreFn from `--dump-corefn` (JSON)
- **ocaml-shim** -- OCaml Lambda IR from `-dlambda`
- **koka-shim** -- Koka Core from `--showcore`
- **swift-shim** -- Swift SIL from `-emit-sil`
- **agda-shim** -- Agda Treeless via MAlonzo backend
- **fsharp-shim** -- F# Typed Tree from `--typedtree`
- **scala3-shim** -- Scala 3 post-erasure trees from `-Vprint:erasure`
- **julia-shim** -- Julia Typed IR via `code_typed()` reflection
- **zig-shim** -- Zig ZIR via `std.zig.AstGen`

## Architecture

```
  Source file (.hs, .rs, .m, .idr, .lean, .erl, .purs, .ml, .kk, .swift,
              .agda, .fsx, .scala, .jl, .zig)
       |
       v
  [Real Compiler Frontend]   <-- ghc, rustc, mmc, idris2, lean4, erlc,
       |                         purs, ocamlopt, koka, swiftc, agda,
       |                         dotnet-fsi, scalac, julia, zig
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

See `spec/organ-ir-example.json` for a concrete example and
`spec/organ-ir.schema.json` for the full JSON Schema.

## Shim Status

| Shim | Language | Compiler IR | Access Method | Status |
|------|----------|-------------|---------------|--------|
| ghc-shim | Haskell | GHC Core | GHC API (`runGhc`) | Working |
| rustc-shim | Rust | MIR | `rustc_private` callbacks | Working |
| mmc-shim | Mercury | HLDS | `mmc --dump-hlds 50` | Working |
| idris2-shim | Idris 2 | Case trees | `idris2 --dumpcases` | Working |
| lean4-shim | Lean 4 | LCNF | Compiler plugin (`baseExt`) | Working |
| erlc-shim | Erlang | Core Erlang | `erlc +to_core` | Working |
| purs-shim | PureScript | CoreFn | `purs compile --dump-corefn` | Working |
| ocaml-shim | OCaml | Lambda | `ocamlopt -dlambda` | Working |
| koka-shim | Koka | Core | `koka --showcore` | Working |
| swift-shim | Swift | SIL | `swiftc -emit-sil` | Working |
| agda-shim | Agda | Treeless | `agda --compile --ghc-dont-call-ghc` | Working |
| fsharp-shim | F# | Typed Tree | `dotnet fsi --typedtree` | Working |
| scala3-shim | Scala 3 | Erasure Trees | `scalac -Vprint:erasure` | Working |
| julia-shim | Julia | Typed IR | `code_typed()` reflection | Working |
| zig-shim | Zig | ZIR | `std.zig.AstGen` | Working |

## Building

Each shim has its own build system:

```bash
# GHC shim (requires GHC 9.14+)
cd ghc-shim && cabal build

# Rustc shim (requires nightly + rustc-dev)
cd rustc-shim && cargo build

# MMC shim (requires mmc on PATH)
cd mmc-shim && cabal build

# Idris 2 shim (requires idris2 on PATH)
cd idris2-shim && cabal build

# Lean 4 shim (requires Lean 4.16+)
cd lean4-shim && lake build

# Core Erlang shim (requires erlc on PATH)
cd erlc-shim && cabal build

# PureScript shim (requires purs on PATH)
cd purs-shim && cabal build

# OCaml shim (requires ocamlopt on PATH)
cd ocaml-shim && cabal build

# Koka shim (requires koka on PATH)
cd koka-shim && cabal build

# Swift shim (requires swiftc on PATH)
cd swift-shim && cabal build

# Agda shim (requires agda on PATH)
cd agda-shim && cabal build

# F# shim (requires dotnet SDK)
cd fsharp-shim && cabal build

# Scala 3 shim (requires scala via Coursier)
cd scala3-shim && cabal build

# Julia shim (requires julia on PATH)
cd julia-shim && cabal build

# Zig shim (requires zig on PATH)
cd zig-shim && cabal build
# Also build the Zig extraction tool:
cd zig-shim/zig-tool && zig build
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
