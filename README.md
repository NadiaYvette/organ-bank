# Organ Bank

**Stable compiler IR extraction for polyglot compilation.**

Organ Bank harvests intermediate representations from compiler corpses.
Each "organ" is a shim that wraps a real compiler's internals, extracts its
typed IR after frontend analysis, and emits a common JSON interchange format
called **OrganIR**. Frankenstein then stitches these organs together into a
single compiled program.

## The Metaphor

Frankenstein is a polyglot compiler that consumes code written in Haskell,
Rust, Mercury, Idris 2, and Lean 4. Rather than writing five parsers and
five type checkers, Frankenstein reuses the real compilers as donors. Organ
Bank is the harvesting facility: it extracts the typed, analyzed IR from
each compiler's guts and packages it in a uniform format that Frankenstein's
MLIR backend can consume.

The organs:

- **ghc-shim** -- GHC Core with demand/strictness annotations
- **rustc-shim** -- MIR with ownership and borrow information
- **mmc-shim** -- Mercury HLDS with mode/determinism analysis
- **idris2-shim** -- Idris 2 case trees from `--dumpcases`
- **lean4-shim** -- Lean 4 LCNF from a compiler plugin

## Architecture

```
  Source file (.hs, .rs, .m, .idr, .lean)
       |
       v
  [Real Compiler Frontend]   <-- ghc, rustc, mmc, idris2, lean4
       |
       v
  [Organ Bank Shim]          <-- this project
       |
       v
  OrganIR JSON on stdout     <-- spec/organ-ir.schema.json
       |
       v
  [Frankenstein]              <-- /home/nyc/src/frankenstein/
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
- **Exports**: which definitions are visible to other modules

See `spec/organ-ir-example.json` for a concrete example.

## Shim Status

| Shim | Language | Status | Notes |
|------|----------|--------|-------|
| ghc-shim | Haskell | Skeleton | GHC API extraction, based on Frankenstein GhcBridge |
| rustc-shim | Rust | Skeleton | rustc_private callbacks, based on Frankenstein rustc-shim |
| mmc-shim | Haskell | Skeleton | Parses `mmc --dump-hlds 50` output |
| idris2-shim | Haskell | Skeleton | Parses `idris2 --dumpcases` output |
| lean4-shim | Lean 4 | Placeholder | Needs Lean 4 compiler plugin for LCNF access |

## Building

Each shim has its own build system:

```bash
# GHC shim (requires GHC 9.14.*)
cd ghc-shim && cabal build

# Rustc shim (requires nightly + rustc-dev)
cd rustc-shim && cargo build

# MMC shim
cd mmc-shim && cabal build

# Idris 2 shim
cd idris2-shim && cabal build

# Lean 4 shim
cd lean4-shim && lake build
```

## Relationship to Frankenstein

Organ Bank is a companion project to the [Frankenstein polyglot compiler](../frankenstein/).
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
