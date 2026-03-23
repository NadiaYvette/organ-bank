# julia-shim

Julia typed IR extraction shim for OrganIR.

## How it works

1. A Julia script (`scripts/extract.jl`) `include()`s the target file, discovers
   user-defined functions via `names(Main)`, calls `code_typed()` for each method,
   and emits OrganIR JSON on stdout.

2. A thin Haskell wrapper (`julia-organ`) locates the script and invokes
   `julia --startup-file=no extract.jl <file>`, passing the JSON through.

## Usage

```
julia-organ myfile.jl > output.json
```

Or directly:

```
julia scripts/extract.jl myfile.jl
```

## Limitations

- `code_typed()` requires concrete types. Functions without type annotations will
  have parameter types inferred as `Any`, which gives less detailed IR. Add type
  annotations for best results.
- Only functions defined at module top level in Main are discovered.
- Generic functions with multiple methods produce one definition per concrete method.
- Julia's IR is SSA-based, so the body is represented as `ssa_body` with a list of
  indexed statements rather than expression trees.

## Building

```
cabal build
```

Requires GHC 9.14+ and Julia 1.11+ on PATH.
