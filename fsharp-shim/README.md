# fsharp-shim

F# typed AST extraction shim for organ-bank.

Invokes `dotnet fsi --typedtree --typedtreetypes` on an F# source file,
parses the typed intermediate representation from stderr, and emits
OrganIR JSON on stdout.

## Build

```
cabal build
```

## Usage

```
fsharp-organ path/to/file.fsx
```

Supports both `.fsx` (F# script) and `.fs` (F# source) files.

## Extraction

The shim captures the `pass-end` section of the typed tree dump, which
contains the post-optimisation IR with IL primitives (`#AI_mul#`, etc.)
and full type annotations.

## IL Intrinsic Mapping

| F# IL Intrinsic | OrganIR Primitive |
|------------------|-------------------|
| `#AI_mul#`       | `mul`             |
| `#AI_sub#`       | `sub`             |
| `#AI_add#`       | `add`             |
| `#AI_ceq#`       | `eq`              |
| `#AI_cgt#`       | `gt`              |
| `#AI_clt#`       | `lt`              |
