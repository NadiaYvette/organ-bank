# zig-shim

Zig ZIR extraction shim for OrganIR. Extracts Zig Intermediate Representation
(ZIR) from a `.zig` source file using `std.zig.AstGen` and emits OrganIR JSON.

## Architecture

Two components:

1. **zig-tool/zir-extract** — A Zig program that uses the standard library's
   `std.zig.Ast.parse()` and `std.zig.AstGen.generate()` to produce ZIR from
   source. Emits OrganIR JSON directly on stdout.

2. **zig-organ** — A Haskell wrapper that locates and invokes `zir-extract`,
   passing the JSON through.

## Limitations

**ZIR is untyped.** It is the output of AstGen (AST → IR lowering), *before*
semantic analysis (Sema). This means:

- No type information is available
- No type checking has been performed
- Generic instantiations are not resolved
- Comptime evaluation has not occurred

ZIR is roughly analogous to a desugared AST, not a typed IR.

## Building

```sh
# Build the Zig extraction tool (requires zig 0.15.x)
cd zig-tool && zig build

# Build the Haskell wrapper (requires GHC 9.14.x)
cabal build
```

## Usage

```sh
# Direct use of the Zig tool
./zig-tool/zig-out/bin/zir-extract path/to/file.zig

# Via the Haskell wrapper
cabal run zig-organ -- path/to/file.zig
```

## Requirements

- Zig 0.15.x (uses standard library AstGen)
- GHC 9.14.x, cabal 3.16.x (for Haskell wrapper)
