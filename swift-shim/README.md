# swift-shim

Swift SIL extraction shim for OrganIR.

Runs `swiftc -emit-sil` on a Swift source file, parses the canonical SIL
text output, and emits OrganIR JSON on stdout.

## Build

```
cabal build
```

## Usage

```
swift-organ <file.swift>
```

## What it extracts

- **Functions**: mangled name, demangled name, access level, calling convention
- **Types**: parameter types and return type from SIL type signatures (mapped to `any` for now)
- **Effects**: `throws` -> exn effect, `async` -> async effect
- **Multiplicity**: functions containing ARC operations (strong_retain, strong_release, copy_value, destroy_value) are marked `affine`
- **Basic blocks**: count of BBs per function (stored in `_sil_blocks`)

## SIL format

SIL (Swift Intermediate Language) is a high-level SSA IR between AST and LLVM IR.
Key constructs:

```
sil [serialized] [ossa] @$s4main9factorialyS2iF : $@convention(thin) (Int) -> Int {
bb0(%0 : $Int):
  %1 = integer_literal $Builtin.Int64, 1
  ...
  return %result : $Int
} // end sil function '$s4main9factorialyS2iF'

sil_witness_table ...
sil_vtable ...
```

## Limitations

- Type translation is shallow: all types become `con` references with the SIL type name
- Expression bodies are stubbed (`elit int 0`) -- full SIL instruction translation is future work
- Name demangling is best-effort (decodes `$s<len><mod><len><name>` patterns); for full demangling, pipe through `swift demangle`
- Requires `swiftc` on PATH
