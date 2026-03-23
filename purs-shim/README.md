# purs-shim

PureScript CoreFn extraction shim for [OrganIR](../spec/organ-ir.schema.json).

## How it works

1. Runs `purs compile --dump-corefn <file.purs>` to produce `output/<Module>/corefn.json`
2. Reads the CoreFn JSON and extracts module name, exports, and declarations
3. Maps declarations to OrganIR definitions and emits JSON on stdout

CoreFn JSON is parsed manually (no aeson dependency) for GHC 9.14 compatibility.

## Build

```
cabal build
```

## Usage

```
purs-organ MyModule.purs > organ-ir.json
```

## OrganIR mapping

- **source_language**: `"purescript"`
- **Types**: All mapped to `any` (CoreFn is type-erased)
- **Effects**: Empty (PureScript is strict; Effect monad detection requires type info)
- **Arity**: Determined by counting nested `Abs` nodes in CoreFn expressions
- **NonRec bindings**: Each becomes a single definition
- **Rec bindings**: Each bind in the group becomes a separate definition

## Limitations

- Expression bodies are placeholders (literal 0); full CoreFn expression translation is future work
- No type information (CoreFn is post-erasure)
- No automatic Effect monad detection
- Requires `purs` (PureScript compiler) on PATH
