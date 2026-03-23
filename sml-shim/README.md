# sml-shim

Extracts type signatures from Standard ML via `mlton -show-basis` and parses source-level function definitions. Note: MLton has no IR dump flag, so this shim provides type-annotated source structure rather than a post-optimization IR.

## Requirements
- MLton on PATH

## Usage
```bash
cabal run sml-organ -- input.sml
```

## Limitations
- No post-optimization IR extraction (MLton doesn't expose one)
- Function bodies are represented as source text, not a structured IR
- Type information comes from MLton's basis dump
