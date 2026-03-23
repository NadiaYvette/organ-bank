# scheme-shim

Extracts Guile Tree-IL (high-level IR) from Scheme source using Guile's `compile` API. Tree-IL preserves lexical structure, closures, and primitive operations.

## Requirements
- Guile 3.0 on PATH (as `guile3.0` or `guile`)

## Usage
```bash
cabal run scheme-organ -- input.scm
```
