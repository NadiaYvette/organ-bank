# cl-shim

Extracts function metadata and disassembly from Common Lisp via SBCL. Uses `sb-introspect` for lambda lists and types, `disassemble` for machine code.

## Requirements
- SBCL on PATH

## Usage
```bash
cabal run cl-organ -- input.lisp
```
