# ada-shim

Extracts GCC GIMPLE from Ada source via `gnat compile -fdump-tree-gimple` and emits OrganIR JSON. Filters out Ada elaboration boilerplate.

## Requirements
- GNAT (gnat compile) on PATH

## Usage
```bash
cabal run ada-organ -- input.adb
```
