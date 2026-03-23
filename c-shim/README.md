# c-shim

Extracts LLVM IR from C source files via `clang -emit-llvm -S -O2` and emits OrganIR JSON.

## Requirements
- clang (LLVM) on PATH

## Usage
```bash
cabal run c-organ -- input.c
```
