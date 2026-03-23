# cpp-shim

Extracts LLVM IR from C++ source files via `clang++ -emit-llvm -S -O2` and emits OrganIR JSON. Attempts C++ name demangling via `c++filt`.

## Requirements
- clang++ (LLVM) on PATH
- c++filt on PATH (optional, for demangling)

## Usage
```bash
cabal run cpp-organ -- input.cpp
```
