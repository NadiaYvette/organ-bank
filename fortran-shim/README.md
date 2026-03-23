# fortran-shim

Extracts GCC GIMPLE (optimized SSA) from Fortran source via `gfortran -fdump-tree-optimized -O2` and emits OrganIR JSON.

## Requirements
- gfortran on PATH

## Usage
```bash
cabal run fortran-organ -- input.f90
```
