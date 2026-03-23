# prolog-shim

Extracts WAM (Warren Abstract Machine) bytecode from Prolog source via `gplc -W` (GNU Prolog). The WAM is the standard Prolog virtual machine instruction set.

## Requirements
- GNU Prolog (`gplc`) on PATH

## Usage
```bash
cabal run prolog-organ -- input.pl
```
