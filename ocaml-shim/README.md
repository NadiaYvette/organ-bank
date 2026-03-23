# ocaml-shim

Extracts OCaml Lambda IR from `.ml` source files and emits OrganIR JSON.

## How it works

1. Invokes `ocamlopt -dlambda -c <file.ml>` to dump the Lambda intermediate representation
2. Parses the S-expression-like Lambda IR output (tokenize, then recursive descent)
3. Extracts top-level `let`/`letrec` bindings, `(function ...)` forms, and the `(makeblock 0 ...)` export list
4. Emits OrganIR JSON on stdout

OCaml Lambda IR is post-type-erasure, so all types map to `any` in OrganIR.
OCaml is strict and impure (refs, exceptions, I/O) but most user-defined
functions are effectively pure; the shim emits empty effect rows by default.

## Requirements

- OCaml compiler (`ocamlopt`) on `$PATH`
- GHC 9.14.1
- cabal-install 3.14+

## Build & Run

```bash
cabal build
cabal run ocaml-organ -- factorial.ml
```

## Example

Given `factorial.ml`:

```ocaml
let rec fact n = if n <= 1 then 1 else n * fact (n - 1)
let () = print_int (fact 10); print_newline ()
```

Running `ocaml-organ factorial.ml` produces OrganIR JSON with the `fact` definition,
its arity (1 parameter), and the module export list.
