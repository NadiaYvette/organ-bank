# koka-shim

Koka Core IR extraction shim for the organ-bank project.

## What it does

Runs `koka --showcore <file.kk>` to obtain Koka's Core IR text, parses function/value definitions, and emits OrganIR JSON on stdout.

## Build

```
cabal build
```

## Usage

```
cabal run koka-organ -- path/to/file.kk
```

Or after install:

```
koka-organ path/to/file.kk
```

## Output format

```json
{
  "organ": "koka",
  "module": "hello",
  "imports": ["std/core"],
  "definitions": [
    {
      "name": "factorial",
      "params": [
        {"name": "n", "type": "std.int"}
      ],
      "effects": ["div"],
      "returnType": "std.int"
    }
  ]
}
```

## Effect mapping

| Koka effect | OrganIR effect |
|-------------|---------------|
| `total`     | `pure`        |
| `div`       | `div`         |
| `console`   | `io`          |
| `io`        | `io`          |
| `exn`       | `exn`         |
| `raise`     | `exn`         |

## Type mapping

| Koka type  | OrganIR type |
|------------|-------------|
| `int`      | `std.int`   |
| `float64`  | `std.float` |
| `bool`     | `std.bool`  |
| `string`   | `std.string`|
| `()`       | `std.unit`  |
| other      | `any`       |
