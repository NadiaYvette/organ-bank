# mmc-shim

Extracts Mercury HLDS from source files and emits OrganIR JSON.

## How it works

1. Runs `mmc --dump-hlds 50` to get the HLDS after mode/determinism analysis
2. Parses predicate declarations from the dump text
3. Maps Mercury determinism to OrganIR effects (det->pure, semidet->exn, multi->choice, nondet->exn+choice)
4. Emits OrganIR JSON on stdout

## Requirements

- Mercury compiler (rotd-2024-06-15 or later) on PATH as `mmc`

## Build & Run

```bash
cabal build
cabal run mmc-organ -- search.m
```
