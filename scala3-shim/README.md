# scala3-shim

Scala 3 post-erasure tree extraction shim for organ-bank.

Runs `scala compile -Vprint:erasure` on a `.scala` source file, parses the
post-erasure typed tree text output, and emits OrganIR JSON on stdout.

## Build

    cabal build

## Usage

    cabal run scala3-organ -- path/to/File.scala

## Requirements

- Scala CLI (`scala`) via Coursier at `~/.local/share/coursier/bin/scala`
- Scala 3.x (tested with 3.8.2)
