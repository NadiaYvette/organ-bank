# Changelog

## 0.1.0 — 2026-03-25

Initial release.

- **OrganIR.Types**: Core data types covering 25 source languages, 20+ expression
  variants, algebraic effect rows, multiplicity annotations, data types, and
  effect declarations.
- **OrganIR.Build**: Smart constructors (`simpleOrganIR`, `organIRWithExports`,
  `funDef`, `valDef`, `localName`, `qualName`) for ergonomic IR construction.
- **OrganIR.Json**: `renderOrganIR` — efficient hand-rolled JSON emission with no
  aeson dependency.
- **OrganIR.Parse**: `parseOrganIR` — hand-rolled JSON parser for round-tripping.
- **OrganIR.Validate**: `validateOrganIR` — structural checks, placeholder detection,
  arity/type consistency, severity levels (Info/Warn/Error).
- **OrganIR.Pretty**: `ppOrganIR` — human-readable IR output with indentation,
  effect rows, data types, and effect declarations.
- **organ-validate** CLI tool for validating and pretty-printing OrganIR JSON.
- 114 round-trip tests covering all OrganIR variants.
