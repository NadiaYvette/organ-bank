# rustc-shim

Extracts MIR from Rust source files and emits OrganIR JSON.

## How it works

1. Uses `rustc_private` to hook into the Rust compiler after analysis
2. Iterates `hir_body_owners()` and calls `optimized_mir()` for each
3. Serializes MIR function signatures and basic block structure as OrganIR JSON

## Requirements

- Rust nightly with `rustc-dev` and `llvm-tools` components
- `rust-toolchain.toml` handles this automatically

## Build & Run

```bash
cargo build
cargo run -- factorial.rs
```
