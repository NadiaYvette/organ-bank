# OrganIR Type Mapping Reference

Quick-reference tables showing how each source language's types map to OrganIR.
This extends `spec/interop-types.md` with all 25 supported languages.

## OrganIR Type Summary

| Tier | OrganIR Type | Builder |
|------|-------------|---------|
| 1 | `std.int` | `tCon "Int"` |
| 1 | `std.float` | `tCon "Float"` |
| 1 | `std.bool` | `tCon "Bool"` |
| 1 | `std.byte` | `tCon "Byte"` |
| 1 | `std.string` | `tCon "String"` |
| 1 | `std.unit` | `tCon "Unit"` |
| 2 | `tuple(T1, T2)` | `tApp "Tuple" [t1, t2]` |
| 2 | `option(T)` | `tApp "Option" [t]` |
| 2 | `result(T, E)` | `tApp "Result" [t, e]` |
| 2 | `list(T)` | `tApp "List" [t]` |
| 3 | `fn(args) -> eff ret` | `TFn args eff ret` |
| - | `any` | `TAny` |

---

## Haskell (GHC Core)

| Haskell | OrganIR | Notes |
|---------|---------|-------|
| `Int#` | `std.int` | Unboxed |
| `Double#` | `std.float` | Unboxed |
| `Char#` | `std.byte` | |
| `Bool` | `std.bool` | |
| `()` | `std.unit` | |
| `String` | `std.string` | |
| `Maybe a` | `option(T)` | |
| `Either e a` | `result(T, E)` | |
| `[a]` | `list(T)` | |
| `(a, b)` | `tuple(T1, T2)` | |
| `a -> b` | `fn(many T) -> {div} T` | Laziness implies possible divergence |
| `IO a` | `fn() -> {io} T` | |
| Typeclass dict | erased | Passed as explicit function argument |

## Rust (MIR)

| Rust | OrganIR | Notes |
|------|---------|-------|
| `i64` | `std.int` | |
| `f64` | `std.float` | |
| `bool` | `std.bool` | |
| `u8` | `std.byte` | |
| `&str` | `std.string` | |
| `()` | `std.unit` | |
| `Option<A>` | `option(T)` | |
| `Result<A, E>` | `result(T, E)` | |
| `Vec<A>` | `list(T)` | |
| `(A, B)` | `tuple(T1, T2)` | |
| `fn(A) -> B` | `fn(affine T) -> {} T` | Move semantics |
| `Box<T>` | `affine T` | Heap-allocated, owned |
| `&T` | `many T` | Shared reference |
| `&mut T` | `linear T` | Exclusive reference |

## Mercury (HLDS)

| Mercury | OrganIR | Notes |
|---------|---------|-------|
| `int` | `std.int` | |
| `float` | `std.float` | |
| `string` | `std.string` | |
| `bool` | `std.bool` | |
| `list(T)` | `list(T)` | |
| det pred | `fn(...) -> {} T` | Pure, total |
| semidet pred | `fn(...) -> {exn} T` | May fail |
| multi pred | `fn(...) -> {choice} T` | Multiple solutions |
| nondet pred | `fn(...) -> {exn, choice} T` | May fail, multiple solutions |
| mode `in` | `many T` | Input, unrestricted |
| mode `di` | `linear T` | Destructive input |
| mode `uo` | `linear T` | Unique output |

## Idris 2 (CExp / Case Trees)

| Idris 2 | OrganIR | Notes |
|---------|---------|-------|
| `Int` | `std.int` | |
| `Double` | `std.float` | |
| `Char` | `std.byte` | |
| `String` | `std.string` | |
| `Bool` | `std.bool` | |
| `()` | `std.unit` | |
| `Maybe a` | `option(T)` | |
| `Either e a` | `result(T, E)` | |
| `List a` | `list(T)` | |
| `(a, b)` | `tuple(T1, T2)` | |
| 0 quantity | erased | Not in OrganIR |
| 1 quantity | `linear T` | QTT linearity |
| `w` quantity | `many T` | Unrestricted |
| Post-erasure | `TAny` | Case tree IR has types erased |

## Lean 4 (LCNF)

| Lean 4 | OrganIR | Notes |
|--------|---------|-------|
| `Nat` / `Int` | `std.int` | |
| `Float` | `std.float` | |
| `Bool` | `std.bool` | |
| `UInt8` | `std.byte` | |
| `String` | `std.string` | |
| `Unit` | `std.unit` | |
| `Option A` | `option(T)` | |
| `Except E A` | `result(T, E)` | |
| `List A` | `list(T)` | |
| `(A, B)` | `tuple(T1, T2)` | |
| `A -> B` | `fn(many T) -> {} T` | Pure function |
| `IO A` | `fn() -> {io} T` | |
| `partial` | adds `{div}` | May diverge |
| Post-erasure | `TAny` | LCNF has types mostly erased |

## Koka (Core)

| Koka | OrganIR | Notes |
|------|---------|-------|
| `int` | `std.int` | |
| `float64` | `std.float` | |
| `bool` | `std.bool` | |
| `char` | `std.byte` | |
| `string` | `std.string` | |
| `()` | `std.unit` | |
| `maybe<a>` | `option(T)` | |
| `either<a,b>` | `result(T, E)` | |
| `list<a>` | `list(T)` | |
| `(a, b)` | `tuple(T1, T2)` | |
| `a -> e b` | `fn(many T) -> E T` | Effect-polymorphic |
| `total` | `{}` (empty effect row) | |
| `exn` | `{exn}` | |
| `io` | `{io}` | |
| `div` | `{div}` | |
| `alloc<h>` | `{alloc}` | Heap effect |

## PureScript (CoreFn)

| PureScript | OrganIR | Notes |
|------------|---------|-------|
| `Int` | `std.int` | |
| `Number` | `std.float` | |
| `Boolean` | `std.bool` | |
| `Char` | `std.byte` | |
| `String` | `std.string` | |
| `Unit` | `std.unit` | |
| `Maybe a` | `option(T)` | |
| `Either e a` | `result(T, E)` | |
| `Array a` | `list(T)` | |
| `Tuple a b` | `tuple(T1, T2)` | |
| `a -> b` | `fn(many T) -> {} T` | Pure |
| `Effect a` | `fn() -> {io} T` | |
| Record types | `TAny` | Object literals |
| CoreFn post-erase | `TAny` | CoreFn JSON has limited type info |

## OCaml (Lambda IR)

| OCaml | OrganIR | Notes |
|-------|---------|-------|
| `int` | `std.int` | |
| `float` | `std.float` | |
| `bool` | `std.bool` | |
| `char` | `std.byte` | |
| `string` | `std.string` | |
| `unit` | `std.unit` | |
| `'a option` | `option(T)` | |
| `('a, 'b) result` | `result(T, E)` | |
| `'a list` | `list(T)` | |
| `'a * 'b` | `tuple(T1, T2)` | |
| `'a -> 'b` | `fn(many T) -> {} T` | Pure |
| `ref` / mutable | `fn() -> {io} T` | |
| Lambda IR types | type strings preserved | OCaml type strings as `tCon` |

## Erlang (Core Erlang)

| Erlang | OrganIR | Notes |
|--------|---------|-------|
| integer | `std.int` | Arbitrary precision |
| float | `std.float` | |
| atom `true`/`false` | `std.bool` | |
| binary / string | `std.string` | |
| `[]` | `std.unit` | Empty list as unit |
| `{A, B}` | `tuple(T1, T2)` | |
| `[H\|T]` | `list(T)` | |
| `fun/N` | `fn(...) -> {io} T` | All Erlang functions have side effects |
| Dynamically typed | `TAny` | Erlang is dynamically typed |

## Agda (Treeless)

| Agda | OrganIR | Notes |
|------|---------|-------|
| `Nat` / `Int` | `std.int` | |
| `Float` | `std.float` | |
| `Bool` | `std.bool` | |
| `Char` | `std.byte` | |
| `String` | `std.string` | |
| `List A` | `list(T)` | |
| `Maybe A` | `option(T)` | |
| All Treeless types | `TAny` | Treeless IR is post-erasure |

## F# (Typed Tree)

| F# | OrganIR | Notes |
|----|---------|-------|
| `int` | `std.int` | |
| `float` | `std.float` | |
| `bool` | `std.bool` | |
| `byte` | `std.byte` | |
| `string` | `std.string` | |
| `unit` | `std.unit` | |
| `'a option` | `option(T)` | |
| `Result<'a,'b>` | `result(T, E)` | |
| `'a list` | `list(T)` | |
| `'a * 'b` | `tuple(T1, T2)` | |
| `'a -> 'b` | `fn(many T) -> {} T` | Pure |
| `async<'a>` | `fn() -> {io} T` | |
| Type strings | preserved as `tCon` | F# type strings |

## Scala 3 (Erasure Trees)

| Scala 3 | OrganIR | Notes |
|---------|---------|-------|
| `Int` | `std.int` | |
| `Double` | `std.float` | |
| `Boolean` | `std.bool` | |
| `Byte` | `std.byte` | |
| `String` | `std.string` | |
| `Unit` | `std.unit` | |
| `Option[A]` | `option(T)` | |
| `Either[E, A]` | `result(T, E)` | |
| `List[A]` | `list(T)` | |
| `(A, B)` | `tuple(T1, T2)` | |
| `A => B` | `fn(many T) -> {} T` | |
| `IO[A]` (Cats Effect) | `fn() -> {io} T` | |
| Type strings | preserved as `tCon` | Scala type strings |

## Swift (SIL)

| Swift | OrganIR | Notes |
|-------|---------|-------|
| `Int` | `std.int` | |
| `Double` | `std.float` | |
| `Bool` | `std.bool` | |
| `UInt8` | `std.byte` | |
| `String` | `std.string` | |
| `Void` / `()` | `std.unit` | |
| `Optional<A>` / `A?` | `option(T)` | |
| `Result<A, E>` | `result(T, E)` | |
| `Array<A>` / `[A]` | `list(T)` | |
| `(A, B)` | `tuple(T1, T2)` | |
| `(A) -> B` | `fn(many T) -> {} T` | Value types: default |
| Owned parameters | `affine T` | Consuming / move-only |
| `inout` parameters | `linear T` | Exclusive access |
| SIL types | type strings preserved | SIL type strings as `tCon` |

## Julia (Typed IR)

| Julia | OrganIR | Notes |
|-------|---------|-------|
| `Int64` | `std.int` | |
| `Float64` | `std.float` | |
| `Bool` | `std.bool` | |
| `UInt8` | `std.byte` | |
| `String` | `std.string` | |
| `Nothing` | `std.unit` | |
| `Union{T, Nothing}` | `option(T)` | |
| `Tuple{A, B}` | `tuple(T1, T2)` | |
| `Vector{T}` / `Array{T}` | `list(T)` | |
| All types | delegated | Julia script handles type mapping |

## C (LLVM IR)

| C / LLVM IR | OrganIR | Notes |
|-------------|---------|-------|
| `i1` | `std.bool` | |
| `i8` | `std.byte` | |
| `i32` | `Int32` | Via `tCon` |
| `i64` | `Int64` | Via `tCon` |
| `float` | `Float32` | |
| `double` | `Float64` | |
| `void` | `Void` | |
| `ptr` / `*` | `Ptr` | |
| struct types | `Struct` | |
| vector types | `Vec` | |
| array types | `Array` | |
| Function types | from LLVM IR signature | Parameter types mapped individually |

## C++ (LLVM IR)

Same as C, plus:

| C++ | OrganIR | Notes |
|-----|---------|-------|
| `std::string` | `std.string` | Via demangled name detection |
| `std::optional<T>` | `option(T)` | Via demangled name |
| `std::vector<T>` | `list(T)` | Via demangled name |
| `std::tuple<A,B>` | `tuple(T1, T2)` | Via demangled name |
| Templates | `TAny` | Post-monomorphization in LLVM IR |
| Exceptions | `{exn}` effect | If `invoke`/`landingpad` present |

## Fortran (GIMPLE)

| Fortran | OrganIR | Notes |
|---------|---------|-------|
| `INTEGER` | `std.int` | |
| `REAL` / `DOUBLE PRECISION` | `std.float` | |
| `LOGICAL` | `std.bool` | |
| `CHARACTER` | `std.string` | |
| All GIMPLE types | `TAny` | GIMPLE parsing is best-effort |

## Ada (GIMPLE)

| Ada | OrganIR | Notes |
|-----|---------|-------|
| `Integer` | `std.int` | |
| `Float` | `std.float` | |
| `Boolean` | `std.bool` | |
| `Character` | `std.byte` | |
| `String` | `std.string` | |
| All GIMPLE types | `TAny` | GIMPLE parsing is best-effort |

## Standard ML (MLton Type Basis)

| SML | OrganIR | Notes |
|-----|---------|-------|
| `int` | `std.int` | |
| `real` | `std.float` | |
| `bool` | `std.bool` | |
| `char` | `std.byte` | |
| `string` | `std.string` | |
| `unit` | `std.unit` | |
| `'a option` | `option(T)` | |
| `'a list` | `list(T)` | |
| `'a * 'b` | `tuple(T1, T2)` | |
| `'a -> 'b` | `fn(many T) -> {} T` | SML functions are pure |
| `'a ref` | `fn() -> {io} T` | Mutable reference |
| MLton type strings | preserved as `tCon` | |

## Common Lisp (Disassembly)

| Common Lisp | OrganIR | Notes |
|-------------|---------|-------|
| `fixnum` | `std.int` | |
| `double-float` | `std.float` | |
| `t` / `nil` | `std.bool` | Convention |
| `string` / `simple-string` | `std.string` | |
| `null` | `std.unit` | |
| `cons` / `list` | `list(T)` | |
| `values` | `tuple(T1, T2)` | Multiple return values |
| `function` | `fn(...) -> {io} T` | All Lisp functions may have side effects |
| All types | delegated | Lisp script handles type mapping |

## Scheme (Tree-IL)

| Scheme | OrganIR | Notes |
|--------|---------|-------|
| exact integer | `std.int` | |
| inexact real | `std.float` | |
| `#t` / `#f` | `std.bool` | |
| string | `std.string` | |
| `'()` | `std.unit` | |
| pair / list | `list(T)` | |
| `values` | `tuple(T1, T2)` | Multiple return values |
| `lambda` | `fn(...) -> {io} T` | |
| All types | delegated | Guile script handles type mapping |

## Prolog (WAM Bytecode)

| Prolog | OrganIR | Notes |
|--------|---------|-------|
| integer | `std.int` | |
| float | `std.float` | |
| atom | `std.string` | Atoms as strings |
| list `[H\|T]` | `list(T)` | |
| compound `f(A,B)` | `tuple(T1, T2)` | |
| predicate `p/N` | `fn(...) -> {exn, choice} T` | Nondeterministic, may fail |
| All types | `TAny` | Prolog is dynamically typed |

## Lua (Frontend)

| Lua | OrganIR | Notes |
|-----|---------|-------|
| number (integer) | `std.int` | Lua 5.4 integer subtype |
| number (float) | `std.float` | |
| boolean | `std.bool` | |
| string | `std.string` | |
| nil | `std.unit` | |
| table (array) | `list(T)` | When used as sequence |
| table (record) | `TAny` | Generic table |
| function | `fn(...) -> {io} T` | All Lua functions may have side effects |
| All types | `TAny` | Lua is dynamically typed |

## Forth (Frontend)

| Forth | OrganIR | Notes |
|-------|---------|-------|
| single-cell integer | `std.int` | |
| double-cell integer | `std.int` | |
| float | `std.float` | |
| flag (0/-1) | `std.bool` | |
| counted string | `std.string` | |
| All types | `TAny` | Forth is untyped (stack-based) |

## Zig (ZIR)

| Zig | OrganIR | Notes |
|-----|---------|-------|
| `i64` | `std.int` | |
| `f64` | `std.float` | |
| `bool` | `std.bool` | |
| `u8` | `std.byte` | |
| `[]const u8` | `std.string` | Zig string slice |
| `void` | `std.unit` | |
| `?T` | `option(T)` | Optional |
| `!T` / error unions | `result(T, E)` | |
| `[]T` | `list(T)` | Slice |
| `.{A, B}` | `tuple(T1, T2)` | |
| `fn(T) T` | `fn(many T) -> {} T` | |
| All ZIR types | delegated | Zig tool handles pre-semantic IR |

---

## Effect Mapping Summary

| Effect | OrganIR | Languages that produce it |
|--------|---------|---------------------------|
| (empty) `{}` | pure / total | Haskell pure fns, Mercury `det`, Rust `const fn`, Koka `total`, Lean 4 total |
| `{exn}` | may fail | Mercury `semidet`, Rust `Result`, Idris 2 partial, Koka `exn` |
| `{io}` | side effects | Haskell `IO`, Mercury `io`, Erlang (all), Lua (all), Lisp (all), Scheme (all) |
| `{choice}` | nondeterminism | Mercury `multi`, Prolog predicates |
| `{div}` | may diverge | Haskell (default for lazy fns), Lean 4 `partial`, Koka `div` |
| `{alloc}` | heap allocation | Koka heap effects |
| `{exn, choice}` | fail + nondeterminism | Mercury `nondet`, Prolog predicates |

## Multiplicity Summary

| Multiplicity | OrganIR | Languages that produce it |
|-------------|---------|---------------------------|
| `many` (default) | unrestricted | Haskell, OCaml, Koka, Lean 4, Scala, F#, most languages |
| `affine` | used at most once | Rust move, Swift consuming params |
| `linear` | used exactly once | Mercury `di`/`uo`, Idris 2 `1`, Rust `&mut T` |
