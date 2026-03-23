# OrganIR Interop Type System

The interop type fragment defines which types can cross function call boundaries
between languages compiled by Frankenstein. It is deliberately smaller than any
individual language's type system.

## Tier 1: Ground Types

Every source language has direct equivalents.

| OrganIR name | MLIR/LLVM | Haskell     | Rust    | Mercury | Idris 2  | Lean 4  |
|-------------|-----------|-------------|---------|---------|----------|---------|
| `std.int`   | `i64`     | `Int`       | `i64`   | `int`   | `Int`    | `Int`   |
| `std.float` | `f64`     | `Double`    | `f64`   | `float` | `Double` | `Float` |
| `std.bool`  | `i1`      | `Bool`      | `bool`  | `bool`  | `Bool`   | `Bool`  |
| `std.byte`  | `i8`      | `Word8`     | `u8`    | `uint8` | `Bits8`  | `UInt8` |
| `std.string`| `ptr+len` | `String`    | `&str`  | `string`| `String` | `String`|
| `std.unit`  | `void`    | `()`        | `()`    | `{}`    | `()`     | `Unit`  |

### Memory layout

- `int`, `float`, `bool`, `byte`: unboxed, passed in registers
- `string`: fat pointer (pointer + length), immutable, refcounted
- `unit`: zero-sized, optimized away

## Tier 2: Structural Compound Types

Composite types that every language can produce and consume.

| OrganIR      | Haskell        | Rust             | Mercury       | Idris 2       | Lean 4       |
|-------------|----------------|------------------|---------------|---------------|--------------|
| `tuple(T..)`| `(a, b, ...)`  | `(A, B, ...)`    | `{A, B, ...}` | `(a, b, ...)`| `(A, B, ...)`|
| `option(T)` | `Maybe a`      | `Option<A>`      | semidet pred  | `Maybe a`     | `Option A`   |
| `result(T,E)`| `Either e a`  | `Result<A, E>`   | det/semidet   | `Either e a`  | `Except E A` |
| `list(T)`   | `[a]`          | `Vec<A>` / `&[A]`| `list(T)`     | `List a`      | `List A`     |

### Memory layout

- `tuple`: sequential fields, no tag
- `option`: tagged union: tag byte + payload (None = tag 0, Some = tag 1 + value)
- `result`: tagged union: tag byte + payload (Ok = tag 0, Err = tag 1)
- `list`: cons-cell chain (head pointer + tail pointer), refcounted nodes

## Tier 3: Function Types

```
fn(args...) -> effect result
```

Each argument carries a **multiplicity annotation**:

| Multiplicity | Meaning | Source language origin |
|-------------|---------|----------------------|
| `many`      | Unrestricted (default) | Haskell, Koka, Lean 4 |
| `affine`    | Used at most once | Rust ownership (move semantics) |
| `linear`    | Used exactly once | Mercury di/uo modes, Idris 2 QTT `1` |

Effect rows list the effects a function may perform:

| Effect    | Meaning | Source |
|-----------|---------|--------|
| (empty)   | Total / pure | Haskell pure functions, Mercury `det`, Rust `const fn` |
| `exn`     | May fail | Mercury `semidet`, Rust `Result`, Idris 2 partial |
| `io`      | Side effects | Haskell `IO`, Mercury `io`, Rust (default) |
| `choice`  | Nondeterminism | Mercury `multi`/`nondet` |
| `div`     | May diverge | Haskell (default), Lean 4 `partial` |
| `alloc`   | May allocate | Koka heap effects |

Multiple effects combine: Mercury `nondet` = `{exn, choice}`.

## Tier 4: What Is Excluded

These types exist in source languages but do **not** cross interop boundaries.
At the boundary, they are erased to simpler types.

| Excluded | Why | Erased to |
|----------|-----|-----------|
| `Vec n a` (dependent) | Type-level terms can't be represented | `list(a)` |
| `impl Trait` | GHC-specific dictionary passing | concrete function pointer |
| `&'a T` (lifetimes) | Rust-specific borrow checker concept | `T` (copied) or `affine T` (moved) |
| `io.state` | Mercury IO threading | `io` effect |
| Typeclass dictionaries | Compiler-internal representation | explicit function arguments |
| Universe levels | Lean 4 / Idris 2 internal | erased |
| Coercions | GHC Core `Cast` nodes | dropped |

### The rule

If a type requires knowledge of a specific compiler's runtime representation
to interpret, it does not belong in the interop fragment. The interop boundary
is where language-specific semantics are projected onto the shared type system.

## Type Mapping: Source Language to OrganIR

### Haskell (GHC Core)

```
Int#           -> std.int
Double#        -> std.float
Char#          -> std.byte  (or std.int for full Unicode)
Bool           -> std.bool
()             -> std.unit
Maybe a        -> option(T)
Either e a     -> result(T, E)
[a]            -> list(T)
(a, b)         -> tuple(T1, T2)
a -> b         -> fn(many T1) -> {div} T2   (laziness = may diverge)
IO a           -> fn() -> {io} T
```

### Rust (MIR)

```
i64            -> std.int
f64            -> std.float
bool           -> std.bool
u8             -> std.byte
&str           -> std.string
()             -> std.unit
Option<A>      -> option(T)
Result<A, E>   -> result(T, E)
Vec<A>         -> list(T)
(A, B)         -> tuple(T1, T2)
fn(A) -> B     -> fn(affine T1) -> {} T2
Box<T>         -> affine T  (heap-allocated, owned)
&T             -> many T    (shared reference, copied)
&mut T         -> linear T  (exclusive reference)
```

### Mercury (HLDS)

```
int            -> std.int
float          -> std.float
string         -> std.string
list(T)        -> list(T)
det pred       -> fn(...) -> {} T
semidet pred   -> fn(...) -> {exn} T
multi pred     -> fn(...) -> {choice} T
nondet pred    -> fn(...) -> {exn, choice} T
mode in        -> many T
mode out       -> (return value)
mode di        -> linear T
mode uo        -> linear T (return)
```

### Idris 2 (CExp)

```
Int            -> std.int
Double         -> std.float
Char           -> std.byte
String         -> std.string
Bool           -> std.bool
()             -> std.unit
Maybe a        -> option(T)
Either e a     -> result(T, E)
List a         -> list(T)
(a, b)         -> tuple(T1, T2)
0 quantity     -> (erased, not in OrganIR)
1 quantity     -> linear T
w quantity     -> many T
```

### Lean 4 (LCNF)

```
Nat / Int      -> std.int
Float          -> std.float
Bool           -> std.bool
UInt8          -> std.byte
String         -> std.string
Unit           -> std.unit
Option A       -> option(T)
Except E A     -> result(T, E)
List A         -> list(T)
(A, B)         -> tuple(T1, T2)
A -> B         -> fn(many T1) -> {} T2
IO A           -> fn() -> {io} T
partial        -> adds {div} to effect row
```
