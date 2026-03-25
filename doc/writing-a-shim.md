# Writing an Organ Bank Shim

A developer guide for adding new language shims to Organ Bank.

## Overview

A **shim** wraps an existing compiler's internals, extracts its typed IR after
frontend analysis, and emits OrganIR JSON on stdout. Frankenstein then consumes
this JSON to compile multi-language programs down to native code via MLIR.

The extraction pipeline for a shim:

```
source.ext
    |
    v
[Real compiler]    <-- invoked by the shim via System.Process
    |
    v
[Compiler IR]      <-- LLVM IR, CoreFn JSON, SIL text, HLDS dump, etc.
    |
    v
[Parse IR text]    <-- your shim's parser (Text -> [Definition])
    |
    v
[Build OrganIR]    <-- using OrganIR.Build smart constructors
    |
    v
[Emit JSON]        <-- OrganIR.Json.renderOrganIR
    |
    v
stdout             <-- consumed by Frankenstein or organ-validate
```

An **independent frontend** (like `lua-frontend`) is similar but skips the
real compiler entirely: it lexes, parses, and type-checks the source itself,
then emits OrganIR directly.

## Quick Start

This walkthrough creates a hypothetical `ruby-shim` that extracts Ruby's YARV
bytecode via `ruby --dump=insns`.

### 1. Create the directory structure

```
organ-bank/
  ruby-shim/
    src/
      Main.hs
      OrganBank/
        RubyShim.hs
    ruby-shim.cabal
```

### 2. Write the .cabal file

Model this on `/home/nyc/src/organ-bank/c-shim/c-shim.cabal`:

```cabal
cabal-version: 3.0
name:          ruby-shim
version:       0.1.0
synopsis:      Extract YARV IR from Ruby and emit OrganIR JSON
license:       GPL-2.0-only
build-type:    Simple

executable ruby-organ
  default-language: GHC2024
  default-extensions: OverloadedStrings
  hs-source-dirs: src
  main-is: Main.hs
  other-modules: OrganBank.RubyShim
  build-depends:
    base >= 4.20 && < 5,
    text,
    process,
    filepath,
    organ-ir
  ghc-options: -Wall
```

Key points:
- The executable name follows the pattern `mylang-organ`.
- Depend on `organ-ir` for the shared types and builders.
- Use `GHC2024` and `OverloadedStrings` to match the project conventions.

### 3. Add to cabal.project

Add one line to `/home/nyc/src/organ-bank/cabal.project`:

```
  ruby-shim/
```

### 4. Implement the extraction function

Create `ruby-shim/src/OrganBank/RubyShim.hs`. The central function has the
signature:

```haskell
extractOrganIR :: FilePath -> IO (Either String Text)
```

Here is a minimal implementation:

```haskell
module OrganBank.RubyShim (extractOrganIR) where

import Control.Exception (SomeException, catch)
import Data.Text (Text)
import Data.Text qualified as T
import OrganIR.Build qualified as IR
import OrganIR.Json (renderOrganIR)
import OrganIR.Types qualified as IR
import System.Exit (ExitCode(..))
import System.FilePath (takeBaseName)
import System.Process (readProcessWithExitCode)

detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "ruby" ["--version"] ""
    pure $ case ec of
        ExitSuccess -> "ruby-shim-0.1 (" <> T.strip (T.pack out) <> ")"
        _ -> "ruby-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "ruby-shim-0.1"

extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    (ec, out, err) <-
        readProcessWithExitCode "ruby" ["--dump=insns", inputPath] ""
    case ec of
        ExitFailure _ -> pure $ Left $ "ruby failed: " ++ err
        ExitSuccess -> do
            let defs = parseYarvOutput (T.pack out)
                modName = takeBaseName inputPath
                ir = IR.simpleOrganIR
                       IR.LRuby        -- you'd add LRuby to SourceLang
                       shimVer
                       (T.pack modName)
                       inputPath
                       defs
            pure $ Right (renderOrganIR ir)

parseYarvOutput :: Text -> [IR.Definition]
parseYarvOutput _ = []  -- TODO: implement actual parsing
```

### 5. Wire up Main.hs

Every shim has the same boilerplate Main. Copy from
`/home/nyc/src/organ-bank/c-shim/src/Main.hs`:

```haskell
module Main (main) where

import System.Environment (getArgs)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO
import OrganBank.RubyShim (extractOrganIR)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [inputPath] -> do
      result <- extractOrganIR inputPath
      case result of
        Left err -> do
          hPutStrLn stderr $ "Error: " ++ err
          exitFailure
        Right json -> TIO.putStr json
    _ -> do
      hPutStrLn stderr "Usage: ruby-organ <file.rb>"
      exitFailure
```

### 6. Add the SourceLang constructor

Add a new constructor to the `SourceLang` type in
`/home/nyc/src/organ-bank/organ-ir/src/OrganIR/Types.hs`:

```haskell
    | LRuby
```

Then update `OrganIR.Json` to handle the new tag in its JSON rendering.

### 7. Test

```bash
# Build
cabal build ruby-shim

# Run on a test file
cabal -v0 run ruby-organ -- test/examples/hello.rb > /tmp/ruby-test.json

# Validate the output
cabal -v0 run organ-validate -- /tmp/ruby-test.json

# Pretty-print
cabal -v0 run organ-validate -- --pretty /tmp/ruby-test.json

# Validate schema compliance
cabal -v0 run organ-validate -- --schema /tmp/ruby-test.json
```

Add a test case to `/home/nyc/src/organ-bank/test/smoke-test.sh`:

```bash
run_test "ruby-shim/hello.rb" ruby-organ "$EXAMPLES/hello.rb" ruby
```

And create `test/examples/hello.rb` with a minimal test program.


## The OrganIR Builder API

All builders are in `/home/nyc/src/organ-bank/organ-ir/src/OrganIR/Build.hs`.
Import as:

```haskell
import OrganIR.Build qualified as IR
import OrganIR.Types qualified as IR
```

### Document constructors

| Constructor | Use when |
|-------------|----------|
| `simpleOrganIR lang shimVer modName srcFile defs` | Most shims: one source file, no exports list |
| `organIRWithExports lang shimVer modName srcFile exports defs` | When the source language has explicit exports (Erlang, Lua, PureScript) |
| `organIR lang shimVer modName defs` | Minimal: no source file path |
| `organIRFull metadata module_` | Full control over metadata and module structure (PureScript shim uses this) |

### Definition constructors

| Constructor | Signature | Use when |
|-------------|-----------|----------|
| `funDef` | `Text -> Ty -> Expr -> Int -> Definition` | Named function with known arity |
| `valDef` | `Text -> Ty -> Expr -> Definition` | Top-level value (constant, CAF) |
| `valDefSimple` | `Text -> Expr -> Definition` | Value with type `TAny` (dynamically typed languages) |
| `conDef` | `Text -> Ty -> Int -> Definition` | Data constructor |
| `funDefNA` | `Text -> Int -> Expr -> Definition` | Function keyed by name/arity (Prolog, Erlang) |

### Expression builders

**Literals:**

| Builder | Example | Produces |
|---------|---------|----------|
| `eInt 42` | | `ELit (LitInt 42)` |
| `eFloat 3.14` | | `ELit (LitFloat 3.14)` |
| `eString "hello"` | | `ELit (LitString "hello")` |
| `eBool True` | | `ELit (LitBool True)` |
| `eNil` | | `ECon (localName "nil") []` |

**Variables and application:**

| Builder | Produces |
|---------|----------|
| `eVar "x"` | `EVar (Name "x" 0)` |
| `eApp f [a, b]` | `EApp f [a, b]` |
| `eApp1 f a` | `EApp f [a]` -- single-arg convenience |

**Lambda and let:**

| Builder | Example | Produces |
|---------|---------|----------|
| `eLam ["x", "y"] body` | | `ELam [LamParam "x" Nothing, LamParam "y" Nothing] body` |
| `eLet1 "x" val body` | | `ELet [LetBind "x" Nothing val] body` |
| `eLet [("x", e1), ("y", e2)] body` | | `ELet [LetBind "x" Nothing e1, LetBind "y" Nothing e2] body` |

**Control flow:**

| Builder | Semantics |
|---------|-----------|
| `eIf cond t f` | Desugars to `ECase cond [branch "true" [] t, branch "false" [] f]` |
| `eMatch scrut [(pat, body)]` | Case expression with explicit patterns |
| `eGuarded guard body fallback` | Guarded expression (synonym for `eIf`) |
| `eSeq [e1, e2, e3]` | Evaluate all, return last; binds intermediates with `ELet` |
| `eAnd [e1, e2]` | Short-circuit conjunction |
| `eOr [e1, e2]` | Short-circuit disjunction |

**Patterns:**

| Builder | Produces |
|---------|----------|
| `pWild` | `PatWild` |
| `pVar "x"` | `PatVar (Name "x" 0) Nothing` |
| `pCon "Just" ["val"]` | `PatCon (localName "Just") [PatBinder "val" Nothing]` |
| `branch "Just" ["val"] body` | `Branch (pCon "Just" ["val"]) body` |

### Type builders

| Builder | Produces | Use for |
|---------|----------|---------|
| `tAny` | `TAny` | Untyped / dynamically typed / post-erasure |
| `tCon "Int"` | `TCon (localName "Int")` | Named ground type |
| `tApp "List" [tCon "Int"]` | `TApp (localName "List") [TCon ...]` | Parameterized type |
| `tFn [tCon "Int"] (tCon "Bool")` | `TFn [FnArg Nothing (TCon "Int")] pureEffect (TCon "Bool")` | Pure function type |
| `tVar "a"` | `TVar (Name "a" 0)` | Type variable |

For function types with effects or multiplicity, construct `TFn` directly:

```haskell
-- fn(affine Int) -> {io} String
IR.TFn
  [IR.FnArg (Just IR.Affine) (IR.tCon "Int")]
  (IR.EffectRow [IR.localName "io"] Nothing)
  (IR.tCon "String")
```

### Effect rows

```haskell
-- Pure (no effects)
IR.pureEffect                          -- EffectRow [] Nothing

-- IO effect
IR.EffectRow [IR.localName "io"] Nothing

-- Multiple effects: {exn, choice}
IR.EffectRow [IR.localName "exn", IR.localName "choice"] Nothing

-- Open effect row with tail variable
IR.EffectRow [IR.localName "io"] (Just (IR.name "e"))
```

### Multiplicity

Use multiplicity when the source language has ownership or linearity:

| Value | Source | Meaning |
|-------|--------|---------|
| `Many` | Haskell, Koka, OCaml, most languages | Unrestricted use |
| `Affine` | Rust move semantics, Swift | Used at most once |
| `Linear` | Mercury di/uo modes, Idris 2 `1`, Rust `&mut` | Used exactly once |

For languages without linearity/ownership, omit multiplicity (use `Nothing`
in `FnArg`).


## Type Mapping Decision Tree

When translating your source language's types to OrganIR, follow this decision
tree:

### Is it a ground scalar type?

Map to a Tier 1 type:

| Source concept | OrganIR | Builder |
|---------------|---------|---------|
| Machine integer (any width) | `std.int` | `tCon "Int"` |
| Floating point | `std.float` | `tCon "Float"` |
| Boolean | `std.bool` | `tCon "Bool"` |
| Byte / char | `std.byte` | `tCon "Byte"` |
| String / text | `std.string` | `tCon "String"` |
| Unit / void / nil | `std.unit` | `tCon "Unit"` |

### Is it a standard container?

Map to a Tier 2 structural type:

| Source concept | OrganIR | Builder |
|---------------|---------|---------|
| Tuple / pair | `tuple(T1, T2)` | `tApp "Tuple" [t1, t2]` |
| Optional / nullable | `option(T)` | `tApp "Option" [t]` |
| Result / either | `result(T, E)` | `tApp "Result" [t, e]` |
| List / array / vector | `list(T)` | `tApp "List" [t]` |

### Is it a function type?

Map to a Tier 3 function type with effects:

1. Determine the **argument types** and map each recursively.
2. Determine the **multiplicity** of each argument (default: `Nothing` / unrestricted).
3. Determine the **effect row**: pure, io, exn, choice, div, alloc.
4. Determine the **return type** and map recursively.

### Is it none of the above?

Use `TAny`. This is appropriate for:
- Dynamically typed languages (Lua, Ruby, Scheme, Common Lisp, Prolog, Forth)
- Post-erasure IR where types have been thrown away (Lean 4 LCNF, Idris 2 CExp)
- Compiler-internal types that don't map to the interop fragment
- Placeholder during development (get the shim working, refine types later)

See `/home/nyc/src/organ-bank/doc/type-mapping-reference.md` for per-language
type tables.


## Expression Translation Strategies

Organ Bank shims use five distinct strategies depending on what the source
compiler exposes. Choose the one that best fits your language.

### Strategy 1: Direct AST access (GHC API, rustc_private)

Use this when you can link against the compiler as a library and call its
internal APIs directly. This gives the richest IR but couples you to the
compiler's Haskell/Rust dependencies.

**Used by:** ghc-shim (GHC API), rustc-shim (rustc_private)

### Strategy 2: Text IR parsing

Invoke the compiler to dump a text representation of its IR, then parse
that text in Haskell. This is the most common strategy.

**Used by:** c-shim (LLVM IR), cpp-shim (LLVM IR), fortran-shim (GIMPLE),
ada-shim (GIMPLE), mmc-shim (HLDS dump), swift-shim (SIL), ocaml-shim
(Lambda), koka-shim (Core), idris2-shim (case trees), sml-shim (type basis)

From the c-shim (`/home/nyc/src/organ-bank/c-shim/src/OrganBank/CShim.hs`):

```haskell
extractOrganIR inputPath = do
    shimVer <- detectCompilerVersion
    (ec, out, err) <-
        readProcessWithExitCode
            "clang"
            ["-emit-llvm", "-S", "-O2", "-o", "-", inputPath]
            ""
    case ec of
        ExitFailure _ -> pure $ Left $ "clang failed: " ++ err
        ExitSuccess -> do
            let defs = parseLlvmIR (T.pack out)
                modName = takeBaseName inputPath
                json = renderOrganIR $
                    IR.simpleOrganIR IR.LC shimVer (T.pack modName) inputPath
                        (map funcToIR defs)
            pure $ Right json
```

### Strategy 3: JSON IR parsing

Some compilers emit JSON directly. Parse the JSON and translate each node.
Because Organ Bank avoids the `aeson` dependency (GHC 9.14 compatibility),
JSON parsing is done by hand with `Data.Text` operations.

**Used by:** purs-shim (CoreFn JSON)

From the purs-shim (`/home/nyc/src/organ-bank/purs-shim/src/OrganBank/PursShim.hs`):

```haskell
-- Run purs compile --dump-corefn, read output/Module/corefn.json
coreFnToOrganIR shimVer sourceFile coreFnText =
    let modName_ = extractJsonString "moduleName" coreFnText
        exports  = extractJsonStringArray "exports" coreFnText
        decls    = extractDecls coreFnText
        ...
     in renderOrganIR (IR.OrganIR metadata module_)
```

### Strategy 4: Delegation to external script

If the source compiler's introspection API is most easily accessed from
the source language itself, write a script in that language that emits
OrganIR JSON, and have the Haskell shim invoke it and validate the output.

**Used by:** julia-shim (Julia script), cl-shim (Lisp script), scheme-shim
(Guile script), zig-shim (Zig tool)

From the julia-shim (`/home/nyc/src/organ-bank/julia-shim/src/OrganBank/JuliaShim.hs`):

```haskell
extractOrganIR inputPath = do
    scriptPath <- findExtractScript
    case scriptPath of
        Nothing -> pure $ Left "Could not locate extract.jl script"
        Just script -> do
            (exitCode, stdout, stderr_) <-
                readProcessWithExitCode
                    "julia" ["--startup-file=no", script, inputPath] ""
            case exitCode of
                ExitSuccess -> pure $ validateJson (T.pack stdout)
                ExitFailure code ->
                    pure $ Left $ "julia exited with code "
                        <> show code <> ":\n" <> stderr_
```

The external script does the heavy lifting; the Haskell shim validates
that the output is well-formed OrganIR.

### Strategy 5: Independent frontend

For languages where no suitable compiler exists, or where the compiler's
IR is inaccessible, write a full frontend (lexer + parser + optional type
checker) in Haskell. This is more work but gives complete control.

**Used by:** sml-frontend, erlang-frontend, scheme-frontend, prolog-frontend,
lua-frontend, forth-frontend

From the lua-frontend (`/home/nyc/src/organ-bank/lua-frontend/src/LuaFrontend/ToOrganIR.hs`):

```haskell
emitLuaIR modName srcFile block =
    let defs = blockToDefs block
        exports = map (IR.nameText . IR.qnName . IR.defName) defs
    in  renderOrganIR $
            IR.organIRWithExports IR.LLua "lua-frontend-0.1"
                (T.pack modName) srcFile exports defs
```

Frontends are structured as libraries with a separate `app/Main.hs`:

```
lua-frontend/
  src/
    LuaFrontend/
      Lexer.hs       -- source -> tokens
      AST.hs         -- language-specific AST types
      Parser.hs      -- tokens -> AST
      ToOrganIR.hs   -- AST -> OrganIR
  app/
    Main.hs          -- CLI wrapper
  lua-frontend.cabal -- exposes library + executable
```

### Best-effort with fallback

Regardless of strategy, shims should degrade gracefully. If you cannot parse
an expression, emit a placeholder rather than crashing:

```haskell
-- From purs-shim: unrecognised expression types fall back to a literal 0
parseCoreFnExpr modName_ txt
    | T.null txt = IR.ELit (IR.LitInt 0)
    | otherwise =
        let exprType = extractJsonString "type" txt
         in case exprType of
                "Literal"     -> parseLiteral txt
                "Var"         -> parseVar modName_ txt
                ...
                _             -> IR.ELit (IR.LitInt 0)  -- fallback
```


## Testing Your Shim

### 1. Create a test example

Add a minimal source file at `test/examples/hello.ext`. Keep it simple --
one or two functions that exercise basic features:

```c
/* test/examples/hello.c */
int factorial(int n) { return n <= 1 ? 1 : n * factorial(n-1); }
```

### 2. Add to smoke-test.sh

Add a line to `/home/nyc/src/organ-bank/test/smoke-test.sh`:

```bash
run_test "ruby-shim/hello.rb"  ruby-organ  "$EXAMPLES/hello.rb"  ruby
```

The fourth argument is the compiler binary name. If it is not on PATH, the
test is automatically skipped.

### 3. Run organ-validate

```bash
# Basic validation: checks JSON structure, required fields, consistency
cabal -v0 run ruby-organ -- test/examples/hello.rb | \
  cabal -v0 run organ-validate

# Pretty-print the IR for visual inspection
cabal -v0 run ruby-organ -- test/examples/hello.rb | \
  cabal -v0 run organ-validate -- --pretty

# Schema validation (stricter)
cabal -v0 run ruby-organ -- test/examples/hello.rb | \
  cabal -v0 run organ-validate -- --schema
```

### 4. Compare with expected output using organ-diff

```bash
# Save expected output
cabal -v0 run ruby-organ -- test/examples/hello.rb > test/expected/hello-rb.json

# Later, check for regressions
cabal -v0 run ruby-organ -- test/examples/hello.rb | \
  cabal -v0 run organ-diff -- test/expected/hello-rb.json -
```

### 5. Run the full smoke test suite

```bash
./test/smoke-test.sh
```


## Naming Conventions

### Package names

| Type | Pattern | Example |
|------|---------|---------|
| Compiler shim | `mylang-shim` | `c-shim`, `purs-shim`, `koka-shim` |
| Independent frontend | `mylang-frontend` | `lua-frontend`, `sml-frontend` |

### Executable names

| Type | Pattern | Example |
|------|---------|---------|
| Compiler shim | `mylang-organ` | `c-organ`, `purs-organ`, `koka-organ` |
| Independent frontend | `mylang-organ-fe` | `lua-organ-fe`, `sml-organ-fe` |

### Module names

| Type | Pattern | Example |
|------|---------|---------|
| Compiler shim | `OrganBank.MyLangShim` | `OrganBank.CShim`, `OrganBank.PursShim` |
| Frontend AST | `MyLangFrontend.AST` | `LuaFrontend.AST` |
| Frontend Lexer | `MyLangFrontend.Lexer` | `LuaFrontend.Lexer` |
| Frontend Parser | `MyLangFrontend.Parser` | `LuaFrontend.Parser` |
| Frontend IR emit | `MyLangFrontend.ToOrganIR` | `LuaFrontend.ToOrganIR` |

### Source language tag

Add a constructor to `SourceLang` in `OrganIR.Types`:

```haskell
data SourceLang
    = LHaskell | LRust | LMercury | ... | LMyLang
```


## Common Patterns

### Invoking a compiler and capturing output

```haskell
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)

runCompiler :: [String] -> IO (Either String String)
runCompiler args = do
    (ec, out, err) <- readProcessWithExitCode "mycompiler" args ""
    pure $ case ec of
        ExitSuccess   -> Right out
        ExitFailure _ -> Left err
```

### Detecting compiler version

Every shim should detect the compiler version and include it in metadata.
Catch exceptions so the shim works even if the compiler is missing or
the version flag is different:

```haskell
import Control.Exception (SomeException, catch)

detectCompilerVersion :: IO Text
detectCompilerVersion = do
    (ec, out, _) <- readProcessWithExitCode "mycompiler" ["--version"] ""
    pure $ case ec of
        ExitSuccess -> "my-shim-0.1 (" <> T.strip (T.pack out) <> ")"
        _           -> "my-shim-0.1"
  `catch` \(_ :: SomeException) -> pure "my-shim-0.1"
```

### Parsing text blocks into definitions

A common pattern for text-based IR: split on function headers, extract the
name and body for each function.

```haskell
-- From c-shim: parse LLVM IR "define" blocks
parseDefines :: [Text] -> [LlvmFunc]
parseDefines [] = []
parseDefines (l : ls)
    | "define " `T.isPrefixOf` l =
        let (bodyLines, rest) = collectBody ls 1
            func = parseDefine l bodyLines
         in func : parseDefines rest
    | otherwise = parseDefines ls
```

### Converting parsed IR to OrganIR definitions

```haskell
-- From c-shim: LlvmFunc -> Definition
funcToIR :: LlvmFunc -> IR.Definition
funcToIR f =
    IR.Definition
        { IR.defName = IR.localName (lfName f)
        , IR.defType =
            IR.TFn
                (map (\(ty, _) -> IR.FnArg Nothing (llvmTypeToIR ty)) (lfParams f))
                IR.pureEffect
                (llvmTypeToIR (lfRetTy f))
        , IR.defExpr =
            IR.EApp (IR.eVar "ssa_body") (map blockToIR (lfBlocks f))
        , IR.defSort = IR.SFun
        , IR.defVisibility = IR.Public
        , IR.defArity = length (lfParams f)
        }
```

### Handling errors gracefully

Shims should never crash. Use `Either String Text` as the return type, and
catch exceptions at the boundary:

```haskell
extractOrganIR :: FilePath -> IO (Either String Text)
extractOrganIR inputPath = do
    result <- try $ do
        shimVer <- detectCompilerVersion
        irText <- invokeCompiler inputPath
        let defs = parseIR irText
        pure $ renderOrganIR (IR.simpleOrganIR ...)
    pure $ case result of
        Left (e :: SomeException) -> Left (show e)
        Right json -> Right json
```

### Mapping LLVM/GCC type strings

For C-family shims working with LLVM IR or GIMPLE:

```haskell
-- From c-shim
llvmTypeToIR :: Text -> IR.Ty
llvmTypeToIR t = case T.strip t of
    "i1"     -> IR.tCon "Bool"
    "i32"    -> IR.tCon "Int32"
    "i64"    -> IR.tCon "Int64"
    "float"  -> IR.tCon "Float32"
    "double" -> IR.tCon "Float64"
    "void"   -> IR.tCon "Void"
    s | "*" `T.isSuffixOf` s -> IR.tCon "Ptr"
      | otherwise -> IR.tCon s
```

### Translating language-specific constructs

For constructs that have no OrganIR equivalent, represent them as function
applications to named placeholders:

```haskell
-- Lua table constructor -> application of "table" pseudo-function
ETable fields -> IR.EApp (IR.eVar "table") (map fieldToIR fields)

-- Lua method call obj:method(args) -> nested application
EMethodCall obj meth args ->
    IR.EApp (IR.EApp (IR.eVar ":") [exprToIR obj, IR.eString meth])
            (map exprToIR args)
```


## Reference Files

| File | Purpose |
|------|---------|
| `organ-ir/src/OrganIR/Types.hs` | All data types: `OrganIR`, `Expr`, `Ty`, `SourceLang` |
| `organ-ir/src/OrganIR/Build.hs` | Smart constructors for building IR |
| `organ-ir/src/OrganIR/Json.hs` | `renderOrganIR :: OrganIR -> Text` |
| `organ-ir/src/OrganIR/Parse.hs` | `parseOrganIR :: Text -> Either String OrganIR` |
| `organ-ir/src/OrganIR/Validate.hs` | Validation checks |
| `organ-ir/src/OrganIR/Pretty.hs` | Human-readable pretty-printer |
| `spec/interop-types.md` | Interop type system spec |
| `spec/organ-ir.schema.json` | JSON Schema for OrganIR |
| `c-shim/` | Minimal shim example (text IR parsing) |
| `purs-shim/` | Full shim example (JSON IR parsing) |
| `julia-shim/` | Delegation pattern example |
| `lua-frontend/` | Independent frontend example |
| `test/smoke-test.sh` | Smoke test runner |
| `cabal.project` | Package list |
