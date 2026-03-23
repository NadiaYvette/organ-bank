/-
  lean4-organ: Extract Lean 4 LCNF and emit OrganIR JSON.

  Lean 4's compiler uses LCNF (Lambda-Compiled Normal Form) as its
  post-erasure IR before code generation. This shim extracts LCNF
  declarations and emits OrganIR JSON.

  Strategy: Use Lean 4's Environment to access compiled declarations
  after elaboration and compilation to LCNF.
-/

namespace OrganBank

/-- A simplified OrganIR definition extracted from LCNF. -/
structure OrganDef where
  name : String
  moduleName : String
  arity : Nat
  unique : Nat
  deriving Repr

/-- Escape a string for JSON output. -/
def jsonEscape (s : String) : String :=
  s.foldl (fun acc c =>
    acc ++ match c with
    | '"'  => "\\\""
    | '\\' => "\\\\"
    | '\n' => "\\n"
    | c    => c.toString
  ) ""

/-- Emit a single definition as OrganIR JSON. -/
def emitDef (d : OrganDef) : String :=
  let args := ", ".intercalate (List.replicate d.arity
    "{\"multiplicity\": \"many\", \"type\": {\"con\": {\"qname\": {\"module\": \"std\", \"name\": {\"text\": \"any\"}}}}}")
  s!"      \{
        \"name\": \{\"module\": \"{jsonEscape d.moduleName}\", \"name\": \{\"text\": \"{jsonEscape d.name}\", \"unique\": {d.unique}}},
        \"type\": \{\"fn\": \{\"args\": [{args}], \"effect\": \{\"effects\": []}, \"result\": \{\"con\": \{\"qname\": \{\"module\": \"std\", \"name\": \{\"text\": \"any\"}}}}}},
        \"expr\": \{\"elit\": \{\"int\": 0}},
        \"sort\": \"fun\",
        \"visibility\": \"public\"
      }"

/-- Emit a complete OrganIR JSON document. -/
def emitOrganIR (modName : String) (defs : List OrganDef) : String :=
  let defsJson := ",\n".intercalate (defs.map emitDef)
  s!"\{
  \"schema_version\": \"1.0.0\",
  \"metadata\": \{
    \"source_language\": \"lean4\",
    \"shim_version\": \"0.1.0\"
  },
  \"module\": \{
    \"name\": \"{jsonEscape modName}\",
    \"definitions\": [{defsJson}],
    \"data_types\": [],
    \"effect_decls\": []
  }
}"

end OrganBank

/--
  Main entry point.

  Usage: lean4-organ <file.lean>

  For the initial version, this uses `lean --run` to elaborate the file
  and extract declaration names. A more complete version would use the
  Lean 4 compiler API to access LCNF directly.

  The real extraction strategy for Lean 4:
  1. Import the target module into the environment
  2. Walk `Environment.constants` for function definitions
  3. Access LCNF via the compiler's internal passes
  4. Map LCNF to OrganIR (erased types become `any`, do-notation effects preserved)
-/
def main (args : List String) : IO UInt32 := do
  match args with
  | [inputPath] =>
    -- For now, use lean --print-prefix to verify Lean is available,
    -- then parse the source file for top-level definitions.
    -- A full implementation would use the compiler API.
    let contents ← IO.FS.readFile inputPath
    let modName := inputPath.dropRight 5  -- strip .lean
    let modName := modName.replace "/" "."
    let defs := extractTopLevelDefs modName contents
    IO.println (OrganBank.emitOrganIR modName defs)
    return 0
  | _ =>
    IO.eprintln "Usage: lean4-organ <file.lean>"
    IO.eprintln "Extracts Lean 4 definitions and emits OrganIR JSON on stdout."
    return 1

/-- Extract top-level definitions by parsing `def` and `theorem` declarations.
    This is a placeholder; the real version would walk LCNF. -/
def extractTopLevelDefs (modName : String) (contents : String) : List OrganBank.OrganDef :=
  let lines := contents.splitOn "\n"
  let defs := lines.filterMap fun line =>
    let trimmed := line.trimLeft
    if trimmed.startsWith "def " then
      let rest := (trimmed.drop 4).trimLeft
      let name := rest.takeWhile fun c => c.isAlpha || c.isDigit || c == '_' || c == '\''
      if name.isEmpty then none
      else some name
    else if trimmed.startsWith "theorem " then
      let rest := (trimmed.drop 8).trimLeft
      let name := rest.takeWhile fun c => c.isAlpha || c.isDigit || c == '_' || c == '\''
      if name.isEmpty then none
      else some name
    else none
  defs.enum.map fun (idx, name) =>
    { name := name
    , moduleName := modName
    , arity := 0  -- would come from LCNF
    , unique := idx + 1 }
