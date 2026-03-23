/-
  lean4-organ: Extract Lean 4 LCNF and emit OrganIR JSON.

  Usage: lean4-organ <file.lean>

  Compiles the input file through Lean 4's frontend (parsing, elaboration,
  LCNF compilation), then extracts post-erasure LCNF declarations and
  emits OrganIR JSON on stdout.
-/
import Lean
import Lean.Elab.Frontend
import Lean.Elab.Import
import Lean.Compiler.LCNF.PhaseExt
import OrganBank.OrganIR
import OrganBank.LcnfExtract

open Lean Lean.Elab
open OrganBank.OrganIR
open OrganBank.LcnfExtract

/-- Run the Lean 4 frontend on a source file and return the final Environment. -/
def elaborateFile (inputPath : System.FilePath) : IO Environment := do
  let input ← IO.FS.readFile inputPath
  -- Enable the new LCNF compiler so baseExt gets populated
  let opts := ({} : Options) |>.setBool `compiler.enableNew true
  initSearchPath (← findSysroot)
  let inputCtx : Parser.InputContext := {
    input := input
    fileName := inputPath.toString
    fileMap := FileMap.ofString input
  }
  let (header, parserState, messages) ← Parser.parseHeader inputCtx
  let (headerEnv, messages) ← Elab.processHeader header opts messages inputCtx
  if messages.hasErrors then
    for msg in messages.toList do
      if msg.severity == .error then
        IO.eprintln (← msg.toString)
    throw (IO.userError "Failed to process header")
  -- Set up the FrontendM state and context
  let frontendCtx : Frontend.Context := { inputCtx := inputCtx }
  let cmdState := Command.mkState headerEnv messages opts
  let frontendState : Frontend.State := {
    commandState := cmdState
    parserState := parserState
    cmdPos := parserState.pos
  }
  -- Run the frontend to elaborate all commands
  let (_, finalState) ← Frontend.processCommands.run frontendCtx |>.run frontendState
  let finalCmdState := finalState.commandState
  if finalCmdState.messages.hasErrors then
    for msg in finalCmdState.messages.toList do
      if msg.severity == .error then
        IO.eprintln (← msg.toString)
    throw (IO.userError "Elaboration failed")
  return finalCmdState.env

/-- Extract LCNF declarations from an environment using CoreM. -/
def extractFromEnv (env : Environment) : IO (Array Definition) := do
  let ctx : Core.Context := {
    fileName := "<organ-bank>"
    fileMap := FileMap.ofString ""
  }
  let state : Core.State := { env := env }
  let (defs, _) ← extractMainModuleDeclsCoreM.toIO ctx state
  return defs

/-- Extract module name from file path: "Foo/Bar.lean" → "Foo.Bar" -/
def fileToModuleName (path : String) : String :=
  let stripped := if path.endsWith ".lean" then path.dropRight 5 else path
  stripped.replace "/" "." |>.replace "\\" "."

def main (args : List String) : IO UInt32 := do
  match args with
  | [inputPath] =>
    try
      let env ← elaborateFile inputPath
      let defs ← extractFromEnv env
      let modName := fileToModuleName inputPath
      let organModule : OrganBank.OrganIR.Module := {
        moduleName := modName
        sourceFile := inputPath
        compilerVersion := s!"lean4-{Lean.versionString}"
        definitions := defs
      }
      IO.println organModule.toJson.pretty
      return 0
    catch e =>
      IO.eprintln s!"Error: {e}"
      return 1
  | _ =>
    IO.eprintln "Usage: lean4-organ <file.lean>"
    IO.eprintln ""
    IO.eprintln "Extracts Lean 4 LCNF (post-erasure IR) and emits OrganIR JSON."
    return 1
