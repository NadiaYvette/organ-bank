/-
  LCNF extraction: walks Lean 4's post-erasure LCNF IR and converts
  it to OrganIR definitions.

  Uses:
  - `Lean.Compiler.LCNF.Decl` for top-level declarations
  - `Lean.Compiler.LCNF.Code` for the ANF-style body
  - `Lean.Compiler.LCNF.PhaseExt` to iterate compiled declarations
-/
import Lean
import Lean.Compiler.LCNF.Basic
import Lean.Compiler.LCNF.PhaseExt
import OrganBank.OrganIR

namespace OrganBank.LcnfExtract

open Lean Lean.Compiler.LCNF
open OrganBank.OrganIR (QName Definition Ty Expr)

/-- Convert an LCNF FVarId to (name, unique) pair. -/
private def fvarToNamePair (fv : FVarId) : String × Nat :=
  (fv.name.toString, fv.name.hash.toNat)

/-- Convert an LCNF Arg to an OrganIR Expr. -/
private def argToOrganExpr (a : Arg) : OrganIR.Expr :=
  match a with
  | .erased => .eunreach
  | .fvar fv =>
    let (n, u) := fvarToNamePair fv
    .evar n u
  | .type _ => .eunreach

/-- Filter non-erased arguments to OrganIR Exprs. -/
private def filterArgs (args : Array Arg) : List OrganIR.Expr :=
  args.toList.filterMap fun a => match a with
    | .fvar fv => some (argToOrganExpr (.fvar fv))
    | _ => none

/-- Convert an LCNF LetValue to an OrganIR Expr. -/
private def letValueToExpr (v : LetValue) : OrganIR.Expr :=
  match v with
  | .value (.natVal n) => .elit_int n
  | .value (.strVal s) => .elit_str s
  | .erased => .eunreach
  | .proj typeName idx struct =>
    let (sn, su) := fvarToNamePair struct
    .eproj typeName.toString idx sn su
  | .const declName _us args =>
    let dn := declName.toString
    let du := declName.hash.toNat
    match filterArgs args with
    | [] => .evar dn du
    | as => .eapp dn du as
  | .fvar fv args =>
    let (n, u) := fvarToNamePair fv
    match filterArgs args with
    | [] => .evar n u
    | as => .eapp n u as

/-- Convert LCNF Code (ANF) to OrganIR Expr (nested lets). -/
partial def codeToExpr (code : Code) : OrganIR.Expr :=
  match code with
  | .let decl k =>
    let (n, u) := fvarToNamePair decl.fvarId
    .elet n u (letValueToExpr decl.value) (codeToExpr k)
  | .fun decl k =>
    let (n, u) := fvarToNamePair decl.fvarId
    let ps := decl.params.toList.map fun p => fvarToNamePair p.fvarId
    let lamBody := codeToExpr decl.value
    .elet n u (.elam ps lamBody) (codeToExpr k)
  | .jp decl k =>
    let (n, u) := fvarToNamePair decl.fvarId
    let ps := decl.params.toList.map fun p => fvarToNamePair p.fvarId
    let jpBody := codeToExpr decl.value
    .elet n u (.elam ps jpBody) (codeToExpr k)
  | .jmp fv args =>
    let (n, u) := fvarToNamePair fv
    .eapp n u (filterArgs args)
  | .cases c =>
    let (dn, du) := fvarToNamePair c.discr
    let alts := c.alts.toList.map fun alt =>
      match alt with
      | .default code => ("_", ([] : List (String × Nat)), codeToExpr code)
      | .alt ctorName params code =>
        let ps := params.toList.map fun p => fvarToNamePair p.fvarId
        (ctorName.toString, ps, codeToExpr code)
    .ecase dn du alts
  | .return fv =>
    let (n, u) := fvarToNamePair fv
    .evar n u
  | .unreach _ =>
    .eunreach

/-- Split a Lean Name into (module, shortName) strings. -/
private def splitLeanName (n : Name) : String × String :=
  let components := n.componentsRev
  match components with
  | short :: rest =>
    let moduleParts := rest.reverse.map Name.toString
    (".".intercalate moduleParts, short.toString)
  | [] => ("", n.toString)

/-- Convert a full LCNF Decl to an OrganIR Definition. -/
def declToDefinition (decl : Decl) : OrganIR.Definition :=
  let (moduleName, shortName) := splitLeanName decl.name
  let qname : QName := {
    module := moduleName
    text := shortName
    unique := decl.name.hash.toNat
  }
  let arity := decl.params.size
  let coreExpr := codeToExpr decl.value
  let body : OrganIR.Expr :=
    if decl.params.isEmpty then coreExpr
    else
      let ps := decl.params.toList.map fun p => fvarToNamePair p.fvarId
      .elam ps coreExpr
  let sort := "fun"
  { name := qname
    ty := .any
    expr := body
    sort := sort
    visibility := "public"
    arity := arity }

/-- Extract LCNF declarations from the main module using CoreM.
    Uses Phase.base for post-erasure but pre-monomorphization decls. -/
def extractMainModuleDeclsCoreM : CoreM (Array OrganIR.Definition) := do
  let defsRef ← IO.mkRef (#[] : Array OrganIR.Definition)
  Lean.Compiler.LCNF.forEachMainModuleDecl (fun decl => do
    defsRef.modify (·.push (declToDefinition decl))
  ) (phase := .base)
  defsRef.get

end OrganBank.LcnfExtract
