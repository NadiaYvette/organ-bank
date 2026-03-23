/-
  OrganIR JSON types and serialization.

  Defines a minimal OrganIR representation and converts it to Lean.Json
  for output. This is the target format for all organ-bank shims.
-/
import Lean.Data.Json

namespace OrganBank.OrganIR

open Lean (Json)

/-- Qualified name in OrganIR. -/
structure QName where
  module : String
  text : String
  unique : Nat := 0

def QName.toJson (q : QName) : Json :=
  .mkObj [
    ("module", .str q.module),
    ("name", .mkObj [
      ("text", .str q.text),
      ("unique", .num q.unique)
    ])
  ]

/-- OrganIR type. -/
inductive Ty where
  | con (mod : String) (text : String)
  | any

/-- OrganIR expression. -/
inductive Expr where
  | evar (name : String) (unique : Nat)
  | elit_int (val : Nat)
  | elit_str (val : String)
  | eapp (fnName : String) (fnUnique : Nat) (args : List Expr)
  | elam (paramNames : List (String × Nat)) (body : Expr)
  | elet (bindName : String) (bindUnique : Nat) (bindVal : Expr) (body : Expr)
  | ecase (scrutName : String) (scrutUnique : Nat) (alts : List (String × List (String × Nat) × Expr))
  | eproj (typeName : String) (idx : Nat) (structName : String) (structUnique : Nat)
  | eextern (name : String)
  | eunreach

instance : Nonempty Expr := ⟨.eunreach⟩
instance : Inhabited Expr := ⟨.eunreach⟩

private def mkQName (mod : String) (text : String) (unique : Nat := 0) : QName :=
  { module := mod, text := text, unique := unique }

def tyToJson (ty : Ty) : Json :=
  match ty with
  | .con mod text => .mkObj [("con", .mkObj [("qname", (mkQName mod text).toJson)])]
  | .any => .mkObj [("con", .mkObj [("qname", (mkQName "std" "any").toJson)])]

partial def exprToJson (e : Expr) : Json :=
  match e with
  | .evar name unique =>
    .mkObj [("evar", .mkObj [("text", .str name), ("unique", .num unique)])]
  | .elit_int val =>
    .mkObj [("elit", .mkObj [("int", .num val)])]
  | .elit_str val =>
    .mkObj [("elit", .mkObj [("string", .str val)])]
  | .eapp fnName fnUnique args =>
    .mkObj [("eapp", .mkObj [
      ("fn", .mkObj [("evar", .mkObj [("text", .str fnName), ("unique", .num fnUnique)])]),
      ("args", .arr (args.map exprToJson).toArray)
    ])]
  | .elam params body =>
    .mkObj [("elam", .mkObj [
      ("params", .arr (params.map fun (n, u) =>
        .mkObj [("name", .mkObj [("text", .str n), ("unique", .num u)]),
                ("type", tyToJson .any)]).toArray),
      ("body", exprToJson body)
    ])]
  | .elet name unique val body =>
    .mkObj [("elet", .mkObj [
      ("binds", .arr #[
        .mkObj [("name", .mkObj [("text", .str name), ("unique", .num unique)]),
                ("type", tyToJson .any),
                ("expr", exprToJson val)]]),
      ("body", exprToJson body)
    ])]
  | .ecase sName sUnique alts =>
    let branches := alts.map fun (ctor, params, body) =>
      let pat := if ctor == "_"
        then .mkObj [("pat_wild", .mkObj [])]
        else .mkObj [("pat_con", .mkObj [
          ("name", (mkQName "" ctor).toJson),
          ("args", .arr (params.map fun (n, u) =>
            .mkObj [("name", .mkObj [("text", .str n), ("unique", .num u)])]).toArray)])]
      .mkObj [("pattern", pat), ("body", exprToJson body)]
    .mkObj [("ecase", .mkObj [
      ("scrutinee", .mkObj [("evar", .mkObj [("text", .str sName), ("unique", .num sUnique)])]),
      ("branches", .arr branches.toArray)
    ])]
  | .eproj typeName idx structName structUnique =>
    .mkObj [("eapp", .mkObj [
      ("fn", .mkObj [("evar", .mkObj [("text", .str s!"proj_{typeName}_{idx}"), ("unique", .num 0)])]),
      ("args", .arr #[.mkObj [("evar", .mkObj [("text", .str structName), ("unique", .num structUnique)])]])
    ])]
  | .eextern name =>
    .mkObj [("evar", .mkObj [("text", .str s!"extern:{name}"), ("unique", .num 0)])]
  | .eunreach =>
    .mkObj [("elit", .mkObj [("int", .num 0)])]

/-- A top-level OrganIR definition. -/
structure Definition where
  name : QName
  ty : Ty
  expr : Expr
  sort : String := "fun"
  visibility : String := "public"
  arity : Nat := 0

def Definition.toJson (d : Definition) : Json :=
  let args := List.replicate d.arity
    (.mkObj [("multiplicity", .str "many"), ("type", tyToJson .any)])
  let tyJson := if d.arity == 0 then tyToJson d.ty
    else .mkObj [("fn", .mkObj [
      ("args", .arr args.toArray),
      ("effect", .mkObj [("effects", .arr #[])]),
      ("result", tyToJson .any)
    ])]
  .mkObj [
    ("name", d.name.toJson),
    ("type", tyJson),
    ("expr", exprToJson d.expr),
    ("sort", .str d.sort),
    ("visibility", .str d.visibility)
  ]

/-- A complete OrganIR module. -/
structure Module where
  schemaVersion : String := "1.0.0"
  sourceLang : String := "lean4"
  shimVersion : String := "0.1.0"
  compilerVersion : String := ""
  sourceFile : String := ""
  moduleName : String
  definitions : Array Definition

def Module.toJson (m : Module) : Json :=
  .mkObj [
    ("schema_version", .str m.schemaVersion),
    ("metadata", .mkObj [
      ("source_language", .str m.sourceLang),
      ("compiler_version", .str m.compilerVersion),
      ("source_file", .str m.sourceFile),
      ("shim_version", .str m.shimVersion)
    ]),
    ("module", .mkObj [
      ("name", .str m.moduleName),
      ("definitions", .arr (m.definitions.map Definition.toJson)),
      ("data_types", .arr #[]),
      ("effect_decls", .arr #[])
    ])
  ]

end OrganBank.OrganIR
