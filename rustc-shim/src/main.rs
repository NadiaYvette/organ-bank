// rustc-organ: Extract MIR from Rust source files and emit OrganIR JSON.
//
// Uses rustc_private to hook into the compiler after borrow checking,
// then serializes optimized MIR as OrganIR JSON via serde_json.
//
// Based on Frankenstein's rustc-shim/src/main.rs.

#![feature(rustc_private)]
#![feature(box_patterns)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;
extern crate rustc_hir;

use rustc_driver::Callbacks;
use rustc_interface::interface::Compiler;
use rustc_middle::ty::{self, TyCtxt};
use rustc_middle::mir::*;
use rustc_span::def_id::DefId;
use serde::Serialize;
use std::env;

// ---------------------------------------------------------------------------
// OrganIR JSON structures
// ---------------------------------------------------------------------------

#[derive(Serialize)]
struct OrganIR {
    schema_version: String,
    metadata: Metadata,
    module: Module,
}

#[derive(Serialize)]
struct Metadata {
    source_language: String,
    shim_version: String,
}

#[derive(Serialize)]
struct Module {
    name: String,
    definitions: Vec<Definition>,
    data_types: Vec<()>,
    effect_decls: Vec<()>,
    exports: Vec<String>,
}

#[derive(Serialize)]
struct Definition {
    name: QName,
    #[serde(rename = "type")]
    ty: Type,
    expr: Expr,
    sort: String,
    visibility: String,
    arity: usize,
}

#[derive(Serialize, Clone)]
struct QName {
    module: String,
    name: Name,
}

#[derive(Serialize, Clone)]
struct Name {
    text: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    unique: Option<u32>,
}

// ---------------------------------------------------------------------------
// OrganIR type representation
// ---------------------------------------------------------------------------

#[derive(Serialize, Clone)]
#[serde(rename_all = "snake_case")]
enum Type {
    Con {
        qname: QName,
    },
    Fn {
        args: Vec<Type>,
        effect: Box<Type>,
        result: Box<Type>,
    },
}

fn type_any() -> Type {
    Type::Con {
        qname: QName {
            module: "std".to_string(),
            name: Name { text: "any".to_string(), unique: None },
        },
    }
}

fn type_int() -> Type {
    Type::Con {
        qname: QName {
            module: "std".to_string(),
            name: Name { text: "int".to_string(), unique: None },
        },
    }
}

fn type_bool() -> Type {
    Type::Con {
        qname: QName {
            module: "std".to_string(),
            name: Name { text: "bool".to_string(), unique: None },
        },
    }
}

fn type_unit() -> Type {
    Type::Con {
        qname: QName {
            module: "std".to_string(),
            name: Name { text: "unit".to_string(), unique: None },
        },
    }
}

fn type_pure() -> Type {
    Type::Con {
        qname: QName {
            module: "std".to_string(),
            name: Name { text: "pure".to_string(), unique: None },
        },
    }
}

// ---------------------------------------------------------------------------
// OrganIR expression representation
// ---------------------------------------------------------------------------

#[derive(Serialize, Clone)]
#[serde(rename_all = "snake_case")]
enum Expr {
    Lit(Lit),
    Var(String),
    App {
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    Tuple(Vec<Expr>),
    List(Vec<Expr>),
}

#[derive(Serialize, Clone)]
#[serde(rename_all = "snake_case")]
enum Lit {
    Int(i64),
    String(String),
}

// ---------------------------------------------------------------------------
// MIR → OrganIR translation
// ---------------------------------------------------------------------------

fn translate_ty(_tcx: TyCtxt<'_>, t: ty::Ty<'_>) -> Type {
    match t.kind() {
        ty::TyKind::Bool => type_bool(),
        ty::TyKind::Int(_) | ty::TyKind::Uint(_) => type_int(),
        ty::TyKind::Tuple(fields) if fields.is_empty() => type_unit(),
        ty::TyKind::Never => type_unit(),
        _ => {
            // Use debug repr as the type name for anything we don't decompose
            Type::Con {
                qname: QName {
                    module: "rust".to_string(),
                    name: Name { text: format!("{:?}", t), unique: None },
                },
            }
        }
    }
}

fn translate_fn_sig(tcx: TyCtxt<'_>, def_id: DefId) -> (Type, usize) {
    // Try to get the function signature
    if tcx.is_closure_like(def_id) {
        return (type_any(), 0);
    }
    let sig = tcx.fn_sig(def_id).skip_binder().skip_binder();
    let args: Vec<Type> = sig.inputs().iter().map(|t| translate_ty(tcx, *t)).collect();
    let ret = translate_ty(tcx, sig.output());
    let arity = args.len();
    let ty = Type::Fn {
        args,
        effect: Box::new(type_pure()),
        result: Box::new(ret),
    };
    (ty, arity)
}

fn translate_place(place: &Place<'_>) -> Expr {
    let local = format!("_{}",  place.local.as_u32());
    if place.projection.is_empty() {
        Expr::Var(local)
    } else {
        // Encode projections as debug string
        Expr::App {
            func: Box::new(Expr::Var("project".to_string())),
            args: vec![
                Expr::Var(local),
                Expr::Lit(Lit::String(format!("{:?}", place.projection))),
            ],
        }
    }
}

fn translate_operand(op: &Operand<'_>) -> Expr {
    match op {
        Operand::Copy(place) | Operand::Move(place) => translate_place(place),
        Operand::Constant(c) => {
            Expr::Lit(Lit::String(format!("{:?}", c)))
        }
        _ => Expr::Lit(Lit::String(format!("{:?}", op))),
    }
}

fn translate_rvalue(rv: &Rvalue<'_>) -> Expr {
    match rv {
        Rvalue::Use(op) => translate_operand(op),
        Rvalue::BinaryOp(binop, box (lhs, rhs)) => {
            Expr::App {
                func: Box::new(Expr::Var(format!("{:?}", binop))),
                args: vec![translate_operand(lhs), translate_operand(rhs)],
            }
        }
        Rvalue::UnaryOp(unop, op) => {
            Expr::App {
                func: Box::new(Expr::Var(format!("{:?}", unop))),
                args: vec![translate_operand(op)],
            }
        }
        Rvalue::Ref(_, bk, place) => {
            Expr::App {
                func: Box::new(Expr::Var(format!("ref_{:?}", bk).to_lowercase())),
                args: vec![translate_place(place)],
            }
        }
        Rvalue::Aggregate(_, ops) => {
            Expr::App {
                func: Box::new(Expr::Var("aggregate".to_string())),
                args: ops.iter().map(|o| translate_operand(o)).collect(),
            }
        }
        Rvalue::Cast(_, op, _) => {
            Expr::App {
                func: Box::new(Expr::Var("cast".to_string())),
                args: vec![translate_operand(op)],
            }
        }
        _ => Expr::Lit(Lit::String(format!("{:?}", rv))),
    }
}

fn translate_statement(stmt: &Statement<'_>) -> Expr {
    match &stmt.kind {
        StatementKind::Assign(box (place, rvalue)) => {
            Expr::App {
                func: Box::new(Expr::Var("assign".to_string())),
                args: vec![translate_place(place), translate_rvalue(rvalue)],
            }
        }
        StatementKind::StorageLive(local) => {
            Expr::App {
                func: Box::new(Expr::Var("storage_live".to_string())),
                args: vec![Expr::Var(format!("_{}", local.as_u32()))],
            }
        }
        StatementKind::StorageDead(local) => {
            Expr::App {
                func: Box::new(Expr::Var("storage_dead".to_string())),
                args: vec![Expr::Var(format!("_{}", local.as_u32()))],
            }
        }
        _ => Expr::Lit(Lit::String(format!("{:?}", stmt.kind))),
    }
}

fn translate_terminator(term: &Terminator<'_>) -> Expr {
    match &term.kind {
        TerminatorKind::Return => Expr::Var("return".to_string()),
        TerminatorKind::Goto { target } => {
            Expr::App {
                func: Box::new(Expr::Var("goto".to_string())),
                args: vec![Expr::Lit(Lit::Int(target.as_u32() as i64))],
            }
        }
        TerminatorKind::SwitchInt { discr, targets } => {
            let mut args = vec![translate_operand(discr)];
            for (val, target) in targets.iter() {
                args.push(Expr::Tuple(vec![
                    Expr::Lit(Lit::Int(val as i64)),
                    Expr::Lit(Lit::Int(target.as_u32() as i64)),
                ]));
            }
            // otherwise target
            args.push(Expr::App {
                func: Box::new(Expr::Var("otherwise".to_string())),
                args: vec![Expr::Lit(Lit::Int(targets.otherwise().as_u32() as i64))],
            });
            Expr::App {
                func: Box::new(Expr::Var("switch_int".to_string())),
                args,
            }
        }
        TerminatorKind::Call { func, args, destination, target, .. } => {
            let mut call_args = vec![translate_operand(func)];
            for arg in args.iter() {
                call_args.push(translate_operand(&arg.node));
            }
            call_args.push(Expr::App {
                func: Box::new(Expr::Var("dest".to_string())),
                args: vec![translate_place(destination)],
            });
            if let Some(tgt) = target {
                call_args.push(Expr::App {
                    func: Box::new(Expr::Var("next_bb".to_string())),
                    args: vec![Expr::Lit(Lit::Int(tgt.as_u32() as i64))],
                });
            }
            Expr::App {
                func: Box::new(Expr::Var("call".to_string())),
                args: call_args,
            }
        }
        TerminatorKind::Drop { place, target, .. } => {
            Expr::App {
                func: Box::new(Expr::Var("drop".to_string())),
                args: vec![
                    translate_place(place),
                    Expr::Lit(Lit::Int(target.as_u32() as i64)),
                ],
            }
        }
        TerminatorKind::Assert { cond, expected, target, .. } => {
            Expr::App {
                func: Box::new(Expr::Var("assert".to_string())),
                args: vec![
                    translate_operand(cond),
                    Expr::Lit(Lit::Int(if *expected { 1 } else { 0 })),
                    Expr::Lit(Lit::Int(target.as_u32() as i64)),
                ],
            }
        }
        TerminatorKind::Unreachable => Expr::Var("unreachable".to_string()),
        _ => Expr::Lit(Lit::String(format!("{:?}", term.kind))),
    }
}

fn translate_basic_block(bb_idx: usize, bb_data: &BasicBlockData<'_>) -> Expr {
    let label = Expr::Lit(Lit::Int(bb_idx as i64));
    let mut stmts: Vec<Expr> = bb_data.statements.iter()
        .map(|s| translate_statement(s))
        .collect();

    if let Some(ref term) = bb_data.terminator {
        stmts.push(translate_terminator(term));
    }

    Expr::Tuple(vec![label, Expr::List(stmts)])
}

fn translate_mir_body(mir: &Body<'_>) -> Expr {
    let blocks: Vec<Expr> = mir.basic_blocks.iter_enumerated()
        .map(|(bb, data)| translate_basic_block(bb.as_u32() as usize, data))
        .collect();

    Expr::App {
        func: Box::new(Expr::Var("mir_body".to_string())),
        args: blocks,
    }
}

// ---------------------------------------------------------------------------
// Top-level emission
// ---------------------------------------------------------------------------

struct OrganCallbacks;

impl Callbacks for OrganCallbacks {
    fn after_analysis<'tcx>(
        &mut self,
        _compiler: &Compiler,
        tcx: TyCtxt<'tcx>,
    ) -> rustc_driver::Compilation {
        emit_organ_ir(tcx);
        rustc_driver::Compilation::Stop
    }
}

fn emit_organ_ir<'tcx>(tcx: TyCtxt<'tcx>) {
    let mut definitions = Vec::new();
    let mut exports = Vec::new();

    for def_id in tcx.hir_body_owners() {
        let def_id = def_id.to_def_id();
        if tcx.is_mir_available(def_id) {
            let mir = tcx.optimized_mir(def_id);
            let name_str = tcx.def_path_str(def_id);

            let (ty, arity) = translate_fn_sig(tcx, def_id);
            let expr = translate_mir_body(mir);

            exports.push(name_str.clone());

            definitions.push(Definition {
                name: QName {
                    module: "rust".to_string(),
                    name: Name {
                        text: name_str,
                        unique: Some(def_id.index.as_u32()),
                    },
                },
                ty,
                expr,
                sort: "fun".to_string(),
                visibility: "public".to_string(),
                arity,
            });
        }
    }

    let ir = OrganIR {
        schema_version: "1.0.0".to_string(),
        metadata: Metadata {
            source_language: "rust".to_string(),
            shim_version: "0.2.0".to_string(),
        },
        module: Module {
            name: "rust".to_string(),
            definitions,
            data_types: vec![],
            effect_decls: vec![],
            exports,
        },
    };

    let json = serde_json::to_string_pretty(&ir).expect("JSON serialization failed");
    println!("{}", json);
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: rustc-organ <file.rs>");
        eprintln!("Extracts MIR and emits OrganIR JSON on stdout.");
        std::process::exit(1);
    }

    let source_file = &args[1];
    let sysroot = std::process::Command::new("rustc")
        .args(["--print", "sysroot"])
        .output()
        .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
        .unwrap_or_default();

    let rustc_args = vec![
        "rustc-organ".to_string(),
        source_file.clone(),
        "--edition=2021".to_string(),
        format!("--sysroot={}", sysroot),
    ];

    let mut callbacks = OrganCallbacks;
    rustc_driver::run_compiler(&rustc_args, &mut callbacks);
}
