// rustc-organ: Extract MIR from Rust source files and emit OrganIR JSON.
//
// Uses rustc_private to hook into the compiler after borrow checking,
// then serializes optimized MIR as OrganIR JSON.
//
// Based on Frankenstein's rustc-shim/src/main.rs.

#![feature(rustc_private)]

extern crate rustc_driver;
extern crate rustc_interface;
extern crate rustc_middle;
extern crate rustc_span;
extern crate rustc_hir;

use rustc_driver::Callbacks;
use rustc_interface::interface::Compiler;
use rustc_middle::ty::TyCtxt;
use rustc_middle::mir::*;
use rustc_span::def_id::DefId;
use std::env;
use std::io::Write;

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
    let stdout = std::io::stdout();
    let mut out = stdout.lock();

    writeln!(out, "{{").unwrap();
    writeln!(out, "  \"schema_version\": \"1.0.0\",").unwrap();
    writeln!(out, "  \"metadata\": {{").unwrap();
    writeln!(out, "    \"source_language\": \"rust\",").unwrap();
    writeln!(out, "    \"shim_version\": \"0.1.0\"").unwrap();
    writeln!(out, "  }},").unwrap();
    writeln!(out, "  \"module\": {{").unwrap();
    writeln!(out, "    \"name\": \"rust\",").unwrap();
    writeln!(out, "    \"definitions\": [").unwrap();

    let mut first = true;
    for def_id in tcx.hir_body_owners() {
        let def_id = def_id.to_def_id();
        if tcx.is_mir_available(def_id) {
            let mir = tcx.optimized_mir(def_id);
            if !first { writeln!(out, ",").unwrap(); }
            first = false;
            emit_mir_def(&mut out, tcx, def_id, mir);
        }
    }

    writeln!(out, "\n    ],").unwrap();
    writeln!(out, "    \"data_types\": [],").unwrap();
    writeln!(out, "    \"effect_decls\": []").unwrap();
    writeln!(out, "  }}").unwrap();
    writeln!(out, "}}").unwrap();
}

fn emit_mir_def<'tcx>(
    out: &mut impl Write,
    tcx: TyCtxt<'tcx>,
    def_id: DefId,
    mir: &Body<'tcx>,
) {
    let name = tcx.def_path_str(def_id);

    write!(out, "      {{").unwrap();
    write!(out, "\"name\": {{\"module\": \"rust\", \"name\": {{\"text\": \"{}\", \"unique\": {}}}}}", name, def_id.index.as_u32()).unwrap();
    write!(out, ", \"type\": {{\"con\": {{\"qname\": {{\"module\": \"std\", \"name\": {{\"text\": \"any\"}}}}}}}}").unwrap();

    // Emit basic block structure as metadata for now
    // Full CFG-to-expression conversion would happen here
    write!(out, ", \"expr\": {{\"elit\": {{\"int\": 0}}}}").unwrap();
    write!(out, ", \"sort\": \"fun\"").unwrap();
    write!(out, ", \"visibility\": \"public\"").unwrap();

    // Include MIR block count as a comment-like field
    write!(out, ", \"_mir_blocks\": {}", mir.basic_blocks.len()).unwrap();
    write!(out, ", \"_mir_locals\": {}", mir.local_decls.len()).unwrap();

    write!(out, "}}").unwrap();
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
