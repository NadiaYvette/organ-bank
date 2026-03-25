{
  description = "Organ Bank — polyglot compiler IR extraction shims";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Rust overlay for nightly toolchain (needed for rustc_private in rustc-shim)
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, rust-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
          # Uncomment if dotnet-sdk or other packages need it:
          # config.allowUnfree = true;
        };

        # ---------- GHC + Haskell tooling ----------
        # GHC 9.14.x — adjust attribute name to match what nixpkgs ships.
        # To list available versions:
        #   nix eval nixpkgs#haskell.compiler --apply 'x: builtins.filter (n: builtins.substring 0 5 n == "ghc91") (builtins.attrNames x)'
        hc = pkgs.haskell.compiler;
        ghc = hc.ghc9141 or (hc.ghc914 or (hc.ghc9121 or hc.ghc912));

        cabal = pkgs.cabal-install;

        # ---------- Rust nightly ----------
        # rustc-shim requires rustc_private (nightly-only internal crates).
        rustNightly = pkgs.rust-bin.nightly.latest.default.override {
          extensions = [ "rust-src" "rustc-dev" "llvm-tools-preview" ];
        };

        # ---------- Compiler / tool packages ----------
        #
        # All 23 target languages from the smoke test, grouped by
        # availability in nixpkgs-unstable.

        compilers = with pkgs; [
          # --- C / C++ (clang, clang++) ---
          clang

          # --- Fortran (gfortran) ---
          gfortran

          # --- OCaml (ocaml, ocamlopt) ---
          ocaml
          ocamlPackages.findlib

          # --- Erlang (erlc) ---
          erlang

          # --- Guile Scheme ---
          guile

          # --- GNU Prolog (gplc) ---
          gprolog

          # --- .NET SDK (dotnet → F# shim) ---
          dotnet-sdk

          # --- Julia ---
          julia

          # --- Zig ---
          zig

          # --- MLton (Standard ML) ---
          mlton

          # --- GNAT (Ada) ---
          # If `gnat` is missing, try `gnatboot` or build gcc with Ada enabled.
          gnat

          # --- Agda ---
          haskellPackages.Agda

          # --- Idris 2 ---
          idris2

          # --- PureScript (purs) ---
          purescript

          # --- Scala CLI (scala) ---
          # If `scala-cli` is not packaged, try `scala_3` or install via coursier:
          #   cs install scala-cli
          scala-cli
        ];

        # LLVM / MLIR tooling (for Frankenstein integration testing)
        llvmTools = with pkgs; [
          llvmPackages.llvm
          # mlir-opt and mlir-translate — shipped as part of the MLIR package
          # in newer nixpkgs.  If llvmPackages.mlir doesn't exist, the MLIR
          # tools are bundled inside the llvm derivation instead.
        ] ++ (if pkgs.llvmPackages ? mlir then [ pkgs.llvmPackages.mlir ] else []);

        # Haskell tooling
        haskellTools = with pkgs; [
          hlint
          haskell-language-server
        ];

        # ---------- Packages NOT in nixpkgs ----------
        #
        # The following compilers/tools are either not packaged in nixpkgs or
        # not reliably available.  Install them manually and ensure they're on
        # PATH (the smoke test will SKIP anything it can't find).
        #
        # Koka:
        #   curl -sSL https://github.com/koka-lang/koka/releases/latest/download/install.sh | sh
        #
        # Mercury (mmc):
        #   Download rotd from https://dl.mercurylang.org/rotd/
        #   or build from source: https://github.com/Mercury-Language/mercury
        #
        # Swift (swiftc):
        #   Not reliably packaged for Linux in nixpkgs.
        #   Install via https://www.swift.org/install/ or swiftly.
        #
        # Lean 4:
        #   Install via elan (Lean's rustup equivalent):
        #   curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh
        #   The lean4-shim uses `lake build`, which elan manages.
        #
        # Common Lisp (SBCL) — needed by cl-shim:
        #   pkgs.sbcl IS in nixpkgs; uncomment the line below if cl-shim is
        #   added to the smoke tests.
        #   # sbcl

      in {
        devShells.default = pkgs.mkShell {
          name = "organ-bank-dev";

          buildInputs = [
            ghc
            cabal
            rustNightly
          ] ++ compilers
            ++ llvmTools
            ++ haskellTools;

          # System libraries that Haskell packages commonly need
          nativeBuildInputs = with pkgs; [
            pkg-config
            zlib
            gmp
            ncurses
          ];

          shellHook = ''
            echo "=== Organ Bank dev shell ==="
            echo "  GHC:    $(ghc --numeric-version 2>/dev/null || echo 'not found')"
            echo "  cabal:  $(cabal --numeric-version 2>/dev/null || echo 'not found')"
            echo "  rustc:  $(rustc --version 2>/dev/null || echo 'not found')"
            echo ""

            # Check for manually-installed compilers
            missing=""
            for cmd in koka mmc swiftc lean leanc sbcl; do
              if ! command -v "$cmd" &>/dev/null; then
                missing="$missing $cmd"
              fi
            done
            if [ -n "$missing" ]; then
              echo "Not in Nix shell (install manually):$missing"
              echo "  See comments in flake.nix for install instructions."
              echo ""
            fi

            # Point smoke-test.sh at the nix-provided cabal
            export CABAL="$(command -v cabal)"
          '';
        };
      });
}
