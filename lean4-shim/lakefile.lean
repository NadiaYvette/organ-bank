import Lake
open Lake DSL

package «lean4-shim» where
  version := v!"0.1.0"
  leanOptions := #[
    ⟨`autoImplicit, false⟩
  ]

lean_lib OrganBank where
  srcDir := "."
  roots := #[`OrganBank.OrganIR, `OrganBank.LcnfExtract]

@[default_target]
lean_exe «lean4-organ» where
  root := `OrganBank.Main
  supportInterpreter := true
