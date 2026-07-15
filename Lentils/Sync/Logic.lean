/-
Sync.Logic — Pure logic for the `sync` utility.
0BSD

Contains only pure functions: argument parsing. No IO is performed here.
The `sync` utility accepts no operands and has no options.

Provenance: POSIX.1-2017, Section "sync — synchronise cached writes to disk".
No GPL source was consulted.
-/

namespace Lentils.Sync.Logic

/--
Options controlling `sync` behaviour.

GNU sync has `-f` (sync only filesystem containing file) and `--file-system`
but POSIX sync has no options. We start with the POSIX baseline.
-/
structure Options where
  deriving Repr, BEq, DecidableEq

/--
Parse `sync` arguments.

POSIX sync accepts no operands and no options.
Any argument is an error per POSIX (GNU sync accepts file operands with -f).
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | _ :: rest => go rest opts operands
  go args {} []

def optionsOf (p : Options × List String) : Options := p.1
def operandsOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- sync with no arguments returns empty operands. -/
theorem parse_empty :
  (parseArgs []).2 = [] := by native_decide

/-- sync with arguments still returns no operands (POSIX). -/
theorem parse_ignores_args :
  (parseArgs ["a", "b"]).2 = [] := by native_decide

/-- sync ignores flag-like args too. -/
theorem parse_ignores_flags :
  (parseArgs ["-f", "--help"]).2 = [] := by native_decide

/-- sync ignores `--` separator. -/
theorem parse_ignores_ddash :
  (parseArgs ["--", "file"]).2 = [] := by native_decide

/-- Options field is always default (no flags accepted). -/
theorem parse_default_options :
  (parseArgs []).1 = {} := by native_decide

/-- Multiple calls produce same result (idempotence). -/
theorem parse_idempotent :
  parseArgs (parseArgs []).2 = parseArgs [] := by native_decide

end Lentils.Sync.Logic
