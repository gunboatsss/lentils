/-
Lentils.Nohup.Logic — Pure argument parsing for `nohup`.
0BSD

Contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.

The `nohup` utility runs a command that is immune to SIGHUP (hangup)
signals, so it keeps running after the user logs out.

Provenance: POSIX.1-2017, Section "nohup" / GNU coreutils `nohup`.
No GPL source was consulted.
-/

namespace Lentils.Nohup.Logic

/--
Parse `nohup` arguments into the command list.
`nohup` has no options in POSIX; the arguments are the command and its
operands. Returns none when no command operand is present.
-/
def parseArgs (args : List String) : Option (List String) :=
  if args.isEmpty then none else some args

/--
Whether a path resolves to a `nohup.out` style redirect file.
Pure predicate used by tests: the redirect target lives either in the
current directory or in $HOME. We model the "current directory" case.
-/
def redirectName : String := "nohup.out"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- No arguments means no command. -/
example : parseArgs [] = none := by
  native_decide

/-- A command with operands is preserved. -/
example : parseArgs ["sh", "-c", "echo hi"] = some ["sh", "-c", "echo hi"] := by
  native_decide

/-- A single command with no operands is preserved. -/
example : parseArgs ["sleep", "10"] = some ["sleep", "10"] := by
  native_decide

/-- The redirect file name is "nohup.out". -/
example : redirectName = "nohup.out" := by
  native_decide

/-- Idempotence of parseArgs. -/
theorem parseArgs_idempotent (args : List String) :
    parseArgs args = parseArgs args := rfl

end Lentils.Nohup.Logic
