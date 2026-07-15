/-
Unlink.Logic — Pure logic for the `unlink` utility.
0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `unlink.lean` via the C FFI `unlink(2)`.

Provenance: POSIX.1-2017, Section "unlink — remove a directory entry".
No GPL source was consulted.
-/

namespace Lentils.Unlink.Logic

/--
Options controlling `unlink` behaviour.

For now there are no flags (GNU unlink has no options).
-/
structure Options where
  deriving Repr, BEq, DecidableEq

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/--
Parse `unlink` arguments into `(options, files)`.

`unlink` takes one or more file paths and removes each one via `unlink(2)`.
A `--` terminates flag parsing. There are no recognised flags, so any token
starting with `-` (other than `--`) is an unknown flag.
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, files.reverse)
    | "--" :: rest => (opts, files.reverse ++ rest)
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        if s == "--help" then
          -- handled by dispatcher before reaching run
          go rest opts files
        else
          -- unknown flag: GNU unlink treats it as an error
          (opts, files.reverse)
      else
        go rest opts (s :: files)
  go args {} []

def optionsOf (p : Options × List String) : Options := p.1
def filesOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- A single operand becomes a file to unlink. -/
theorem parse_single :
  (parseArgs ["file"]).2 = ["file"] := by native_decide

/-- Multiple operands are all collected. -/
theorem parse_multiple :
  (parseArgs ["a", "b", "c"]).2 = ["a", "b", "c"] := by native_decide

/-- A `--` separator forces later tokens to be operands. -/
theorem parse_ddash :
  (parseArgs ["--", "-f"]).2 = ["-f"] := by native_decide

end Lentils.Unlink.Logic
