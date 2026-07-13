/-
Cp.Logic — Pure logic for the `cp` utility. 0BSD

Contains only pure functions: argument parsing and operand splitting.
No IO is performed here. All filesystem interaction lives in `cp.lean`.
-/

namespace Lentils.Cp.Logic

/--
Options controlling `cp` behaviour.

| flag              | field       |
|-------------------|-------------|
| `-f`/`--force`    | `force`     |
| `-r`/`-R`/`--recursive` | `recursive` |
| `-v`/`--verbose`  | `verbose`   |
-/
structure Options where
  force : Bool := false
  recursive : Bool := false
  verbose : Bool := false
  deriving Repr

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/--
Parse `cp` arguments into `(options, operands)`.

Flags are recognised as long as they appear before a `--` separator or a
non-flag operand. A `--` terminates flag parsing and every following token is
treated as an operand unconditionally. Unknown flags terminate flag parsing
(silent POSIX-ish behaviour); operands encountered are collected untouched.
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | "-f" :: rest => go rest { opts with force := true } operands
    | "--force" :: rest => go rest { opts with force := true } operands
    | "-r" :: rest => go rest { opts with recursive := true } operands
    | "-R" :: rest => go rest { opts with recursive := true } operands
    | "--recursive" :: rest => go rest { opts with recursive := true } operands
    | "-v" :: rest => go rest { opts with verbose := true } operands
    | "--verbose" :: rest => go rest { opts with verbose := true } operands
    | s :: rest =>
      if s.startsWith "-" then
        -- unknown flag: stop parsing flags
        (opts, operands.reverse)
      else
        go rest opts (s :: operands)
  go args {} []

/--
Split a list of operands into `(sources, destination)`.

All operands except the last are sources; the final operand is the
destination. With fewer than two operands there is no destination
(`none`) and no sources.
-/
def splitSourcesDest (operands : List String) : List String × Option String :=
  match operands.reverse with
  | [] => ([], none)
  | dest :: revSrcs => (revSrcs.reverse, some dest)

def optionsOf (p : Options × List String) : Options := p.1
def operandsOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Splitting two operands yields one source and one destination. -/
theorem split_sources_dest_two :
  splitSourcesDest ["a", "b"] = (["a"], some "b") := by native_decide

/-- Splitting a single operand yields no sources but keeps the operand as a destination. -/
theorem split_sources_dest_one :
  splitSourcesDest ["a"] = ([], some "a") := by native_decide

/-- Parsing `-r` sets the `recursive` flag. -/
example : (parseArgs ["-r", "a", "b"]).1.recursive = true := by native_decide

/-- A plain operand becomes a source operand. -/
example : (parseArgs ["a", "b"]).2 = ["a", "b"] := by native_decide

end Lentils.Cp.Logic
