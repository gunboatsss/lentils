/-
Link.Logic — Pure logic for the `link` utility.
0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `link.lean` via the C FFI `link(2)`.

Provenance: POSIX.1-2017, Section "link — call the link() function".
No GPL source was consulted.
-/

namespace Lentils.Link.Logic

/--
Options controlling `link` behaviour.

For now there are no flags (POSIX link has no options).
-/
structure Options where
  deriving Repr, BEq, DecidableEq

/--
Parse `link` arguments into `(options, operands)`.

`link` takes exactly two operands: an existing file path (source) and a new
link path (destination). A `--` terminates flag parsing.
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        (opts, operands.reverse)
      else
        go rest opts (s :: operands)
  go args {} []

def optionsOf (p : Options × List String) : Options := p.1
def operandsOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Two operands are collected as source and link name. -/
theorem parse_two_args :
  (parseArgs ["old", "new"]).2 = ["old", "new"] := by native_decide

/-- A `--` separator is handled. -/
theorem parse_ddash :
  (parseArgs ["--", "old", "new"]).2 = ["old", "new"] := by native_decide

/-- Empty args yields no operands. -/
theorem parse_empty :
  (parseArgs []).2 = [] := by native_decide

/-- Single arg is collected. -/
theorem parse_one_arg :
  (parseArgs ["only"]).2 = ["only"] := by native_decide

/-- Three or more args are all collected. -/
theorem parse_three_args :
  (parseArgs ["a", "b", "c"]).2 = ["a", "b", "c"] := by native_decide

/-- A flag-looking operand (starting with dash) stops collection (POSIX). -/
theorem parse_flag_stops :
  (parseArgs ["a", "-x"]).2 = ["a"] := by native_decide

/-- Operands after `--` are collected even if they look like flags. -/
theorem parse_ddash_flag :
  (parseArgs ["--", "-x"]).2 = ["-x"] := by native_decide

end Lentils.Link.Logic
