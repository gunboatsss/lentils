/-
Ln.Logic — Pure logic for the `ln` utility. 0BSD

Contains only pure functions: argument parsing and operand splitting. No IO
is performed here. All filesystem interaction lives in `ln.lean` via the C
FFI calls `link(2)` and `symlink(2)`.
-/

namespace Lentils.Ln.Logic

/--
Options controlling `ln` behaviour.

| flag              | field      |
|-------------------|------------|
| `-s`/`--symbolic` | `symbolic` |
| `-f`/`--force`    | `force`    |
| `-v`/`--verbose`  | `verbose`  |
-/
structure Options where
  symbolic : Bool := false
  force : Bool := false
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/-- Apply a single recognised short flag character to `opts`. -/
def applyShort (c : Char) (opts : Options) : Option Options :=
  match c with
  | 's' => some { opts with symbolic := true }
  | 'f' => some { opts with force := true }
  | 'v' => some { opts with verbose := true }
  | _ => none

/--
Apply a combined short-flag token (e.g. `-sf`) to `opts`, returning `none`
if any character is an unrecognised short flag.
-/
def applyShortFlags (s : String) (opts : Options) : Option Options :=
  let chars := s.toList.tail
  let rec go (cs : List Char) (o : Options) : Option Options :=
    match cs with
    | [] => some o
    | c :: r =>
      match applyShort c o with
      | none => none
      | some o2 => go r o2
  go chars opts

/--
Parse `ln` arguments into `(options, operands)`.

Flags are recognised as long as they appear before a `--` separator or a
non-flag operand. A `--` terminates flag parsing and every following token is
treated as an operand unconditionally. Combined short flags (e.g. `-sf`) are
expanded. Unknown flags terminate flag parsing (silent POSIX-ish behaviour).
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | "-s" :: rest => go rest { opts with symbolic := true } operands
    | "--symbolic" :: rest => go rest { opts with symbolic := true } operands
    | "-f" :: rest => go rest { opts with force := true } operands
    | "--force" :: rest => go rest { opts with force := true } operands
    | "-v" :: rest => go rest { opts with verbose := true } operands
    | "--verbose" :: rest => go rest { opts with verbose := true } operands
    | s :: rest =>
      if s == "-" then
        go rest opts (s :: operands)
      else if s.startsWith "-" then
        if s.length >= 3 then
          match applyShortFlags s opts with
          | none => (opts, operands.reverse)
          | some newOpts => go rest newOpts operands
        else
          (opts, operands.reverse)
      else
        go rest opts (s :: operands)
  go args {} []

/--
Split a list of operands into `(sources, linkName)`.

All operands except the last are sources; the final operand is the link name
(or target directory). With fewer than two operands `linkName` is `none` and
the single source is linked into the current directory under its base name.
-/
def splitSourcesLink (operands : List String) : List String × Option String :=
  match operands.reverse with
  | [] => ([], none)
  | [only] => ([only], none)
  | link :: revSrcs => (revSrcs.reverse, some link)

def optionsOf (p : Options × List String) : Options := p.1
def operandsOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- A single plain operand yields no link name. -/
theorem split_single :
  splitSourcesLink ["file"] = (["file"], none) := by native_decide

/-- Two operands split into a source and a link name. -/
theorem split_pair :
  splitSourcesLink ["a", "b"] = (["a"], some "b") := by native_decide

/-- Parsing `-s` sets the `symbolic` flag. -/
theorem parse_symbolic :
  (parseArgs ["-s", "a", "b"]).1.symbolic = true := by native_decide

/-- Parsing `-sf` sets both flags. -/
theorem parse_symbolic_force :
  (parseArgs ["-sf", "a", "b"]).1 =
    { symbolic := true, force := true, verbose := false } := by native_decide

end Lentils.Ln.Logic
