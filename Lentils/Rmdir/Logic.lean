/-
Rmdir.Logic — Pure logic for the `rmdir` utility. 0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `rmdir.lean` via the C FFI `rmdir(2)`.
-/

namespace Lentils.Rmdir.Logic

/--
Options controlling `rmdir` behaviour.

| flag                 | field      |
|----------------------|------------|
| `-p`/`--parents`     | `parents`  |
| `-v`/`--verbose`     | `verbose`  |
-/
structure Options where
  parents : Bool := false
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
  | 'p' => some { opts with parents := true }
  | 'v' => some { opts with verbose := true }
  | _ => none

/--
Apply a combined short-flag token (e.g. `-pv`) to `opts`, returning `none`
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
Parse `rmdir` arguments into `(options, paths)`.

Flags are recognised as long as they appear before a `--` separator or a
non-flag operand. A `--` terminates flag parsing and every following token is
treated as a path operand unconditionally. Combined short flags (e.g. `-pv`)
are expanded. Unknown flags terminate flag parsing (silent POSIX-ish
behaviour); operands encountered are collected untouched.
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | "-p" :: rest => go rest { opts with parents := true } operands
    | "--parents" :: rest => go rest { opts with parents := true } operands
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

def optionsOf (p : Options × List String) : Options := p.1
def pathsOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- A plain operand becomes a path operand. -/
theorem parse_plain :
  (parseArgs ["dir"]).2 = ["dir"] := by native_decide

/-- Parsing `-p` sets the `parents` flag. -/
theorem parse_parents :
  (parseArgs ["-p", "dir"]).1.parents = true := by native_decide

/-- Parsing `--parents -v` sets both flags. -/
theorem parse_parents_verbose :
  (parseArgs ["--parents", "-v", "dir"]).1 =
    { parents := true, verbose := true } := by native_decide

/-- A `--` separator forces later tokens to be operands. -/
example : (parseArgs ["--", "-p"]).2 = ["-p"] := by native_decide

end Lentils.Rmdir.Logic
