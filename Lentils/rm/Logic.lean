/-
Rm.Logic — Pure logic for the `rm` utility. 0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `rm.lean` via C FFI (unlink/rmdir).
-/

namespace Lentils.rm.Logic

/--
Options controlling `rm` behaviour.

| flag                | field      |
|---------------------|------------|
| `-f`/`--force`      | `force`    |
| `-i`/`--interactive`| `interactive` |
| `-r`/`-R`/`--recursive` | `recursive` |
| `-d`/`--dir`        | `dir`      |
| `-v`/`--verbose`    | `verbose`  |
-/
structure Options where
  force : Bool := false
  interactive : Bool := false
  recursive : Bool := false
  dir : Bool := false
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
  | 'f' => some { opts with force := true }
  | 'i' => some { opts with interactive := true }
  | 'r' => some { opts with recursive := true }
  | 'R' => some { opts with recursive := true }
  | 'd' => some { opts with dir := true }
  | 'v' => some { opts with verbose := true }
  | _ => none

/--
Apply a combined short-flag token (e.g. `-rf`) to `opts`, returning `none`
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
Parse `rm` arguments into `(options, files)`.

Flags are recognised as long as they appear before a `--` separator or a
non-flag operand. A `--` terminates flag parsing and every following token is
treated as a file operand unconditionally. Combined short flags (e.g. `-rf`)
are expanded. Unknown flags terminate flag parsing (silent POSIX-ish
behaviour); operands encountered are collected untouched.
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | "-f" :: rest => go rest { opts with force := true } operands
    | "--force" :: rest => go rest { opts with force := true } operands
    | "-i" :: rest => go rest { opts with interactive := true } operands
    | "--interactive" :: rest => go rest { opts with interactive := true } operands
    | "-r" :: rest => go rest { opts with recursive := true } operands
    | "-R" :: rest => go rest { opts with recursive := true } operands
    | "--recursive" :: rest => go rest { opts with recursive := true } operands
    | "-d" :: rest => go rest { opts with dir := true } operands
    | "--dir" :: rest => go rest { opts with dir := true } operands
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
def filesOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- A plain operand becomes a file operand. -/
theorem parse_plain :
  (parseArgs ["file"]).2 = ["file"] := by native_decide

/-- Parsing `-r` sets the `recursive` flag. -/
theorem parse_recursive :
  (parseArgs ["-r", "file"]).1.recursive = true := by native_decide

/-- Parsing `--recursive -f` sets both flags. -/
theorem parse_recursive_force :
  (parseArgs ["--recursive", "-f", "file"]).1 =
    { force := true, recursive := true, interactive := false, dir := false, verbose := false } := by native_decide

/-- A `--` separator forces later tokens to be operands. -/
example : (parseArgs ["--", "-r"]).2 = ["-r"] := by native_decide

/-- `-v` sets the `verbose` flag. -/
theorem parse_verbose :
  (parseArgs ["-v", "a"]).1.verbose = true := by native_decide

end Lentils.rm.Logic
