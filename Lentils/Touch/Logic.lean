/-
Touch.Logic — Pure logic for the `touch` utility. 0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `touch.lean`.
-/

namespace Lentils.Touch.Logic

/--
Options controlling `touch` behaviour.

| flag              | field      |
|-------------------|------------|
| `-c`/`--no-create`| `noCreate` |
| `-f`/`--force`    | `force`    |
-/
structure Options where
  noCreate : Bool := false
  force : Bool := false
  deriving Repr

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/--
Parse `touch` arguments into `(options, files)`.

Flags are recognised as long as they appear before a `--` separator or a
non-flag operand. A `--` terminates flag parsing and every following token is
treated as a file operand unconditionally. Unknown flags terminate flag
parsing (silent POSIX-ish behaviour); operands encountered are collected
untouched.
-/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (operands : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, operands.reverse)
    | "--" :: rest => (opts, operands.reverse ++ rest)
    | "-c" :: rest => go rest { opts with noCreate := true } operands
    | "--no-create" :: rest => go rest { opts with noCreate := true } operands
    | "-f" :: rest => go rest { opts with force := true } operands
    | "--force" :: rest => go rest { opts with force := true } operands
    | s :: rest =>
      if s.startsWith "-" then
        -- unknown flag: stop parsing flags
        (opts, operands.reverse)
      else
        go rest opts (s :: operands)
  go args {} []

def optionsOf (p : Options × List String) : Options := p.1
def operandsOf (p : Options × List String) : List String := p.2

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- A plain operand becomes a file operand. -/
theorem parse_plain :
  (parseArgs ["file"]).2 = ["file"] := by native_decide

/-- Parsing `-c` sets the `noCreate` flag. -/
theorem parse_no_create :
  (parseArgs ["-c", "file"]).1.noCreate = true := by native_decide

/-- Parsing `--no-create` sets the `noCreate` flag. -/
theorem parse_no_create_long :
  (parseArgs ["--no-create", "file"]).1.noCreate = true := by native_decide

/-- A `--` separator forces later tokens to be operands. -/
example : (parseArgs ["--", "-c"]).2 = ["-c"] := by native_decide

end Lentils.Touch.Logic
