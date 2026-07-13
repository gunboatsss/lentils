/-
Mv.Logic — Pure logic for the `mv` utility. 0BSD

Contains only pure functions: argument parsing and operand splitting.
No IO is performed here. All filesystem interaction lives in `mv.lean`.
-/

namespace Lentils.mv.Logic

/--
Options controlling `mv` behaviour.

| flag            | field         |
|-----------------|---------------|
| `-f`/`--force`  | `force`       |
| `-i`/`--interactive` | `interactive` |
| `-n`/`--no-clobber` | `noClobber` |
| `-v`/`--verbose`| `verbose`     |
-/
structure Options where
  force : Bool := false
  interactive : Bool := false
  noClobber : Bool := false
  verbose : Bool := false
  deriving Repr, BEq

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/--
Parse `mv` arguments into `(options, operands)`.

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
    | "-i" :: rest => go rest { opts with interactive := true } operands
    | "--interactive" :: rest => go rest { opts with interactive := true } operands
    | "-n" :: rest => go rest { opts with noClobber := true } operands
    | "--no-clobber" :: rest => go rest { opts with noClobber := true } operands
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

/--
Compute the target path for a single source when moving into `dest`.

If `destIsDir` is true (multiple sources are being moved), the target is
`dest / basename(source)`; otherwise the target is `dest` itself.
-/
def targetPath (dest : String) (source : String) (destIsDir : Bool) : String :=
  if destIsDir then
    (System.FilePath.mk dest / source).toString
  else
    dest

def optionsOf (p : Options × List String) : Options := p.1
def operandsOf (p : Options × List String) : List String := p.2

theorem targetPath_plain : targetPath "d.txt" "s.txt" false = "d.txt" := by rfl
theorem targetPath_into_dir : targetPath "dir" "s.txt" true = "dir/s.txt" := by native_decide

end Lentils.mv.Logic
