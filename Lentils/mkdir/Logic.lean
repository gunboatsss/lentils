/-
Mkdir.Logic — Pure logic for `mkdir`. 0BSD -/
namespace Lentils.mkdir.Logic

/--
Check if a string looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/--
Parse `mkdir` arguments.

Returns `(parents, paths)` where `parents` is `true` when the `-p`/`--parents`
flag is present, and `paths` is the list of directory operands (in order).
A `--` separator terminates flag parsing and the remaining tokens are treated
as operands unconditionally.
-/
def parseArgs (args : List String) : Bool × List String :=
  let rec go (remaining : List String) (parents : Bool) (paths : List String) : Bool × List String :=
    match remaining with
    | [] => (parents, paths.reverse)
    | "--" :: rest => (parents, (paths.reverse ++ rest))
    | "-p" :: rest => go rest true paths
    | "--parents" :: rest => go rest true paths
    | s :: rest =>
      if s.startsWith "-" then
        (parents, paths.reverse)  -- unknown flag: stop parsing
      else
        go rest parents (s :: paths)
  go args false []

/--
Extract the `parents` flag and directory operands from parsed args.
-/
def parentsOf (parsed : Bool × List String) : Bool :=
  parsed.1

def pathsOf (parsed : Bool × List String) : List String :=
  parsed.2

end Lentils.mkdir.Logic
