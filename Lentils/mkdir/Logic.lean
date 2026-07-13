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

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Parsing empty args yields no parents flag. -/
theorem parseArgs_empty :
  (parseArgs []).1 = false := by native_decide

/-- Parsing `-p` sets the parents flag. -/
theorem parseArgs_p_flag :
  (parseArgs ["-p", "dir"]).1 = true := by native_decide

/-- Parsing `--parents` sets the parents flag. -/
theorem parseArgs_parents_flag :
  (parseArgs ["--parents", "dir"]).1 = true := by native_decide

/-- A plain operand becomes a directory operand. -/
theorem parseArgs_operand :
  (parseArgs ["somedir"]).2 = ["somedir"] := by native_decide

/-- A `--` separator forces later tokens to be operands. -/
theorem parseArgs_dashdash :
  (parseArgs ["--", "-p"]).2 = ["-p"] := by native_decide

/-- parentsOf extracts the bool from the parsed pair. -/
theorem parentsOf_from_parse :
  parentsOf (parseArgs ["-p"]) = true := by native_decide

/-- pathsOf extracts the directory list from the parsed pair. -/
theorem pathsOf_from_parse :
  pathsOf (parseArgs ["dir1", "dir2"]) = ["dir1", "dir2"] := by native_decide

end Lentils.mkdir.Logic
