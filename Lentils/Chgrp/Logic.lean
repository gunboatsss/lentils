/-
Chgrp.Logic — Pure logic for the `chgrp` utility.
0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `chgrp.lean` via the C FFI `chown(2)`
(called with empty owner to change group only).

Provenance: POSIX.1-2017, Section "chgrp — change group ownership".
No GPL source was consulted.
-/

namespace Lentils.Chgrp.Logic

/--
Options controlling `chgrp` behaviour.
-/
structure Options where
  verbose : Bool := false
  recursive : Bool := false
  deriving Repr, BEq, DecidableEq

/--
Parsed arguments for `chgrp`:

  chgrp [OPTIONS] GROUP FILE...
-/
structure ParsedArgs where
  options : Options
  group : String
  files : List String
  deriving Repr, BEq, DecidableEq

/--
Parse `chgrp` arguments.

  chgrp [OPTIONS] GROUP FILE...
-/
def parseArgs (args : List String) : ParsedArgs :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : ParsedArgs :=
    match remaining with
    | [] => { options := opts, group := "", files := files.reverse }
    | "--" :: rest => { options := opts, group := "", files := files.reverse ++ rest }
    | "-v" :: rest => go rest { opts with verbose := true } files
    | "--verbose" :: rest => go rest { opts with verbose := true } files
    | "-R" :: rest => go rest { opts with recursive := true } files
    | "--recursive" :: rest => go rest { opts with recursive := true } files
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        { options := opts, group := "", files := files.reverse }
      else
        -- First non-flag token is the group, rest are files
        { options := opts, group := s, files := files.reverse ++ rest }
  go args {} []

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Parse basic args. -/
theorem parse_simple :
  (parseArgs ["staff", "file"]).group = "staff" := by native_decide

/-- Parse with verbose flag. -/
theorem parse_verbose :
  (parseArgs ["-v", "staff", "file"]).options.verbose = true := by native_decide

/-- Parse with recursive flag. -/
theorem parse_recursive :
  (parseArgs ["-R", "staff", "file"]).options.recursive = true := by native_decide

/-- Empty args yields empty group and files. -/
theorem parse_empty :
  (parseArgs []).group = "" := by native_decide

/-- Files after group are collected. -/
theorem parse_multiple_files :
  (parseArgs ["staff", "a", "b", "c"]).files = ["a", "b", "c"] := by native_decide

/-- `--` separator: following tokens become files (group stays empty). -/
theorem parse_ddash :
  (parseArgs ["--", "-v"]).group = "" := by native_decide

/-- Unknown flag stops parsing and returns empty group. -/
theorem parse_unknown_flag :
  (parseArgs ["-x", "staff"]).group = "" := by native_decide

end Lentils.Chgrp.Logic
