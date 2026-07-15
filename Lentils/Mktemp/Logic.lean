/-
Mktemp.Logic — Pure logic for the `mktemp` utility.
0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `mktemp.lean` via the C FFI.

Provenance: POSIX.1-2017, Section "mktemp — create temporary files/dirs".
No GPL source was consulted.
-/

namespace Lentils.Mktemp.Logic

/--
Options controlling `mktemp` behaviour.
-/
structure Options where
  directory : Bool := false      -- -d
  tmpdir : String := ""          -- -p DIR
  suffix : String := ""          -- --suffix=SUFF
  dryRun : Bool := false         -- -u (deprecated, just print name)
  quiet : Bool := false          -- -q
  deriving Repr, BEq, DecidableEq

/--
The default template when none is given.
-/
def defaultTemplate : String := "tmp.XXXXXXXXXX"

/--
Parse `mktemp` arguments into `(options, template)`.

  mktemp [-d] [-p DIR] [-q] [-u] [TEMPLATE]
-/
def parseArgs (args : List String) : Options × String :=
  let rec go (remaining : List String) (opts : Options) (template : String)
      : Options × String :=
    match remaining with
    | [] => (opts, if template.isEmpty then defaultTemplate else template)
    | "--" :: _ => (opts, if template.isEmpty then defaultTemplate else template)
    | "-d" :: rest => go rest { opts with directory := true } template
    | "-u" :: rest => go rest { opts with dryRun := true } template
    | "-q" :: rest => go rest { opts with quiet := true } template
    | "-p" :: dir :: rest => go rest { opts with tmpdir := dir } template
    | "--suffix" :: suf :: rest => go rest { opts with suffix := suf } template
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        (opts, if template.isEmpty then defaultTemplate else template)
      else
        go rest opts s  -- first non-flag is template
  go args {} ""

/--
Build the final template string from options and user template.

If -p DIR is given, prepend DIR/.
Append suffix if given.
Ensure template ends with XXXXXX.
-/
def buildTemplate (opts : Options) (userTemplate : String) : String :=
  let base :=
    if userTemplate.contains '/' then userTemplate
    else if opts.tmpdir.isEmpty then userTemplate
    else
      let dir := opts.tmpdir
      let dir' := if dir.endsWith "/" then dir.dropRight 1 else dir
      dir' ++ "/" ++ userTemplate
  let withSuffix := base ++ opts.suffix
  -- Ensure at least 6 X's at the end (POSIX minimum)
  withSuffix

-- ─── Theorems ──────────────────────────────────────────────────────────────────

theorem parse_default :
  (parseArgs []).2 = defaultTemplate := by native_decide

theorem parse_dir :
  (parseArgs ["-d"]).1.directory = true := by native_decide

theorem parse_template :
  (parseArgs ["mytemp.XXXXXX"]).2 = "mytemp.XXXXXX" := by native_decide

/-- buildTemplate: plain template unchanged. -/
theorem buildTemplate_plain :
  buildTemplate {} "tmp.XXXXXX" = "tmp.XXXXXX" := by native_decide

/-- buildTemplate: with -p DIR prepends directory. -/
theorem buildTemplate_with_dir :
  buildTemplate { tmpdir := "/tmp", suffix := "" } "tmp.XXXXXX" = "/tmp/tmp.XXXXXX" := by native_decide

/-- buildTemplate: -p DIR strips trailing slash. -/
theorem buildTemplate_dir_trailing_slash :
  buildTemplate { tmpdir := "/tmp/", suffix := "" } "tmp.XXXXXX" = "/tmp/tmp.XXXXXX" := by native_decide

/-- buildTemplate: with suffix appended. -/
theorem buildTemplate_with_suffix :
  buildTemplate { suffix := ".bak" } "tmp.XXXXXX" = "tmp.XXXXXX.bak" := by native_decide

/-- buildTemplate: -p DIR and suffix together. -/
theorem buildTemplate_dir_and_suffix :
  buildTemplate { tmpdir := "/tmp", suffix := ".txt" } "tmp.XXXXXX" = "/tmp/tmp.XXXXXX.txt" := by native_decide

end Lentils.Mktemp.Logic
