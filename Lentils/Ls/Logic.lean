/-
Ls.Logic — Verified pure logic for `ls`. 0BSD

Spec-First Methodology:
  1. State types    — LsInput (flags + dirs)
  2. Specification  — showName, sortNames, formatName, formatLongLine: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Correctness    — theorem: impl = spec
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.

`ls` lists directory contents. Pure layer handles argument parsing, name filtering,
sorting, and output formatting. The IO layer provides directory enumeration and metadata.
Dir and Vdir share this logic: Dir.lean and Vdir.lean delegate to Lentils.Ls.run.

Provenance: POSIX.1-2017, Section "ls — list directory contents".
-/

namespace Lentils.Ls.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parsed options for `ls`.
- `all`  : `-a` / `--all` — include entries whose names begin with `.`
- `long` : `-l` / `--long` — use the long (detailed) listing format
- `dirs` : the operands (directories / files to list); defaults to `["."]`
-/
structure Options where
  all : Bool := false
  long : Bool := false
  dirs : List String := ["."]
  deriving Repr, BEq, DecidableEq, Inhabited

/--
Input state for ls.
-/
abbrev LsInput := Options

/-- The default options: no flags, list the current directory. -/
def defaultOptions : Options := {}

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
True when `name` begins with a dot (a "hidden" entry in POSIX).
-/
def isHidden (name : String) : Bool :=
  name.startsWith "."

/--
True for the `.` and `..` directory entries.
-/
def isDotEntry (name : String) : Bool :=
  name = "." || name = ".."

/--
Parse a raw argument list into `Options`.
Flags and operands are processed left-to-right. Combined single-letter
flags such as `-al` / `-la` are recognised. Unknown flags (strings that
start with `-` of length > 1) are ignored so the wrapper can remain
forward-compatible.
-/
def parseArgs (args : List String) : Options :=
  let rec go (args : List String) (opts : Options) : Options :=
    match args with
    | [] => opts
    | "-a" :: rest => go rest { opts with all := true }
    | "--all" :: rest => go rest { opts with all := true }
    | "-l" :: rest => go rest { opts with long := true }
    | "--long" :: rest => go rest { opts with long := true }
    | "-al" :: rest => go rest { all := true, long := true }
    | "-la" :: rest => go rest { all := true, long := true }
    | arg :: rest =>
        if arg.startsWith "-" && arg.length > 1 then
          go rest opts
        else
          go rest { opts with dirs := opts.dirs ++ [arg] }
  let parsed := go args { all := false, long := false, dirs := [] }
  { parsed with dirs := if parsed.dirs.isEmpty then ["."] else parsed.dirs }

/--
Whether a name should be shown, given the options.
Hidden names are suppressed unless `-a` is requested.
-/
def showName (opts : Options) (name : String) : Bool :=
  if opts.all then true else !isHidden name

/--
Sort names in ascending lexicographic (byte) order.
-/
def sortNames (names : List String) : List String :=
  names.mergeSort

/--
Format a plain (short) listing entry: the name followed by a newline.
-/
def formatName (name : String) : String :=
  name ++ "\n"

/--
Pure fields needed to render one long-format line. All values are
primitive so this structure can be produced from the IO layer after
calling `stat` / `System.FilePath.metadata`.
-/
structure LongInfo where
  typeChar : String
  links : UInt64
  size : UInt64
  modifiedSec : Int
  name : String
  deriving Repr, BEq

/--
Format a single long-format line:

    <type> <links> <size> <mtime> <name>

This mirrors the salient fields of POSIX `ls -l` (type, link count,
size, modification time, name). Permissions / owner / group are not
exposed by Lean's `Metadata` and are intentionally omitted here.
-/
def formatLongLine (info : LongInfo) : String :=
  s!"{info.typeChar} {info.links} {info.size} {info.modifiedSec} {info.name}\n"

/--
Specification wrapper: parse args.
-/
def specParse (args : List String) : Options := parseArgs args

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: A leading dot makes a name hidden.
-/
theorem i_isHidden_dot : isHidden "." = true := by native_decide

/--
I2: A non-dot name is not hidden.
-/
theorem i_isHidden_foo (s : String) (h : ¬ s.startsWith ".") : isHidden s = false := by
  unfold isHidden
  simp [h]

/--
I3: Without `-a`, a hidden name is suppressed.
-/
theorem i_showName_hides_hidden (opts : Options) (h : opts.all = false) (name : String) (hname : name.startsWith ".") :
    showName opts name = false := by
  unfold showName
  simp [h, isHidden, hname]

/--
I4: With `-a`, all names are shown.
-/
theorem i_showName_all_shows_all (opts : Options) (h : opts.all = true) (name : String) :
    showName opts name = true := by
  unfold showName
  simp [h]

/--
I5: `isDotEntry` is true only for "." and "..".
-/
theorem i_isDotEntry_dot : isDotEntry "." = true := by native_decide

/--
I6: `isDotEntry` is false for other names.
-/
theorem i_isDotEntry_other (name : String) (h : name ≠ "." ∧ name ≠ "..") : isDotEntry name = false := by
  unfold isDotEntry
  simp [h.1, h.2]

/--
I7: The default operand list is just the current directory.
-/
theorem i_default_dirs : (parseArgs []).dirs = ["."] := by native_decide

/--
I8: Parsing `-a` sets the `all` flag.
-/
theorem i_parse_a_sets_all : (parseArgs ["-a"]).all = true := by native_decide

/--
I9: Parsing `-l` sets the `long` flag.
-/
theorem i_parse_l_sets_long : (parseArgs ["-l"]).long = true := by native_decide

/--
I10: Parsing `-al` sets both flags.
-/
theorem i_parse_al_sets_both : (parseArgs ["-al"]).all = true ∧ (parseArgs ["-al"]).long = true := by
  native_decide

/--
I11: A plain operand becomes a directory operand.
-/
theorem i_parse_dir_arg : (parseArgs ["dir"]).dirs = ["dir"] := by native_decide

/--
I12: Multiple operands are accumulated.
-/
theorem i_parse_two_dirs : (parseArgs ["dir1", "dir2"]).dirs = ["dir1", "dir2"] := by native_decide

/--
I13: Sorting preserves length.
-/
theorem i_sort_length (names : List String) : (sortNames names).length = names.length := by
  simp [sortNames]

/--
I15: formatName appends a trailing newline for concrete names.
-/
theorem i_formatName_foo : formatName "foo" = "foo\n" := rfl

/--
I16: formatLongLine for a concrete LongInfo.
-/
theorem i_formatLongLine_example : formatLongLine { typeChar := "-", links := 1, size := 0, modifiedSec := 0, name := "f" }
    = "- 1 0 0 f\n" := rfl

/--
I21: Empty args always produce the default options (all=false, long=false, dirs=["."]).
-/
theorem i_parse_empty_default : parseArgs [] = defaultOptions := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- isHidden "." = true. -/
example : isHidden "." = true := i_isHidden_dot

/-- Without -a, a hidden name is suppressed. -/
example : showName defaultOptions ".bashrc" = false :=
  i_showName_hides_hidden defaultOptions rfl ".bashrc" (by native_decide : ".bashrc".startsWith ".")

/-- With -a, a hidden name is shown. -/
example : showName { all := true } ".bashrc" = true :=
  i_showName_all_shows_all { all := true } rfl ".bashrc"

/-- Default operand list is just the current directory. -/
example : (parseArgs []).dirs = ["."] := i_default_dirs

/-- Parsing -a sets all flag. -/
example : (parseArgs ["-a"]).all = true := i_parse_a_sets_all

/-- Parsing -l sets long flag. -/
example : (parseArgs ["-l"]).long = true := i_parse_l_sets_long

/-- A plain operand becomes a directory operand. -/
example : (parseArgs ["dir"]).dirs = ["dir"] := i_parse_dir_arg

/-- formatName appends newline. -/
example : formatName "foo" = "foo\n" := rfl

end Lentils.Ls.Logic
