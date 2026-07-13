/-
Lentils.Ls.Logic — Pure specification for the `ls` utility.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `ls` utility lists the contents of directories. This pure layer
handles argument parsing and all output formatting. The actual
filesystem enumeration (via `System.FilePath.readDir`) and metadata
lookup (via `stat`) live in the IO wrapper.

Provenance: POSIX.1-2017, Section "ls — list directory contents".
No GPL source was consulted.
-/

namespace Lentils.Ls.Logic

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
  deriving Repr

/-- The default options: no flags, list the current directory. -/
def defaultOptions : Options := {}

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
  deriving Repr

/--
Format a single long-format line:

    <type> <links> <size> <mtime> <name>

This mirrors the salient fields of POSIX `ls -l` (type, link count,
size, modification time, name). Permissions / owner / group are not
exposed by Lean's `Metadata` and are intentionally omitted here.
-/
def formatLongLine (info : LongInfo) : String :=
  s!"{info.typeChar} {info.links} {info.size} {info.modifiedSec} {info.name}\n"

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- A leading dot makes a name hidden. -/
theorem isHidden_dot : isHidden "." = true := by native_decide

/-- A non-dot name is not hidden. -/
theorem isHidden_foo : isHidden "foo" = false := by native_decide

/-- Without `-a`, a hidden name is suppressed. -/
theorem showName_hides_hidden :
  showName defaultOptions ".bashrc" = false := by native_decide

/-- With `-a`, a hidden name is shown. -/
theorem showName_all_shows_hidden :
  showName { all := true } ".bashrc" = true := by native_decide

/-- The default operand list is just the current directory. -/
example : (parseArgs []).dirs = ["."] := by native_decide

/-- Parsing `-a` sets the `all` flag. -/
example : (parseArgs ["-a"]).all = true := by native_decide

/-- Parsing `-l` sets the `long` flag. -/
example : (parseArgs ["-l"]).long = true := by native_decide

/-- A plain operand becomes a directory operand. -/
example : (parseArgs ["dir"]).dirs = ["dir"] := by native_decide

/-- Sorting a concrete list is stable and deterministic. -/
example : sortNames ["b", "a", "c"] = ["a", "b", "c"] := by
  native_decide

/-- formatName appends a single trailing newline. -/
example : formatName "foo" = "foo\n" := rfl
