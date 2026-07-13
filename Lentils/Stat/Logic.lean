/-
Stat.Logic — Pure logic for the `stat` utility.
0BSD

Contains only pure functions: argument parsing, mode-to-string conversion,
and formatting. No IO is performed here. All filesystem interaction lives in
`stat.lean` via the C FFI wrappers in Native.
-/

set_option maxRecDepth 20000

namespace Lentils.Stat.Logic

/--
Parsed options for `stat`.
- `follow` : `-L` / `--dereference` — follow symlinks
- `filesys` : `-f` / `--file-system` — display file system status
- `terse` : `-t` / `--terse` — terse format
- `format` : `-c` / `--format` — custom format string
- `files` : the operands (files to stat)
-/
structure Options where
  follow : Bool := false
  filesys : Bool := false
  terse : Bool := false
  format : Option String := none
  files : List String := []
  deriving Repr

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/--
Parse `stat` arguments into `Options`.
-/
def parseArgs (args : List String) : Options :=
  let rec go (remaining : List String) (opts : Options) : Options :=
    match remaining with
    | [] => opts
    | "--" :: rest => { opts with files := opts.files ++ rest }
    | "-L" :: rest => go rest { opts with follow := true }
    | "--dereference" :: rest => go rest { opts with follow := true }
    | "-f" :: rest => go rest { opts with filesys := true }
    | "--file-system" :: rest => go rest { opts with filesys := true }
    | "-t" :: rest => go rest { opts with terse := true }
    | "--terse" :: rest => go rest { opts with terse := true }
    | "-c" :: fmt :: rest => go rest { opts with format := some fmt }
    | "--format" :: fmt :: rest => go rest { opts with format := some fmt }
    | s :: rest =>
      if s.startsWith "--" then
        -- unknown long flag: treat as file
        { opts with files := opts.files ++ s :: rest }
      else if s.startsWith "-" && s.length > 1 then
        -- Combined short flags, e.g. -Lt = -L -t
        let flags := (s.drop 1).toString.toList
        let rec handleFlags (fs : List Char) (curOpts : Options) : Options :=
          match fs with
          | [] => go rest curOpts
          | 'L' :: rfs => handleFlags rfs { curOpts with follow := true }
          | 'f' :: rfs => handleFlags rfs { curOpts with filesys := true }
          | 't' :: rfs => handleFlags rfs { curOpts with terse := true }
          | _ :: rfs => handleFlags rfs curOpts
        handleFlags flags opts
      else
        go rest { opts with files := opts.files ++ [s] }
  go args {}

/--
File type classification from st_mode bits.
-/
def fileType (mode : UInt64) : String :=
  let t := mode >>> 12 &&& 0xF
  match t with
  | 0x8 => "regular file"
  | 0x4 => "directory"
  | 0xA => "symbolic link"
  | 0x2 => "character device"
  | 0x6 => "block device"
  | 0x1 => "FIFO"
  | 0xC => "socket"
  | _   => "unknown"

/--
Convert mode bits to a permission string like "rwxr-xr-x".
-/
def modeString (mode : UInt64) : String :=
  let dig i := (mode >>> i &&& 1) == 1
  let r i := if dig i then "r" else "-"
  let w i := if dig (i-1) then "w" else "-"
  let x i :=
    if dig (i-2) then
      if i == 8 && ((mode >>> 11) &&& 1) == 1 then "s"  -- setuid
      else if i == 5 && ((mode >>> 10) &&& 1) == 1 then "s"  -- setgid
      else if i == 2 && ((mode >>> 9) &&& 1) == 1 then "t"   -- sticky
      else "x"
    else
      if i == 8 && ((mode >>> 11) &&& 1) == 1 then "S"
      else if i == 5 && ((mode >>> 10) &&& 1) == 1 then "S"
      else if i == 2 && ((mode >>> 9) &&& 1) == 1 then "T"
      else "-"
  r 8 ++ w 8 ++ x 8 ++ r 5 ++ w 5 ++ x 5 ++ r 2 ++ w 2 ++ x 2

/--
Format a single stat value for display.
-/
def formatStatLine (mode size nlink uid gid blocks blksize : UInt64) (name : String) : String :=
  let kind := fileType mode
  s!"  File: {name}\n" ++
  s!"  Size: {size}    \tBlocks: {blocks}    \tIO Block: {blksize}   {kind}\n" ++
  s!"  Mode: {modeString mode} ({mode})\n" ++
  s!"  Links: {nlink}    \tUID: {uid}    \tGID: {gid}\n"

/--
Terse format: single line.
-/
def formatTerse (mode size nlink uid gid blocks blksize dev ino _rdev : UInt64) (name : String) : String :=
  s!"{name} {ino} {mode} {nlink} {uid} {gid} {dev} {size} {blksize} {blocks}\n"

/--
Format file system info.
-/
def formatFsLine (bsize frsize blocks bfree bavail files ffree favail _namemax : UInt64) (name : String) : String :=
  s!"  File: \"{name}\"\n" ++
  s!"  Block size: {bsize}    \tFundamental block size: {frsize}\n" ++
  s!"  Blocks: Total: {blocks}    \tFree: {bfree}    \tAvailable: {bavail}\n" ++
  s!"  Inodes: Total: {files}    \tFree: {ffree}    \tAvailable: {favail}\n"

/--
Apply a format string using stat values.
Supports %s (size), %f (mode hex), %n (name), %b (blocks),
%u (uid), %g (gid), %h (nlink), %o (blksize), %d (dev), %i (ino).
-/
def formatCustom (fmt : String) (mode size nlink uid gid blocks blksize dev ino : UInt64) (name : String) : String :=
  let rec go (chars : List Char) (acc : String) : String :=
    match chars with
    | [] => acc
    | '%' :: rest =>
      match rest with
      | [] => acc ++ "%"
      | c :: rest2 =>
        let subst : String :=
          match c with
          | 's' => toString size
          | 'f' => toString mode
          | 'n' => name
          | 'b' => toString blocks
          | 'u' => toString uid
          | 'g' => toString gid
          | 'h' => toString nlink
          | 'o' => toString blksize
          | 'd' => toString dev
          | 'i' => toString ino
          | '%' => "%"
          | _ => "%" ++ String.singleton c
        go rest2 (acc ++ subst)
    | c :: rest => go rest (acc.push c)
  go (fmt.toList) ""

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Mode string for regular file with 644. -/
theorem modeString644 : modeString 0o100644 = "rw-r--r--" := by native_decide

/-- Mode string for directory with 755. -/
theorem modeString755 : modeString 0o040755 = "rwxr-xr-x" := by native_decide

/-- Format custom with %s. -/
example : formatCustom "%s" 0 42 0 0 0 0 0 0 0 "" = "42" := rfl

/-- Format custom with %n. -/
example : formatCustom "%n" 0 0 0 0 0 0 0 0 0 "foo" = "foo" := rfl

/-- Format custom with %%%. -/
example : formatCustom "%%%s" 0 42 0 0 0 0 0 0 0 "" = "%42" := rfl
