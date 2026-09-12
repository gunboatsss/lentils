/-
Stat.Logic — Verified pure logic for `stat`. 0BSD

Spec-First Methodology:
  1. State types    — StatInput (options + files)
  2. Specification  — parseArgs, formatStatLine, modeString: the formal "what"
  3. Invariants     — parametric properties over all inputs
  4. Lemmas         — helper theorems used in proofs
  5. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.
-/

set_option maxRecDepth 20000

namespace Lentils.Stat.Logic

-- 1. State Types

structure Options where
  follow : Bool := false
  filesys : Bool := false
  terse : Bool := false
  format : Option String := none
  files : List String := []
  deriving Repr, BEq, DecidableEq, Inhabited

structure StatInput where
  opts : Options
  deriving Inhabited, BEq

def defaultInput : StatInput := { opts := {} }

-- 2. Specification (= Implementation)

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
        { opts with files := opts.files ++ s :: rest }
      else if s.startsWith "-" && s.length > 1 then
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

def modeString (mode : UInt64) : String :=
  let dig i := (mode >>> i &&& 1) == 1
  let r i := if dig i then "r" else "-"
  let w i := if dig (i-1) then "w" else "-"
  let x i :=
    if dig (i-2) then
      if i == 8 && ((mode >>> 11) &&& 1) == 1 then "s"
      else if i == 5 && ((mode >>> 10) &&& 1) == 1 then "s"
      else if i == 2 && ((mode >>> 9) &&& 1) == 1 then "t"
      else "x"
    else
      if i == 8 && ((mode >>> 11) &&& 1) == 1 then "S"
      else if i == 5 && ((mode >>> 10) &&& 1) == 1 then "S"
      else if i == 2 && ((mode >>> 9) &&& 1) == 1 then "T"
      else "-"
  r 8 ++ w 8 ++ x 8 ++ r 5 ++ w 5 ++ x 5 ++ r 2 ++ w 2 ++ x 2

def formatStatLine (mode size nlink uid gid blocks blksize : UInt64) (name : String) : String :=
  let kind := fileType mode
  s!"  File: {name}\n" ++
  s!"  Size: {size}    \tBlocks: {blocks}    \tIO Block: {blksize}   {kind}\n" ++
  s!"  Mode: {modeString mode} ({mode})\n" ++
  s!"  Links: {nlink}    \tUID: {uid}    \tGID: {gid}\n"

def formatTerse (mode size nlink uid gid blocks blksize dev ino _rdev : UInt64) (name : String) : String :=
  s!"{name} {ino} {mode} {nlink} {uid} {gid} {dev} {size} {blksize} {blocks}\n"

def formatFsLine (bsize frsize blocks bfree bavail files ffree favail _namemax : UInt64) (name : String) : String :=
  s!"  File: \"{name}\"\n" ++
  s!"  Block size: {bsize}    \tFundamental block size: {frsize}\n" ++
  s!"  Blocks: Total: {blocks}    \tFree: {bfree}    \tAvailable: {bavail}\n" ++
  s!"  Inodes: Total: {files}    \tFree: {ffree}    \tAvailable: {favail}\n"

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

def specParse (args : List String) : Options := parseArgs args

-- 3. Invariants

theorem i_modeString_644 : modeString 0o100644 = "rw-r--r--" := by native_decide

theorem i_modeString_755 : modeString 0o040755 = "rwxr-xr-x" := by native_decide

theorem i_fileType_regular : fileType 0x8000 = "regular file" := by native_decide

theorem i_fileType_directory : fileType 0x4000 = "directory" := by native_decide

theorem i_fileType_symlink : fileType 0xA000 = "symbolic link" := by native_decide

theorem i_formatCustom_s : formatCustom "%s" 0 42 0 0 0 0 0 0 0 "" = "42" := rfl

theorem i_formatCustom_n : formatCustom "%n" 0 0 0 0 0 0 0 0 0 "foo" = "foo" := rfl

theorem i_formatCustom_percent : formatCustom "%%%s" 0 42 0 0 0 0 0 0 0 "" = "%42" := rfl

theorem i_formatCustom_unknown : formatCustom "%q" 0 0 0 0 0 0 0 0 0 "" = "%q" := rfl

theorem i_parse_follow : (parseArgs ["-L", "file"]).follow = true := by native_decide

theorem i_parse_filesys : (parseArgs ["-f", "file"]).filesys = true := by native_decide

theorem i_parse_terse : (parseArgs ["-t", "file"]).terse = true := by native_decide

theorem i_parse_format : (parseArgs ["-c", "%s", "file"]).format = some "%s" := by native_decide

theorem i_parse_combined : (parseArgs ["-Lt", "file"]).follow = true ∧ (parseArgs ["-Lt", "file"]).terse = true := by
  native_decide

theorem i_parse_double_dash : (parseArgs ["--", "-L"]).files = ["-L"] := by native_decide

/--
An empty custom format produces empty output regardless of the stat values.
Parametric over all field values and the file name.
-/
theorem i_formatCustom_empty (mode size nlink uid gid blocks blksize dev ino : UInt64)
    (name : String) :
    formatCustom "" mode size nlink uid gid blocks blksize dev ino name = "" := rfl

theorem i_modeString_zero : modeString 0 = "---------" := by native_decide

theorem i_modeString_all_bits : modeString 0o7777 = "rwsrwsrwt" := by native_decide

-- 5. Concrete Corollaries

example : modeString 0o100644 = "rw-r--r--" := i_modeString_644

example : modeString 0o040755 = "rwxr-xr-x" := i_modeString_755

example : formatCustom "%s" 0 42 0 0 0 0 0 0 0 "" = "42" := i_formatCustom_s

example : formatCustom "%n" 0 0 0 0 0 0 0 0 0 "foo" = "foo" := i_formatCustom_n

example : formatCustom "%%%s" 0 42 0 0 0 0 0 0 0 "" = "%42" := i_formatCustom_percent

end Lentils.Stat.Logic
