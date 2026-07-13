/-
Df.Logic — Pure logic for the `df` utility.
0BSD

Contains only pure functions: argument parsing, size formatting, and display.
No IO is performed here. All filesystem interaction lives in `df.lean` via the
C FFI statvfs wrapper.
-/

set_option maxRecDepth 20000

namespace Lentils.Df.Logic

/--
Parsed options for `df`.
- `blockSize` : `--block-size=SIZE` or `-B SIZE` — scale sizes by SIZE
- `human` : `-h` / `--human-readable` — human-readable sizes
- `inodes` : `-i` / `--inodes` — show inode info
- `type` : `-t` / `--type=TYPE` — limit to filesystem type
- `all` : `-a` / `--all` — include dummy filesystems
- `files` : the operands (filesystem paths)
-/
structure Options where
  human : Bool := false
  inodes : Bool := false
  all : Bool := false
  blockSize : UInt64 := 1024
  files : List String := []
  deriving Repr

/--
Parse a size string into a UInt64 block size.
Supports K, M, G suffixes. Defaults to 1024.
-/
def parseSize (s : String) : UInt64 :=
  if s.isEmpty then 1024 else
    let chars := s.toList
    let lastChar := chars.getLast? |>.getD ' '
    let numStr := String.ofList (chars.dropLast)
    let base := match numStr.toNat? with | some n => n.toUInt64 | none => 1024
    match lastChar with
    | 'K' | 'k' => base * 1024
    | 'M' | 'm' => base * 1024 * 1024
    | 'G' | 'g' => base * 1024 * 1024 * 1024
    | _ => s.toNat?.map (fun n : Nat => n.toUInt64) |>.getD 1024

/--
Parse `df` arguments into `Options`.
-/
def parseArgs (args : List String) : Options :=
  let rec go (remaining : List String) (opts : Options) : Options :=
    match remaining with
    | [] => opts
    | "--" :: rest => { opts with files := opts.files ++ rest }
    | "-h" :: rest => go rest { opts with human := true }
    | "--human-readable" :: rest => go rest { opts with human := true }
    | "-i" :: rest => go rest { opts with inodes := true }
    | "--inodes" :: rest => go rest { opts with inodes := true }
    | "-a" :: rest => go rest { opts with all := true }
    | "--all" :: rest => go rest { opts with all := true }
    | "-B" :: s :: rest => go rest { opts with blockSize := parseSize s }
    | "--block-size" :: s :: rest => go rest { opts with blockSize := parseSize s }
    | s :: rest =>
      if s.startsWith "--" then
        { opts with files := opts.files ++ s :: rest }
      else if s.startsWith "-" && s.length > 1 then
        -- Combined short flags, e.g. -hi = -h -i
        let flags := (s.drop 1).toString.toList
        let rec handleFlags (fs : List Char) (curOpts : Options) : Options :=
          match fs with
          | [] => go rest curOpts
          | 'h' :: rfs => handleFlags rfs { curOpts with human := true }
          | 'i' :: rfs => handleFlags rfs { curOpts with inodes := true }
          | 'a' :: rfs => handleFlags rfs { curOpts with all := true }
          | _ :: rfs => handleFlags rfs curOpts
        handleFlags flags opts
      else
        go rest { opts with files := opts.files ++ [s] }
  go args {}

/--
Format size in human-readable form (K, M, G suffixes).
-/
def humanSize (bytes : UInt64) : String :=
  let val := bytes / 1024
  if val < 10 then s!"{val}" else
  if val < 10*1024 then s!"{val / 1024}K" else
  if val < 10*1024*1024 then s!"{val / (1024*1024)}M" else
  s!"{val / (1024*1024*1024)}G"

/--
Format a single df output line.
-/
def formatLine (fs : String) (totalBlocks freeBlocks availBlocks usePct : UInt64) (mounted : String) : String :=
  s!"{fs}  {totalBlocks}  {freeBlocks}  {availBlocks}  {usePct}%  {mounted}\n"

/--
Header line for df output.
-/
def headerLine : String :=
  "Filesystem     1K-blocks     Used    Available  Use%  Mounted on\n"

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Default block size is 1024. -/
example : parseSize "" = 1024 := by
  native_decide

/-- Parse K suffix. -/
example : parseSize "1K" = 1024 := by
  native_decide
