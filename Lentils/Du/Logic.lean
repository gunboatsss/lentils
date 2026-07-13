/-
Du.Logic — Pure logic for the `du` utility.
0BSD

Contains only pure functions: argument parsing, size formatting.
No IO is performed here. All filesystem interaction lives in `du.lean`.
-/

set_option maxRecDepth 20000

namespace Lentils.Du.Logic

/--
Parsed options for `du`.
- `all` : `-a` / `--all` — count files (not just directories)
- `summarize` : `-s` / `--summarize` — only total for each argument
- `human` : `-h` / `--human-readable` — human-readable sizes
- `blockSize` : `-B SIZE` / `--block-size=SIZE` — block size scaling
- `maxDepth` : `--max-depth=N` — max directory depth (-1 = unlimited)
- `files` : the operands (paths to analyze)
-/
structure Options where
  all : Bool := false
  summarize : Bool := false
  human : Bool := false
  blockSize : UInt64 := 1024
  maxDepth : Int := -1
  files : List String := []
  deriving Repr

/--
Parse a size string into a UInt64.
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
Parse `du` arguments into `Options`.
-/
def parseArgs (args : List String) : Options :=
  let rec go (remaining : List String) (opts : Options) : Options :=
    match remaining with
    | [] => opts
    | "--" :: rest => { opts with files := opts.files ++ rest }
    | "-a" :: rest => go rest { opts with all := true }
    | "--all" :: rest => go rest { opts with all := true }
    | "-s" :: rest => go rest { opts with summarize := true }
    | "--summarize" :: rest => go rest { opts with summarize := true }
    | "-h" :: rest => go rest { opts with human := true }
    | "--human-readable" :: rest => go rest { opts with human := true }
    | "-B" :: s :: rest => go rest { opts with blockSize := parseSize s }
    | "--block-size" :: s :: rest => go rest { opts with blockSize := parseSize s }
    | "--max-depth" :: s :: rest =>
      let depth := match s.toInt? with | some n => n | none => -1
      go rest { opts with maxDepth := depth }
    | s :: rest =>
      if s.startsWith "--" then
        -- unknown long option: treat as file
        { opts with files := opts.files ++ s :: rest }
      else if s.startsWith "-" && s.length > 1 then
        -- Combined short flags, e.g. -sh = -s -h
        let flags := (s.drop 1).toString.toList
        let rec handleFlags (fs : List Char) (curOpts : Options) : Options :=
          match fs with
          | [] => go rest curOpts
          | 'a' :: rfs => handleFlags rfs { curOpts with all := true }
          | 's' :: rfs => handleFlags rfs { curOpts with summarize := true }
          | 'h' :: rfs => handleFlags rfs { curOpts with human := true }
          | _ :: rfs => handleFlags rfs curOpts  -- ignore unknown flags
        handleFlags flags opts
      else
        go rest { opts with files := opts.files ++ [s] }
  go args {}

/--
Human-readable size formatting.
-/
def humanSize (bytes : UInt64) : String :=
  let val := bytes
  if val < 1024 then s!"{val}" else
  if val < 1024*1024 then s!"{val / 1024}K" else
  if val < 1024*1024*1024 then s!"{val / (1024*1024)}M" else
  s!"{val / (1024*1024*1024)}G"

/--
Format a single du output line.
-/
def formatLine (blocks : UInt64) (opts : Options) (name : String) : String :=
  let scaled := blocks * 512 / opts.blockSize
  if scaled == 0 && blocks > 0 then "1" else
  if opts.human then
    s!"{humanSize (blocks * 512)}\t{name}\n"
  else
    s!"{scaled}\t{name}\n"

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- Human size 0. -/
example : humanSize 0 = "0" := rfl

/-- Human size 1023. -/
example : humanSize 1023 = "1023" := rfl

/-- Human size 1024. -/
example : humanSize 1024 = "1K" := rfl
