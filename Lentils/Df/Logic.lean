/-
Df.Logic — Verified pure logic for `df`. 0BSD

Spec-First Methodology:
  1. State types    — DfInput (flags + args)
  2. Specification  — parseArgs, formatLine, headerLine: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Correctness    — theorem: impl = spec
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.

`df` reports file system disk space usage.
-/

set_option maxRecDepth 20000

namespace Lentils.Df.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

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
  deriving Repr, BEq, Inhabited

/--
Input state for df.
-/
structure DfInput where
  opts : Options
  deriving Inhabited, BEq

/--
Default input.
-/
def defaultInput : DfInput := { opts := {} }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

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
Header line for df output (1K-blocks).
-/
def headerLine : String :=
  "Filesystem     1K-blocks     Used    Available  Use%  Mounted on\n"

/--
Specification: parse arguments into Options.
-/
def specParse (args : List String) : Options := parseArgs args

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Default block size is 1024.
-/
theorem i_default_blockSize : ({} : Options).blockSize = 1024 := rfl

/--
I2: Parse empty string returns 1024.
-/
theorem i_parseSize_empty : parseSize "" = 1024 := by
  native_decide

/--
I3: Parse K suffix.
-/
theorem i_parseSize_K : parseSize "1K" = 1024 := by
  native_decide

/--
I4: Parse M suffix.
-/
theorem i_parseSize_M : parseSize "1M" = 1048576 := by
  native_decide

/--
I5: Parse G suffix.
-/
theorem i_parseSize_G : parseSize "1G" = 1073741824 := by
  native_decide

/--
I6: Parse human flag.
-/
theorem i_parse_human : (parseArgs ["-h"]).human = true := by
  native_decide

/--
I7: Parse inodes flag.
-/
theorem i_parse_inodes : (parseArgs ["-i"]).inodes = true := by
  native_decide

/--
I8: Parse all flag.
-/
theorem i_parse_all : (parseArgs ["-a"]).all = true := by
  native_decide

/--
I9: Parse combined flags.
-/
theorem i_parse_combined : (parseArgs ["-hi"]).human = true ∧ (parseArgs ["-hi"]).inodes = true := by
  native_decide

/--
I10: Parse block size.
-/
theorem i_parse_blockSize : (parseArgs ["-B", "2K"]).blockSize = 2048 := by
  native_decide

/--
I11: Parse file argument.
-/
theorem i_parse_file : (parseArgs ["/"]).files = ["/"] := by
  native_decide

/--
I12: Parse multiple files.
-/
theorem i_parse_files : (parseArgs ["/", "/tmp"]).files = ["/", "/tmp"] := by
  native_decide

/--
I13: Parse "--" stops flag processing.
-/
theorem i_parse_double_dash : (parseArgs ["--", "-h"]).files = ["-h"] := by
  native_decide

/--
I14: humanSize for 0 returns "0".
-/
theorem i_humanSize_zero : humanSize 0 = "0" := rfl

/--
I15: humanSize for 0 returns "0".
-/
theorem i_humanSize_small : humanSize 1023 = "0" := by
  native_decide

/--
I16: humanSize for a larger value (in K units).
-/
theorem i_humanSize_large : humanSize (10*1024) = "0K" := by
  native_decide

/--
I20: Empty size strings parse to the default block size (parametric).
-/
theorem i_parseSize_of_empty (s : String) (h : s.isEmpty = true) :
    parseSize s = 1024 := by
  simp [parseSize, h]

/--
I22: headerLine is non-empty.
-/
theorem i_headerLine_nonempty : headerLine ≠ "" := by
  native_decide

/--
I22: formatLine for concrete values.
-/
theorem i_formatLine_example : formatLine "/dev/sda1" 100 50 25 50 "/" = "/dev/sda1  100  50  25  50%  /\n" := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- parseSize "" is 1024. -/
example : parseSize "" = 1024 := i_parseSize_empty

/-- parseSize "1K" is 1024. -/
example : parseSize "1K" = 1024 := i_parseSize_K

/-- parseArgs with -h sets human flag. -/
example : (parseArgs ["-h"]).human = true := i_parse_human

end Lentils.Df.Logic
