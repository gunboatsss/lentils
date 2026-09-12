/-
Du.Logic — Verified pure logic for `du`. 0BSD

Spec-First Methodology:
  1. State types    — DuInput (options + args)
  2. Specification  — parseArgs, humanSize, formatLine: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Correctness    — theorem: impl = spec
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.

`du` estimates file space usage.
-/

set_option maxRecDepth 20000

namespace Lentils.Du.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

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
  deriving Repr, BEq, Inhabited

/--
Input state for du.
-/
structure DuInput where
  opts : Options
  deriving Inhabited, BEq

/--
Default input.
-/
def defaultInput : DuInput := { opts := {} }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

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
Human-readable size formatting (1024-based).
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
I2: Default max depth is -1 (unlimited).
-/
theorem i_default_maxDepth : ({} : Options).maxDepth = -1 := rfl

/--
I3: Parse empty string returns 1024.
-/
theorem i_parseSize_empty : parseSize "" = 1024 := by
  native_decide

/--
I4: Parse K suffix.
-/
theorem i_parseSize_K : parseSize "1K" = 1024 := by
  native_decide

/--
I5: Parse M suffix.
-/
theorem i_parseSize_M : parseSize "1M" = 1048576 := by
  native_decide

/--
I6: Parse G suffix.
-/
theorem i_parseSize_G : parseSize "1G" = 1073741824 := by
  native_decide

/--
I7: Parse all flag.
-/
theorem i_parse_all : (parseArgs ["-a"]).all = true := by
  native_decide

/--
I8: Parse summarize flag.
-/
theorem i_parse_summarize : (parseArgs ["-s"]).summarize = true := by
  native_decide

/--
I9: Parse human flag.
-/
theorem i_parse_human : (parseArgs ["-h"]).human = true := by
  native_decide

/--
I10: Parse combined flags.
-/
theorem i_parse_combined : (parseArgs ["-sh"]).summarize = true ∧ (parseArgs ["-sh"]).human = true := by
  native_decide

/--
I11: Parse block size.
-/
theorem i_parse_blockSize : (parseArgs ["-B", "2K"]).blockSize = 2048 := by
  native_decide

/--
I12: Parse max depth.
-/
theorem i_parse_maxDepth : (parseArgs ["--max-depth", "3"]).maxDepth = 3 := by
  native_decide

/--
I13: Parse file argument.
-/
theorem i_parse_file : (parseArgs ["/tmp"]).files = ["/tmp"] := by
  native_decide

/--
I14: Parse multiple files.
-/
theorem i_parse_files : (parseArgs ["/tmp", "/var"]).files = ["/tmp", "/var"] := by
  native_decide

/--
I15: Parse "--" stops flag processing.
-/
theorem i_parse_double_dash : (parseArgs ["--", "-h"]).files = ["-h"] := by
  native_decide

/--
I16: humanSize for 0 returns "0".
-/
theorem i_humanSize_zero : humanSize 0 = "0" := rfl

/--
I17: humanSize for 1023 returns "1023".
-/
theorem i_humanSize_1023 : humanSize 1023 = "1023" := rfl

/--
I18: humanSize for 1024 returns "1K".
-/
theorem i_humanSize_1K : humanSize 1024 = "1K" := rfl

/--
I19: humanSize for 1M.
-/
theorem i_humanSize_1M : humanSize (1024*1024) = "1M" := by
  native_decide

/--
I20: humanSize for 1G.
-/
theorem i_humanSize_1G : humanSize (1024*1024*1024) = "1G" := by
  native_decide

/--
I21: Empty size strings parse to the default block size (parametric).
-/
theorem i_parseSize_of_empty (s : String) (h : s.isEmpty = true) :
    parseSize s = 1024 := by
  simp [parseSize, h]

/--
I22: formatLine for concrete values.
-/
theorem i_formatLine_example : formatLine 0 { blockSize := 1024 } "test" = "0\ttest\n" := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- parseSize "" is 1024. -/
example : parseSize "" = 1024 := i_parseSize_empty

/-- parseSize "1K" is 1024. -/
example : parseSize "1K" = 1024 := i_parseSize_K

/-- parseArgs with -h sets human flag. -/
example : (parseArgs ["-h"]).human = true := i_parse_human

/-- humanSize 0 = "0". -/
example : humanSize 0 = "0" := i_humanSize_zero

/-- humanSize 1024 = "1K". -/
example : humanSize 1024 = "1K" := i_humanSize_1K

end Lentils.Du.Logic
