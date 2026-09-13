/-
Shred.Logic — Verified pure logic for `shred`. 0BSD

Spec-First Methodology:
  1. State types    — ShredInput (options + files)
  2. Specification  — patternForPass, parseArgs: the formal "what"
  3. Invariants     — parametric properties over all inputs
  4. Lemmas         — helper theorems used in proofs
  5. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.

`shred` overwrites files with patterns to make data harder to recover.
The pure layer handles argument parsing and pattern generation.
-/

namespace Lentils.Shred.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Options controlling `shred` behaviour.
-/
structure Options where
  passes : Nat := 3
  force : Bool := false
  verbose : Bool := false
  -- GNU keeps the file unless -u/--remove is given.
  remove : Bool := false
  exact : Bool := false
  zero : Bool := true
  deriving Repr, BEq, DecidableEq, Inhabited

/--
Input state for shred.
-/
structure ShredInput where
  opts : Options
  files : List String
  deriving Inhabited, BEq, DecidableEq

/--
Default input: default options, no files.
-/
def defaultInput : ShredInput := { opts := {}, files := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse shred arguments into ShredInput.
-/
def parseArgs (args : List String) : ShredInput :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : ShredInput :=
    match remaining with
    | [] => { opts := opts, files := files.reverse }
    | "--" :: rest => { opts := opts, files := files.reverse ++ rest }
    | "-n" :: n :: rest =>
      let passes := match n.toNat? with | some p => p | none => 3
      go rest { opts with passes := passes } files
    | "--iterations" :: n :: rest =>
      let passes := match n.toNat? with | some p => p | none => 3
      go rest { opts with passes := passes } files
    | "-f" :: rest => go rest { opts with force := true } files
    | "--force" :: rest => go rest { opts with force := true } files
    | "-v" :: rest => go rest { opts with verbose := true } files
    | "--verbose" :: rest => go rest { opts with verbose := true } files
    | "-u" :: rest => go rest { opts with remove := true } files
    | "--remove" :: rest => go rest { opts with remove := true } files
    | "-z" :: rest => go rest { opts with zero := true } files
    | "--zero" :: rest => go rest { opts with zero := true } files
    | s :: rest =>
      if s.startsWith "-" && s != "-" then defaultInput
      else go rest opts (s :: files)
  go args {} []

/--
Generate a pattern for pass number p (0-indexed).
Patterns cycle through three values:
  0x00 (all zeros), 0xFF (all ones), 0x55 (alternating 01010101)
-/
def patternForPass (p : Nat) : UInt8 :=
  match p % 3 with
  | 0 => 0x00    -- all zeros
  | 1 => 0xFF    -- all ones
  | _ => 0x55    -- alternating 01010101

/--
Specification: parse args into shred input.
-/
def specParse (args : List String) : ShredInput := parseArgs args

/--
Specification: generate pattern for a given pass.
-/
def specPattern (pass : Nat) : UInt8 := patternForPass pass

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Pattern for pass 0 is 0x00 (zeros).
-/
theorem i_pattern_pass0 : patternForPass 0 = 0x00 := by native_decide

/--
I2: Pattern for pass 1 is 0xFF (ones).
-/
theorem i_pattern_pass1 : patternForPass 1 = 0xFF := by native_decide

/--
I3: Pattern for pass 2 is 0x55 (alternating).
-/
theorem i_pattern_pass2 : patternForPass 2 = 0x55 := by native_decide

/--
I4: Pattern cycles with period 3: pass n+3 = pass n.
Parametric over all pass numbers.
-/
theorem i_pattern_cycle (n : Nat) : patternForPass (n + 3) = patternForPass n := by
  unfold patternForPass
  have h_mod : (n + 3) % 3 = n % 3 := by omega
  simp [h_mod]

/--
I5: Default input has 3 passes.
-/
theorem i_default_passes : defaultInput.opts.passes = 3 := rfl

/--
I6: Parsing empty args gives default input with no files.
-/
theorem i_parse_empty : parseArgs [] = defaultInput := by
  native_decide

/--
I7: Parsing "-n 5 f" gives passes=5, files=["f"].
-/
theorem i_parse_n_flag : (parseArgs ["-n", "5", "f"]).opts.passes = 5 := by
  native_decide

/--
I8: Parsing "-v f" gives verbose=true.
-/
theorem i_parse_verbose : (parseArgs ["-v", "f"]).opts.verbose = true := by
  native_decide

/--
I9: Parsing "-f f" gives force=true.
-/
theorem i_parse_force : (parseArgs ["-f", "f"]).opts.force = true := by
  native_decide

/--
I10: Parsing "-u f" gives remove=true.
-/
theorem i_parse_remove : (parseArgs ["-u", "f"]).opts.remove = true := by
  native_decide

/--
I11: Parsing "-z f" gives zero=true.
-/
theorem i_parse_zero : (parseArgs ["-z", "f"]).opts.zero = true := by
  native_decide

/--
I12: Parsing "--" stops flag processing.
-/
theorem i_parse_double_dash : (parseArgs ["--", "-n", "5"]).files = ["-n", "5"] := by
  native_decide

/--
I13: Unknown flag returns empty result.
-/
theorem i_parse_unknown_flag : (parseArgs ["--bogus"]).files = [] := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Pattern Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
0x00 is not equal to 0xFF.
-/
theorem pattern_00_ne_FF : (0x00 : UInt8) ≠ 0xFF := by
  native_decide

/--
0xFF is not equal to 0x55.
-/
theorem pattern_FF_ne_55 : (0xFF : UInt8) ≠ 0x55 := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- patternForPass 0 = 0x00. -/
example : patternForPass 0 = 0x00 := i_pattern_pass0

/-- patternForPass 1 = 0xFF. -/
example : patternForPass 1 = 0xFF := i_pattern_pass1

/-- patternForPass 2 = 0x55. -/
example : patternForPass 2 = 0x55 := i_pattern_pass2

/-- patternForPass 3 = patternForPass 0 (cycle). -/
example : patternForPass 3 = patternForPass 0 := i_pattern_cycle 0

/-- parse single file. -/
example : (parseArgs ["file.txt"]).files = ["file.txt"] := by
  native_decide

/-- parse with passes. -/
example : (parseArgs ["-n", "5", "f"]).opts.passes = 5 := i_parse_n_flag

end Lentils.Shred.Logic
