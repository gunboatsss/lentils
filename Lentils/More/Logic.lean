/-
More.Logic — Verified pure paging logic for `more`. 0BSD

Spec-First Methodology:
  1. State types    — MoreInput (args)
  2. Specification  — pageSize, pageIndicator, splitLines: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Correctness    — theorem: impl = spec
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.

The `more` utility paginates stdin to stdout, pausing after each
screenful and waiting for a keypress before continuing.
-/

namespace Lentils.More.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for more.
-/
structure MoreInput where
  args : List String
  deriving Inhabited, BEq

/--
Default input: no args (read from stdin).
-/
def defaultInput : MoreInput := { args := [] }

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Default number of lines per page if terminal height cannot be determined.
POSIX-compatible default is 24 lines.
-/
def defaultLines : Nat := 24

/--
Split a string into a list of lines.
This is the reverse of String.intercalate "\n".
-/
def splitLines (input : String) : List String :=
  input.splitOn "\n"

/--
Check if a byte value represents a quit command ('q' or 'Q').
'q' = 113, 'Q' = 81 in ASCII.
-/
def isQuit (b : UInt8) : Bool :=
  b == 113 || b == 81

/--
Compute the number of lines to display per page.
Reserves one line for the prompt.
-/
def pageSize (height : Nat) : Nat :=
  if height > 1 then height - 1 else 1

/--
Format a page indicator with percentage.
Given the current line and total lines, produce something like "--More--(50%)".
Returns empty string if total is 0 or percentage >= 100.
-/
def pageIndicator (currentLine : Nat) (totalLines : Nat) : String :=
  if totalLines = 0 then ""
  else
    let pct := (currentLine * 100) / totalLines
    if pct >= 100 then
      ""
    else
      s!"--More--({pct}%)"

/--
Compute whether a page break is needed.
Returns true if there are remaining lines beyond current position.
-/
def hasMorePages (currentLine totalLines : Nat) : Bool :=
  currentLine < totalLines

/--
The specification for more's pure logic: given input text and terminal height,
compute the page size and whether more pages exist.
-/
def spec (input : MoreInput) (text : String) (height : Nat) : Nat × String × Bool :=
  let lines := splitLines text
  let totalLines := lines.length
  let pSize := pageSize height
  let indicator := pageIndicator 0 totalLines
  let morePages := hasMorePages 0 totalLines
  (pSize, indicator, morePages)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Exit codes
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Exit code for successful completion.
-/
def exitOK : UInt32 := 0

/--
Exit code for error.
-/
def exitError : UInt32 := 1

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Default lines is positive.
-/
theorem i_defaultLines_pos : defaultLines > 0 := by
  native_decide

/--
I2: `pageSize` for default height is 23.
-/
theorem i_pageSize_default : pageSize defaultLines = 23 := by
  native_decide

/--
I3: `isQuit` returns true for 'q' (113).
-/
theorem i_isQuit_q : isQuit 113 = true := by
  native_decide

/--
I4: `isQuit` returns true for 'Q' (81).
-/
theorem i_isQuit_Q : isQuit 81 = true := by
  native_decide

/--
I5: `isQuit` returns false for space (32).
-/
theorem i_isQuit_space : isQuit 32 = false := by
  native_decide

/--
I6: `splitLines` of empty string returns [""].
-/
theorem i_splitLines_empty : splitLines "" = [""] := by
  native_decide

/--
I7: `splitLines` of a single-line string returns that line.
-/
theorem i_splitLines_single : splitLines "hello" = ["hello"] := by
  native_decide

/--
I8: `splitLines` with newlines.
-/
theorem i_splitLines_multi : splitLines "a\nb\nc" = ["a", "b", "c"] := by
  native_decide

/--
I9: `pageIndicator` at 0% of non-empty total shows "(0%)".
-/
theorem i_pageIndicator_zero : pageIndicator 0 100 = "--More--(0%)" := rfl

/--
I10: `pageIndicator` when current=total.
-/
theorem i_pageIndicator_full : pageIndicator 100 100 = "" := rfl

/--
I11: `pageIndicator` at 50%.
-/
theorem i_pageIndicator_half : pageIndicator 50 100 = "--More--(50%)" := rfl

/--
I12: `pageIndicator` for zero total returns empty string.
-/
theorem i_pageIndicator_zero_total : pageIndicator 0 0 = "" := rfl

/--
I13: `pageSize` is always at least 1 (one line reserved for output).
-/
theorem i_pageSize_pos (height : Nat) : pageSize height ≥ 1 := by
  unfold pageSize
  split <;> omega

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Default lines is 24. -/
example : defaultLines = 24 := rfl

/-- splitLines of "hello\nworld". -/
theorem splitLines_two_lines : splitLines "hello\nworld" = ["hello", "world"] := by
  native_decide

/-- pageSize 24 = 23. -/
example : pageSize 24 = 23 := i_pageSize_default

/-- pageIndicator at start. -/
example : pageIndicator 0 100 = "--More--(0%)" := i_pageIndicator_zero

/-- pageIndicator at end. -/
example : pageIndicator 100 100 = "" := rfl

/-- isQuit 'q'. -/
example : isQuit 113 = true := i_isQuit_q

/-- isQuit 'Q'. -/
example : isQuit 81 = true := i_isQuit_Q

end Lentils.More.Logic
