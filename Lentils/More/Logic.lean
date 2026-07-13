/-
More.Logic — Pure paging logic for `more`.
0BSD

This file contains ONLY pure functions — no IO, no FFI.
Formal proofs are at the bottom.
No `sorry` or `admit` allowed.

The `more` utility paginates stdin to stdout, pausing after each
screenful and waiting for a keypress before continuing.
-/

namespace Lentils.More.Logic

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
Returns empty string if total is 0.
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
Exit code for successful completion.
-/
def exitOK : UInt32 := 0

/--
Exit code for error.
-/
def exitError : UInt32 := 1

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/--
Default lines is positive.
-/
theorem defaultLines_pos : defaultLines > 0 := by
  native_decide

/--
`pageSize` with a height > 1 yields a positive number.
-/
theorem pageSize_pos (h : Nat) (hpos : h > 1) : pageSize h > 0 := by
  unfold pageSize
  split
  · omega
  · omega

/--
`pageSize` for default height.
-/
theorem pageSize_default : pageSize defaultLines = 23 := by
  native_decide

/--
`isQuit` returns true for lowercase 'q'.
-/
theorem isQuit_lowercase_q : isQuit 113 = true := by
  native_decide

/--
`isQuit` returns true for uppercase 'Q'.
-/
theorem isQuit_uppercase_Q : isQuit 81 = true := by
  native_decide

/--
`isQuit` returns false for space (32).
-/
theorem isQuit_space : isQuit 32 = false := by
  native_decide

/--
`splitLines` preserves empty input.
-/
theorem splitLines_empty : splitLines "" = [""] := by
  native_decide

/--
`splitLines` of a single-line input.
-/
theorem splitLines_single : splitLines "hello" = ["hello"] := by
  native_decide

/--
`splitLines` of multi-line input.
-/
theorem splitLines_multi : splitLines "a\nb\nc" = ["a", "b", "c"] := by
  native_decide

/--
`pageIndicator` at 0% of non-empty total.
-/
theorem pageIndicator_zero : pageIndicator 0 100 = "--More--(0%)" := rfl

/--
`pageIndicator` at 100% or more returns empty string.
-/
theorem pageIndicator_full : pageIndicator 100 100 = "" := rfl

/--
`pageIndicator` at 50%.
-/
theorem pageIndicator_half : pageIndicator 50 100 = "--More--(50%)" := rfl

/--
`pageIndicator` for zero total returns empty string.
-/
theorem pageIndicator_zero_total : pageIndicator 0 0 = "" := rfl

end Lentils.More.Logic
