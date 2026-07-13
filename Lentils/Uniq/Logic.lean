/-
Uniq.Logic — Pure deduplication for `uniq`. 0BSD -/
import Lentils.Common.Lines
namespace Lentils.Uniq.Logic
open Lentils.Common.Lines
open ByteArray

inductive Mode | normal | unique | repeated deriving Inhabited, DecidableEq

def parseArgs (args : List String) : Mode × List String :=
  let rec go (args : List String) (mode : Mode) : Mode × List String :=
    match args with | [] => (mode, []) | "-d" :: rest => go rest Mode.repeated | "-u" :: rest => go rest Mode.unique | arg :: rest => if arg.startsWith "-" && arg ≠ "-" then go rest mode else (mode, arg :: rest)
  go args Mode.normal

def groupAdjacentDuplicates (lines : List ByteArray) : List (ByteArray × Nat) :=
  let rec go (lines : List ByteArray) (current : ByteArray) (count : Nat) : List (ByteArray × Nat) :=
    match lines with | [] => [(current, count)] | l :: rest => if l = current then go rest current (count + 1) else (current, count) :: go rest l 1
  match lines with | [] => [] | first :: rest => go rest first 1

def processLines (input : ByteArray) (mode : Mode) : ByteArray :=
  let lines := splitLines input
  let groups := groupAdjacentDuplicates lines
  let filtered : List ByteArray :=
    match mode with
    | Mode.normal => groups.map (λ (h, _) => h)
    | Mode.unique => (groups.filter (λ (_, c) => c = 1)).map (λ (h, _) => h)
    | Mode.repeated => (groups.filter (λ (_, c) => c > 1)).map (λ (h, _) => h)
  joinLines filtered

theorem processLines_empty : processLines ByteArray.empty Mode.normal = ByteArray.empty := by
  unfold processLines splitLines joinLines groupAdjacentDuplicates
  native_decide

-- ─── Normal mode ─────────────────────────────────────────────────────────────

/-- Normal mode suppresses adjacent duplicates: "a\na\nb" becomes "a\nb". -/
theorem processLines_normalSuppressesAdjacentDuplicates :
  processLines (ByteArray.mk #[0x61, 0x0A, 0x61, 0x0A, 0x62]) Mode.normal =
  ByteArray.mk #[0x61, 0x0A, 0x62] := by
  native_decide

/-- A run of three adjacent duplicates collapses to a single line in normal mode. -/
theorem processLines_normalCollapsesRun :
  processLines (ByteArray.mk #[0x78, 0x0A, 0x78, 0x0A, 0x78]) Mode.normal =
  ByteArray.mk #[0x78] := by
  native_decide

-- ─── uniq -u (unique only) ───────────────────────────────────────────────────

/-- `-u` prints only lines that never repeat: "a\nb\nb" yields just "a". -/
theorem processLines_uniqueOnlyUnique :
  processLines (ByteArray.mk #[0x61, 0x0A, 0x62, 0x0A, 0x62]) Mode.unique =
  ByteArray.mk #[0x61] := by
  native_decide

/-- `-u` on an all-duplicate input yields the empty output. -/
theorem processLines_uniqueAllDuplicateEmpty :
  processLines (ByteArray.mk #[0x78, 0x0A, 0x78, 0x0A, 0x78]) Mode.unique =
  ByteArray.empty := by
  native_decide

-- ─── uniq -d (repeated only) ─────────────────────────────────────────────────

/-- `-d` prints only repeated lines: "a\nb\nb" yields just "b". -/
theorem processLines_repeatedOnlyRepeated :
  processLines (ByteArray.mk #[0x61, 0x0A, 0x62, 0x0A, 0x62]) Mode.repeated =
  ByteArray.mk #[0x62] := by
  native_decide

/-- `-d` on an all-duplicate input yields the line printed once. -/
theorem processLines_repeatedAllDuplicate :
  processLines (ByteArray.mk #[0x78, 0x0A, 0x78, 0x0A, 0x78]) Mode.repeated =
  ByteArray.mk #[0x78] := by
  native_decide

-- ─── Boundary / shape inputs ─────────────────────────────────────────────────

/-- A single line with no trailing newline is returned unchanged. -/
theorem processLines_singleLine :
  processLines (ByteArray.mk #[0x61, 0x62]) Mode.normal =
  ByteArray.mk #[0x61, 0x62] := by
  native_decide

/-- An empty input yields an empty output in every mode. -/
theorem processLines_emptyAllModes (m : Mode) :
  processLines ByteArray.empty m = ByteArray.empty := by
  cases m <;> native_decide

-- ─── Interleaved duplicates ──────────────────────────────────────────────────

/-- Non-adjacent (interleaved) duplicates are kept: "a\nb\na" stays "a\nb\na". -/
theorem processLines_interleavedKept :
  processLines (ByteArray.mk #[0x61, 0x0A, 0x62, 0x0A, 0x61]) Mode.normal =
  ByteArray.mk #[0x61, 0x0A, 0x62, 0x0A, 0x61] := by
  native_decide

/-- `-u` keeps both interleaved copies since neither run repeats. -/
theorem processLines_interleavedUniqueKept :
  processLines (ByteArray.mk #[0x61, 0x0A, 0x62, 0x0A, 0x61]) Mode.unique =
  ByteArray.mk #[0x61, 0x0A, 0x62, 0x0A, 0x61] := by
  native_decide

-- ─── parseArgs ───────────────────────────────────────────────────────────────

/-- parseArgs recognizes the `-u` (unique) flag. -/
theorem parseArgs_unique : parseArgs ["-u"] = (Mode.unique, []) := by native_decide

/-- parseArgs recognizes the `-d` (repeated) flag. -/
theorem parseArgs_repeated : parseArgs ["-d"] = (Mode.repeated, []) := by native_decide

end Lentils.Uniq.Logic
