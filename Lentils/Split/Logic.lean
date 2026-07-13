/-
Split.Logic — Pure split logic for `split`. 0BSD
-/

namespace Lentils.Split.Logic

def defaultPrefix : String := "x"

/-- Convert a number to a base-26 alphabetic representation (a-z).
    Builds digits least-significant first via append. -/
def toBase26 (n : Nat) : List Char :=
  if h0 : n = 0 then []
  else
    have h : n / 26 < n := by
      refine Nat.div_lt_self ?_ (by decide)
      exact Nat.pos_of_ne_zero h0
    let d := Char.ofNat ((n % 26) + 97)
    toBase26 (n / 26) ++ [d]
termination_by n

/-- Generate a suffix string for chunk index i with given length and numeric flag.
    Alphabetic: a=0, b=1, ..., z=25, padded with 'a' to length. -/
def suffix (i : Nat) (len : Nat := 2) (numeric : Bool := false) : String :=
  if numeric then
    let s := toString i
    if s.length ≥ len then s
    else String.ofList (List.replicate (len - s.length) '0') ++ s
  else
    let base26 := toBase26 i
    let chars := if base26.isEmpty then ['a'] else base26
    if chars.length ≥ len then String.ofList chars
    else String.ofList (List.replicate (len - chars.length) 'a') ++ String.ofList chars

/-- Split input lines into chunks. Uses partial for recursive termination. -/
partial def splitLines (input : List String) (maxLines : Nat) : List (Nat × List String) :=
  let rec go (remaining : List String) (chunkIdx : Nat) (acc : List (Nat × List String)) : List (Nat × List String) :=
    if remaining.isEmpty then acc.reverse
    else
      let chunk :=
        if maxLines > 0 then remaining.take maxLines
        else remaining
      let rest :=
        if maxLines > 0 then remaining.drop maxLines
        else []
      go rest (chunkIdx + 1) ((chunkIdx, chunk) :: acc)
  go input 0 []

/-- Split input by line count. -/
def splitByLines (input : String) (maxLines : Nat) (suffixLen : Nat := 2) (numericSuffix : Bool := false) : List (String × String) :=
  let lines := input.splitOn "\n"
  let chunks := splitLines lines maxLines
  chunks.map (λ (idx, chunk) =>
    (suffix idx suffixLen numericSuffix, String.intercalate "\n" chunk ++ (if input.endsWith "\n" then "\n" else "")))

/-- Split input bytes into chunks. -/
partial def splitBytes (input : ByteArray) (maxBytes : Nat) (suffixLen : Nat := 2) (numericSuffix : Bool := false) : List (String × ByteArray) :=
  let rec go (offset : Nat) (chunkIdx : Nat) (acc : List (String × ByteArray)) : List (String × ByteArray) :=
    if offset ≥ input.size then acc.reverse
    else
      let endPos := min (offset + maxBytes) input.size
      let chunk := input.extract offset endPos
      go endPos (chunkIdx + 1) ((suffix chunkIdx suffixLen numericSuffix, chunk) :: acc)
  go 0 0 []

/-- Parse a number with optional suffix. -/
def parseSuffixed (s : String) : Option Nat :=
  let (numPart, mult) :=
    if s.endsWith "b" then (s.dropEnd 1, 512)
    else if s.endsWith "k" then (s.dropEnd 1, 1024)
    else if s.endsWith "K" then (s.dropEnd 1, 1024)
    else if s.endsWith "M" then (s.dropEnd 1, 1024 * 1024)
    else if s.endsWith "G" then (s.dropEnd 1, 1024 * 1024 * 1024)
    else (s, 1)
  match numPart.toNat? with
  | some n => some (n * mult)
  | none => none

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : suffix 0 = "aa" := by native_decide
example : suffix 1 = "ab" := by native_decide
example : suffix 25 = "az" := by native_decide
example : suffix 26 = "ba" := by native_decide
example : suffix 0 3 false = "aaa" := by native_decide
example : suffix 0 2 true = "00" := by native_decide
example : suffix 5 2 true = "05" := by native_decide
example : suffix 10 2 true = "10" := by native_decide
example : parseSuffixed "100" = some 100 := by native_decide
example : parseSuffixed "2k" = some 2048 := by native_decide

end Lentils.Split.Logic
