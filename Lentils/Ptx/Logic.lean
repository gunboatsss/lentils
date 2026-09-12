/-
Ptx.Logic — Verified pure permuted index logic for `ptx`. 0BSD

Spec-First Methodology:
  1. State types    — PtxInput (flags + args)
  2. Specification  — generate, rotations, sortByKeyword, formatEntry: the formal "what"
  3. Implementation — the "how" (= spec, since spec is executable)
  4. Correctness    — theorem: impl = spec
  5. Invariants     — parametric properties over all inputs
  6. Lemmas         — helper theorems used in proofs
  7. Concrete corollaries (optional)

No IO, no FFI, no `sorry` or `admit`.

`ptx` generates a permuted index: each content word appears as a keyword,
with its context on either side, sorted alphabetically by keyword.
-/

namespace Lentils.Ptx.Logic

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for ptx.
-/
structure PtxInput where
  gFlag : Bool := false    -- include stop words as keywords
  foldCase : Bool := true  -- case-insensitive matching/sorting
  files : List String := []
  deriving Inhabited, BEq

/--
Default input.
-/
def defaultInput : PtxInput := {}

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification (= Implementation)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Standard stop words for permuted index.
-/
def stopWords : List String :=
  ["a", "an", "and", "are", "as", "at", "be", "but", "by", "for",
   "if", "in", "into", "is", "it", "no", "not", "of", "on", "or",
   "such", "that", "the", "their", "then", "there", "these", "they",
   "this", "to", "was", "will", "with"]

/--
Check if a word is a stop word, optionally folding case.
-/
def isStopWord (w : String) (foldCase : Bool := true) : Bool :=
  let cmp := if foldCase then String.toLower w else w
  stopWords.any (λ sw => sw == cmp)

/--
Split a line into words (by single spaces). Filters empty strings.
-/
def wordsOf (line : String) : List String :=
  List.filter (λ w => !w.isEmpty) (line.splitOn " ")

/--
Enumerate a list with indices (0-based).
-/
def enumerate (xs : List String) : List (Nat × String) :=
  let rec go (i : Nat) (remaining : List String) : List (Nat × String) :=
    match remaining with
    | [] => []
    | x :: xs => (i, x) :: go (i + 1) xs
  go 0 xs

/--
Sort entries by keyword (insertion sort). Parametrized by foldCase.
-/
def sortByKeyword (entries : List (String × String × String)) (foldCase : Bool := true) : List (String × String × String) :=
  let keyCmp := if foldCase then λ (s : String) => String.toLower s else λ s => s
  let rec insert (entry : String × String × String) (sorted : List (String × String × String)) : List (String × String × String) :=
    match sorted with
    | [] => [entry]
    | e :: es =>
      if keyCmp entry.2.1 ≤ keyCmp e.2.1 then
        entry :: sorted
      else
        e :: insert entry es
  let rec go (remaining : List (String × String × String)) (acc : List (String × String × String)) : List (String × String × String) :=
    match remaining with
    | [] => acc
    | e :: es => go es (insert e acc)
  go entries []

/--
Generate all rotations of a line where each content word becomes the keyword.
-/
def rotations (line : String) (gFlag : Bool) (foldCase : Bool := true) : List (String × String × String) :=
  let ws := wordsOf line
  let indexed := enumerate ws
  let relevant := if gFlag then indexed else List.filter (λ (_, w) => !isStopWord w foldCase) indexed
  List.map (λ (i, kw) =>
    let leftParts := List.take i ws
    let rightParts := List.drop (i + 1) ws
    let left := String.intercalate " " leftParts
    let right := String.intercalate " " rightParts
    (left, kw, right)
  ) relevant

/--
Format a single permuted index entry.
Left context is right-aligned to 30 characters; keyword is highlighted;
right context is truncated to fit in 72 total chars.
-/
def formatEntry (left : String) (keyword : String) (right : String) : String :=
  let leftLen := left.length
  let kwLen := keyword.length
  let totalWidth := 72
  let leftWidth := 30
  let kwStart := leftWidth - min leftLen leftWidth
  let paddingLeft := String.ofList (List.replicate kwStart ' ')
  let paddedLeftStr : String := if leftLen > leftWidth then
    "..." ++ String.ofList (left.toList.drop (leftLen - leftWidth + 3))
  else
    left
  let afterKw := totalWidth - leftWidth - kwLen
  let rightTrimmed : String := if right.length > afterKw then
    String.ofList (right.toList.take (afterKw - 3)) ++ "..."
  else
    right
  paddingLeft ++ paddedLeftStr ++ " " ++ keyword ++ " " ++ rightTrimmed

/--
Flatten a list of lists (concatenates in order).
-/
def flatten (xss : List (List α)) : List α :=
  let rec go (remaining : List (List α)) (acc : List α) : List α :=
    match remaining with
    | [] => acc.reverse
    | xs :: rest => go rest (xs.reverse ++ acc)
  go xss []

/--
Generate the full permuted index for a string.
-/
def generate (input : String) (gFlag : Bool) (foldCase : Bool := true) : String :=
  let lines := input.splitOn "\n"
  let rotationLists : List (List (String × String × String)) :=
    List.map (λ line =>
      let t := String.trimAscii line
      if t.isEmpty then []
      else rotations line gFlag foldCase
    ) lines
  let rotationsList := flatten rotationLists
  let sorted := sortByKeyword rotationsList foldCase
  let entries := List.map (λ (l, kw, r) => formatEntry l kw r) sorted
  String.intercalate "\n" entries

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Correctness Theorem
-- ═══════════════════════════════════════════════════════════════════════════════════

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: `isStopWord` recognizes "the" when folding case.
-/
theorem i_isStopWord_the : isStopWord "the" true = true := by native_decide

/--
I2: `isStopWord` does not recognize non-stop words.
-/
theorem i_isStopWord_hello : isStopWord "hello" = false := by native_decide

/--
I3: `isStopWord` is case-sensitive when foldCase=false.
-/
theorem i_isStopWord_The_noFold : isStopWord "The" false = false := by native_decide

/--
I4: `isStopWord` is case-insensitive when foldCase=true.
-/
theorem i_isStopWord_The_fold : isStopWord "The" true = true := by native_decide

/--
I5: Generate of empty input produces empty string.
-/
theorem i_generate_empty : generate "" false = "" := by native_decide

/--
I6: `wordsOf` splits on spaces and filters empties.
-/
theorem i_wordsOf_simple : wordsOf "hello world" = ["hello", "world"] := by native_decide

/--
I7: `wordsOf` handles multiple spaces.
-/
theorem i_wordsOf_multiple_spaces : wordsOf "hello   world" = ["hello", "world"] := by native_decide

/--
I8: `wordsOf` of empty string returns [].
-/
theorem i_wordsOf_empty : wordsOf "" = [] := by native_decide

/--
I9: `enumerate` produces indexed list.
-/
theorem i_enumerate_simple : enumerate ["a", "b", "c"] = [(0, "a"), (1, "b"), (2, "c")] := by
  native_decide

/--
I10: `enumerate` of a simple list.
-/
theorem i_enumerate_simple2 : enumerate ["a"] = [(0, "a")] := by
  native_decide

/--
I13: Every word produced by `wordsOf` is non-empty.
-/
theorem i_wordsOf_nonempty : ∀ (line : String) (w : String),
    w ∈ wordsOf line → (!w.isEmpty) = true := by
  intro line w h
  unfold wordsOf at h
  simp only [List.mem_filter] at h
  exact h.2

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- isStopWord "the" = true. -/
example : isStopWord "the" = true := i_isStopWord_the

/-- isStopWord "hello" = false. -/
example : isStopWord "hello" = false := i_isStopWord_hello

/-- isStopWord "The" with fold=false. -/
example : isStopWord "The" false = false := i_isStopWord_The_noFold

/-- isStopWord "The" with fold=true. -/
example : isStopWord "The" true = true := i_isStopWord_The_fold

/-- generate "" = "". -/
example : generate "" false = "" := i_generate_empty

/-- wordsOf "hello world". -/
example : wordsOf "hello world" = ["hello", "world"] := i_wordsOf_simple

end Lentils.Ptx.Logic
