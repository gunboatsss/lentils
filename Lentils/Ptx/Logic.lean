/-
Ptx.Logic — Pure permuted index generation for `ptx`. 0BSD
-/

namespace Lentils.Ptx.Logic

def stopWords : List String :=
  ["a", "an", "and", "are", "as", "at", "be", "but", "by", "for",
   "if", "in", "into", "is", "it", "no", "not", "of", "on", "or",
   "such", "that", "the", "their", "then", "there", "these", "they",
   "this", "to", "was", "will", "with"]

def isStopWord (w : String) (foldCase : Bool := true) : Bool :=
  let cmp := if foldCase then String.toLower w else w
  stopWords.any (λ sw => sw == cmp)

/-- Split a line into words (by whitespace). -/
def wordsOf (line : String) : List String :=
  let parts := line.splitOn " "
  List.filter (λ w => !w.isEmpty) parts

/-- Enumerate a list with indices. -/
def enumerate (xs : List String) : List (Nat × String) :=
  let rec go (i : Nat) (remaining : List String) : List (Nat × String) :=
    match remaining with
    | [] => []
    | x :: xs => (i, x) :: go (i + 1) xs
  go 0 xs

/-- Simple sort by keyword (insertion sort). -/
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

/-- Generate all rotations of a line where each content word becomes the keyword. -/
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

/-- Format a single permuted index entry. -/
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

/-- Flatten a list of lists. -/
def flatten (xss : List (List (String × String × String))) : List (String × String × String) :=
  let rec go (remaining : List (List (String × String × String))) (acc : List (String × String × String)) : List (String × String × String) :=
    match remaining with
    | [] => acc.reverse
    | xs :: rest => go rest (xs.reverse ++ acc)
  go xss []

/-- Generate the full permuted index for a string. -/
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

/-- Format a line for output in the permuted index. -/
def formatLine (line : String) (gFlag : Bool) (foldCase : Bool := true) : String :=
  let ws := wordsOf line
  if ws.isEmpty then ""
  else
    let rots := rotations line gFlag foldCase
    let entries := List.map (λ (l, kw, r) => formatEntry l kw r) rots
    String.intercalate "\n" entries

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : isStopWord "the" = true := by native_decide
example : isStopWord "hello" = false := by native_decide
example : isStopWord "The" false = false := by native_decide
example : isStopWord "The" true = true := by native_decide
example : generate "" false = "" := by native_decide
example : wordsOf "hello world" = ["hello", "world"] := by native_decide

end Lentils.Ptx.Logic
