/-
Fmt.Logic — Pure text reformatter for `fmt`. 0BSD

Reformats paragraphs of text to a target line width, breaking on word
boundaries. Pure functions only; proofs use native_decide.
-/

namespace Lentils.Fmt.Logic

/-- A line is "blank" when it contains only whitespace. -/
def isBlank (l : String) : Bool :=
  l.trimAscii.isEmpty

/--
Split input into paragraphs. A paragraph is a maximal run of non-blank
lines; blank lines separate paragraphs.
-/
def splitParagraphs (input : String) : List (List String) :=
  let lines := input.splitOn "\n"
  let rec go (ls : List String) (cur : List String) (acc : List (List String)) : List (List String) :=
    match ls with
    | [] =>
      if cur.isEmpty then acc.reverse else (cur.reverse :: acc).reverse
    | l :: rest =>
      if isBlank l then
        if cur.isEmpty then go rest [] acc
        else go rest [] (cur.reverse :: acc)
      else
        go rest (l :: cur) acc
  go lines [] []

/-- Join paragraph lines with single spaces. -/
def joinParagraph (lines : List String) : String :=
  match lines with
  | [] => ""
  | first :: rest =>
    let rec go (acc : String) (ls : List String) : String :=
      match ls with
      | [] => acc
      | l :: ls => go (acc ++ " " ++ l) ls
    go first rest

/-- String take/drop that return `String` (not `Slice`). -/
def strTake (s : String) (n : Nat) : String :=
  String.ofList (s.toList.take n)

def strDrop (s : String) (n : Nat) : String :=
  String.ofList (s.toList.drop n)

/-- Break a word longer than `width` into chunks of exactly `width`. -/
def breakWord (w : String) (width : Nat) : List String :=
  if width = 0 then [w]
  else if w.isEmpty then []
  else
    let cs := w.toList
    let nChunks := (cs.length + width - 1) / width
    List.range nChunks |>.map (λ i =>
      String.ofList ((cs.drop (i * width)).take width))

/--
Wrap a single paragraph (already joined into one string) at `width`,
breaking on spaces. Words longer than `width` are hard-split.
-/
def wrapLine (text : String) (width : Nat) : List String :=
  if width = 0 then [text]
  else
    let words := text.splitOn " " |>.filter (not ·.isEmpty)
    let rec go (ws : List String) (cur : String) (acc : List String) : List String :=
      match ws with
      | [] => (cur :: acc).reverse
      | w :: rest =>
        let cand := if cur.isEmpty then w else s!"{cur} {w}"
        if cand.length ≤ width then
          go rest cand acc
        else
          let acc1 := cur :: acc
          let pieces := breakWord w width
          let pl := pieces.length
          let init := pieces.take (pl - 1)
          let last := (pieces.drop (pl - 1)).headD ""
          go rest last (init.reverse ++ acc1)
    (go words "" []).filter (not ·.isEmpty)

/--
Reformat `input` to `width` columns (default 75), preserving paragraph
structure (blank lines separate paragraphs).
-/
def fmt (input : String) (width : Nat := 75) : String :=
  let paras := splitParagraphs input
  let wrapped := paras.map (λ p => String.intercalate "\n" (wrapLine (joinParagraph p) width))
  String.intercalate "\n\n" wrapped

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : fmt "" = "" := by native_decide
example : fmt "hello world" 80 = "hello world" := by native_decide
example : fmt "hello world" 5 = "hello\nworld" := by native_decide
example : fmt "abcdef" 3 = "abc\ndef" := by native_decide
example : fmt "a\n\nb" 80 = "a\n\nb" := by native_decide

end Lentils.Fmt.Logic
