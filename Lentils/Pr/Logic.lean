/-
Pr.Logic — Verified pure logic for `pr`. 0BSD
-/

namespace Lentils.Pr.Logic

structure PrInput where
  input : String := ""
  pageLength : Nat := 66
  columns : Nat := 1
  width : Nat := 72
  title : String := ""
  showHeader : Bool := true
  deriving Inhabited

abbrev PrOutput := String

def listGet {α} (l : List α) (i : Nat) (d : α) : α :=
  let rec go (xs : List α) (j : Nat) : α :=
    match xs with | [] => d | x :: xs => if j = 0 then x else go xs (j - 1)
  go l i

def padRight (s : String) (w : Nat) : String :=
  if s.length ≥ w then s else s ++ String.ofList (List.replicate (w - s.length) ' ')

def headerLine (title : String) (pageNum : Nat) (width : Nat) : String :=
  let right := s!"Page {pageNum}"
  let avail := if width > right.length then width - right.length else 0
  let pad := if avail > title.length then avail - title.length else 0
  title ++ String.ofList (List.replicate pad ' ') ++ right

def colAt (ls : List String) (c n : Nat) : List String :=
  let rec go (xs : List String) (i : Nat) (acc : List String) : List String :=
    match xs with | [] => acc.reverse | x :: xs => go xs (i + 1) (if i % n == c then x :: acc else acc)
  go ls 0 []

def columnize (pageLines : List String) (n : Nat) (width : Nat) : List String :=
  if n ≤ 1 then pageLines
  else
    let colWidth := width / n
    let rowCount := (pageLines.length + n - 1) / n
    let cols := List.range n |>.map (λ c => colAt pageLines c n)
    let rec rows (r : Nat) (acc : List String) : List String :=
      if r ≥ rowCount then acc.reverse
      else
        let cells := List.range n |>.map (λ c => listGet (listGet cols c []) r "")
        let line := String.intercalate "  " (cells.map (λ s => padRight s colWidth))
        rows (r + 1) (line :: acc)
    termination_by rowCount - r
    rows 0 []

def paginateColumns (lines : List String) (n width bodyPerPage : Nat) : List (List String) :=
  let numPages := if lines.isEmpty then 1 else (lines.length + bodyPerPage - 1) / bodyPerPage
  let rec go (pageIdx : Nat) (acc : List (List String)) : List (List String) :=
    if pageIdx = 0 then acc.reverse
    else
      let start := (numPages - pageIdx) * bodyPerPage
      let taken := (lines.drop start).take bodyPerPage
      let page := taken ++ List.replicate (bodyPerPage - taken.length) ""
      go (pageIdx - 1) (columnize page n width :: acc)
  termination_by pageIdx
  go numPages []

def makePage (bodyRows : List String) (pageNum : Nat) (title : String) (width headerLines pageLength : Nat) : List String :=
  let header := if headerLines > 0 then ["", headerLine title pageNum width, "", "", ""] else []
  let header := header.take headerLines
  let total := header.length + bodyRows.length
  let padCount := if pageLength > total then pageLength - total else 0
  header ++ bodyRows ++ List.replicate padCount ""

def pr (input : String) (pageLength columns width : Nat) (title : String) (showHeader : Bool) : String :=
  let lines := input.splitOn "\n"
  let lines := if lines.length > 1 && (lines.getLastD "") == "" then lines.dropLast else lines
  let headerLines := if showHeader then 5 else 0
  let bodyPerPage := if pageLength > headerLines then pageLength - headerLines else 1
  let bodyPages := paginateColumns lines columns width bodyPerPage
  let indexed := bodyPages.zip (List.range bodyPages.length)
  let pages := indexed.map (λ (body, i) => makePage body (i + 1) title width headerLines pageLength)
  let allLines := pages.flatMap id
  String.intercalate "\n" allLines ++ "\n"

def spec (input : PrInput) : PrOutput :=
  pr input.input input.pageLength input.columns input.width input.title input.showHeader

/--
I2: Single column mode does not columnize any list (∀ lines, w).
-/
theorem i_single_column (lines : List String) (w : Nat) : columnize lines 1 w = lines := by
  unfold columnize; simp

/--
I3: padRight on empty string with w=0 returns empty.
-/
theorem i_padRight_empty_0 : padRight "" 0 = "" := by
  unfold padRight; rfl

/--
I4: Example: headerLine with title "x" page 1 width 10.
-/
theorem i_headerLine_example : headerLine "x" 1 10 = "x   Page 1" := by
  native_decide

/--
I5: listGet default works for empty lists (∀ i, d).
-/
theorem i_listGet_empty (i : Nat) (d : String) : listGet ([] : List String) i d = d := rfl

/--
I6: Example: colAt extracts columns.
-/
theorem i_colAt_example : colAt ["a", "b", "c", "d"] 0 2 = ["a", "c"] := by
  native_decide

/--
I7: Example: padRight pads short strings.
-/
theorem i_padRight_example : padRight "abc" 5 = "abc  " := by
  native_decide

/--
I8: Empty input with no header produces "\n".
-/
theorem i_empty_no_header : pr "" 1 1 10 "t" false = "\n" := by
  native_decide

end Lentils.Pr.Logic
