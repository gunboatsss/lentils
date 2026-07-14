/-
Nl.Logic — Pure line-numbering logic for `nl`. 0BSD -/
namespace Lentils.Nl.Logic

/--
Number the non-empty lines of input.
Each numbered line gets a 6-column right-justified number followed by a tab and the line content.
Empty lines are passed through unnumbered.
-/
def numberLines (input : String) (startNum : Nat := 1) (incr : Nat := 1) : String :=
  if input.isEmpty then ""
  else
    let lines := input.splitOn "\n"
    -- Drop trailing empty line from final newline
    let lines := match lines with
      | [] => []
      | _ :: _ => if lines.reverse.head? = some "" then (lines.reverse.tail).reverse else lines
    let rec go (remaining : List String) (num : Nat) (acc : List String) : List String :=
      match remaining with
      | [] => acc.reverse
      | line :: rest =>
        if line.isEmpty then
          -- GNU nl outputs 7 spaces for blank lines (no number, no tab)
          go rest num ("       " :: acc)
        else
          let numStr := toString num
          let padding := if numStr.length < 6 then 6 - numStr.length else 0
          let numbered := s!"{String.ofList (List.replicate padding ' ')}{numStr}\t{line}"
          go rest (num + incr) (numbered :: acc)
    String.intercalate "\n" (go lines startNum [])

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- Empty input produces empty output. -/
example : numberLines "" = "" := by
  native_decide

end Lentils.Nl.Logic


