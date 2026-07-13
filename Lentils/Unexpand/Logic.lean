/-
Unexpand.Logic — Pure space-to-tab conversion logic for `unexpand`. 0BSD -/
namespace Lentils.Unexpand.Logic

/--
Convert leading spaces to tabs in a single line.
Default tab stop is 8.
Uses a simple iterative approach with explicit indices.
Marked partial because Lean can't prove termination for the space-counting loop.
-/
partial def unexpandLine (line : String) (tabSize : Nat := 8) : String :=
  let chars := line.toList
  let len := chars.length
  let rec go (i : Nat) (acc : List Char) : List Char :=
    if i ≥ len then acc.reverse
    else
      match chars.drop i with
      | ' ' :: _ =>
        -- Count consecutive spaces
        let rec countSpaces (j : Nat) (n : Nat) : Nat × Nat :=
          match chars.drop j with
          | ' ' :: _ => countSpaces (j + 1) (n + 1)
          | _ => (n, j)
        let (spaceCount, nextIdx) := countSpaces i 0
        let tabs := spaceCount / tabSize
        let remainingSpaces := spaceCount % tabSize
        let tabChars := List.replicate tabs '\t' ++ List.replicate remainingSpaces ' '
        go nextIdx (tabChars.reverse ++ acc)
      | c :: _ =>
        go (i + 1) (c :: acc)
      | [] => acc.reverse
  String.ofList (go 0 [])

/--
Convert leading spaces to tabs in multi-line input.
-/
def unexpand (input : String) (tabSize : Nat := 8) : String :=
  let lines := input.splitOn "\n"
  let converted := lines.map (λ l => unexpandLine l tabSize)
  String.intercalate "\n" converted

end Lentils.Unexpand.Logic
