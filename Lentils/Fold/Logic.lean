/-
Fold.Logic — Pure line-wrapping logic for `fold`. 0BSD -/
namespace Lentils.Fold.Logic

/--
Fold a single line at the given width.
Always breaks at exactly `width` characters.
-/
def foldLine (line : String) (width : Nat) : List String :=
  if width = 0 then [line]
  else
    let chars := line.toList
    let total := chars.length
    let numSegments := (total + width - 1) / width  -- ceiling division
    let segments := List.range numSegments |>.map (λ i =>
      let start := i * width
      String.ofList (chars.drop start |>.take width))
    segments

/--
Fold multi-line input at the given width.
-/
def fold (input : String) (width : Nat := 80) : String :=
  let lines := input.splitOn "\n"
  let folded := lines.map (λ l => String.intercalate "\n" (foldLine l width))
  String.intercalate "\n" folded

end Lentils.Fold.Logic
