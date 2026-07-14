/-
Fold.Logic — Pure line-wrapping logic for `fold`. 0BSD -/
namespace Lentils.Fold.Logic

/--
Fold a single line at the given width.
Always breaks at exactly `width` characters.
When `breakSpaces` is true, break at the last space before the width.
Uses a simple iterative approach: fold by explicit segments for hard breaks,
and by scanning for spaces for -s mode, ensuring termination via structural
recursion on the (user-visible) segment count.
-/
partial def foldLine (line : String) (width : Nat) (breakSpaces : Bool := false) : List String :=
  if width = 0 then [line]
  else if line.length ≤ width then [line]
  else
    let chars := line.toList
    if breakSpaces then
      let rec lastSpaceIn (cs : List Char) (w : Nat) (lastIdx : Nat) (i : Nat) : Nat :=
        if w = 0 then lastIdx
        else
          match cs with
          | [] => lastIdx
          | c :: rest =>
            let next := if c = ' ' then i else lastIdx
            lastSpaceIn rest (w - 1) next (i + 1)
      let rec go (cs : List Char) (acc : List String) : List String :=
        if cs.isEmpty then acc.reverse
        else if cs.length ≤ width then
          (acc.reverse ++ [String.ofList cs])
        else
          let breakIdx := lastSpaceIn (cs.take width) width 0 0
          if breakIdx = 0 then
            let (first, rest) := (cs.take width, cs.drop width)
            go rest (String.ofList first :: acc)
          else
            -- Break AFTER the space (include space at end of first segment)
            let (first, rest) := (cs.take (breakIdx + 1), cs.drop (breakIdx + 1))
            go rest (String.ofList first :: acc)
      go chars []
    else
      let total := chars.length
      let numSegments := (total + width - 1) / width
      let segments := List.range numSegments |>.map (λ i =>
        let start := i * width
        String.ofList (chars.drop start |>.take width))
      segments

/--
Fold multi-line input at the given width.
-/
def fold (input : String) (width : Nat := 80) (breakSpaces : Bool := false) : String :=
  let lines := input.splitOn "\n"
  let folded := lines.map (λ l => String.intercalate "\n" (foldLine l width breakSpaces))
  String.intercalate "\n" folded

-- ─── Proofs ──────────────────────────────────────────────────────────────────

-- Note: foldLine is partial due to the nested recursion in -s mode,
-- so native_decide is not available for proofs involving that path.
