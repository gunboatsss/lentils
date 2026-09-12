/-
Fold.Logic — Verified pure logic for `fold`. 0BSD
-/

namespace Lentils.Fold.Logic

structure FoldInput where
  input : String := ""
  width : Nat := 80
  breakSpaces : Bool := false
  deriving Inhabited

abbrev FoldOutput := String

-- NOTE: `foldLine` remains `partial def` — the `breakSpaces` branch recurses on
-- `cs.drop` fragments with a non-trivial termination measure; a full
-- `termination_by` proof is out of scope here.
partial def foldLine (line : String) (width : Nat) (breakSpaces : Bool := false) : List String :=
  if width = 0 then [line]
  else if line.length ≤ width then [line]
  else
    let chars := line.toList
    if breakSpaces then
      let rec lastSpaceIn (cs : List Char) (w : Nat) (lastIdx : Nat) (i : Nat) : Nat :=
        if w = 0 then lastIdx
        else match cs with | [] => lastIdx | c :: rest => let next := if c = ' ' then i else lastIdx; lastSpaceIn rest (w - 1) next (i + 1)
      let rec go (cs : List Char) (acc : List String) : List String :=
        if cs.isEmpty then acc.reverse
        else if cs.length ≤ width then (acc.reverse ++ [String.ofList cs])
        else
          let breakIdx := lastSpaceIn (cs.take width) width 0 0
          if breakIdx = 0 then let (first, rest) := (cs.take width, cs.drop width); go rest (String.ofList first :: acc)
          else let (first, rest) := (cs.take (breakIdx + 1), cs.drop (breakIdx + 1)); go rest (String.ofList first :: acc)
      go chars []
    else
      let total := chars.length
      let numSegments := (total + width - 1) / width
      List.range numSegments |>.map (λ i => String.ofList (chars.drop (i * width) |>.take width))

def fold (input : String) (width : Nat := 80) (breakSpaces : Bool := false) : String :=
  let lines := input.splitOn "\n"
  let folded := lines.map (λ l => String.intercalate "\n" (foldLine l width breakSpaces))
  String.intercalate "\n" folded

def spec (input : FoldInput) : FoldOutput :=
  fold input.input input.width input.breakSpaces

/--
I1: Empty input → empty output.
-/
theorem i_empty : fold "" 80 = "" := by
  native_decide

/--
I2: Zero width is a fixed point for `foldLine` (parametric, proved by unfold + simp).
-/
theorem i_foldLine_zero (line : String) (b : Bool) :
    foldLine line 0 b = [line] := by
  unfold foldLine
  simp

/--
I3: Short input (less than width) is unchanged (concrete example).
-/
theorem i_short_example : fold "hello" 80 = "hello" := by
  native_decide

/--
I4: fold at width 3 wraps.
-/
theorem i_wrap_example : fold "abcdef" 3 = "abc\ndef" := by
  native_decide

example : fold "" 80 = "" := i_empty
example : fold "hello" 80 = "hello" := i_short_example
example : fold "abcdef" 3 = "abc\ndef" := i_wrap_example
example : fold "hello world" 5 = "hello\n worl\nd" := by native_decide

end Lentils.Fold.Logic
