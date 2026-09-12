/-
Unexpand.Logic — Verified pure logic for `unexpand`. 0BSD
-/

namespace Lentils.Unexpand.Logic

structure UnexpandInput where
  input : String := ""
  tabSize : Nat := 8
  deriving Inhabited

abbrev UnexpandOutput := String

def unexpandLine (line : String) (tabSize : Nat := 8) : String :=
  let chars := line.toList
  let len := chars.length
  let rec go (i : Nat) (spaceRun : Nat) (acc : List Char) : List Char :=
    if i ≥ len then List.replicate spaceRun ' ' ++ acc.reverse
    else
      match chars.drop i with
      | ' ' :: _ =>
        let spaceRun' := spaceRun + 1
        if spaceRun' = tabSize then go (i + 1) 0 ('\t' :: acc)
        else go (i + 1) spaceRun' acc
      | c :: _ =>
        let spaces := List.replicate spaceRun ' '
        go (i + 1) 0 (c :: spaces ++ acc)
      | [] => List.replicate spaceRun ' ' ++ acc.reverse
    termination_by len - i
  String.ofList (go 0 0 [])

def unexpand (input : String) (tabSize : Nat := 8) : String :=
  let lines := input.splitOn "\n"
  let converted := lines.map (λ l => unexpandLine l tabSize)
  String.intercalate "\n" converted

def spec (input : UnexpandInput) : UnexpandOutput :=
  unexpand input.input input.tabSize

/--
`unexpand` maps `unexpandLine` over newline-split input (parametric over all inputs).
-/
theorem unexpand_unfold (s : String) (n : Nat) :
    unexpand s n = String.intercalate "\n" ((s.splitOn "\n").map (λ l => unexpandLine l n)) := by
  simp [unexpand]

/--
I2: Empty input → empty output at tab size 8.
-/
theorem i_empty : unexpand "" 8 = "" := by
  native_decide

/--
I3: A line with no leading spaces is unchanged.
-/
theorem i_no_leading_spaces : unexpandLine "hello" 8 = "hello" := by
  native_decide

/--
I4: 8 leading spaces become a tab.
-/
theorem i_eight_spaces_to_tab : unexpandLine "        " 8 = "\t" := by
  native_decide

example : unexpand "" 8 = "" := i_empty
example : unexpandLine "hello" 8 = "hello" := i_no_leading_spaces
example : unexpandLine "        hello" 8 = "\thello" := by native_decide

end Lentils.Unexpand.Logic
