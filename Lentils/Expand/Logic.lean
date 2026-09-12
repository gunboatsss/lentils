/-
Expand.Logic — Verified pure logic for `expand`. 0BSD
-/

namespace Lentils.Expand.Logic

structure ExpandInput where
  input : String := ""
  tabSize : Nat := 8
  deriving Inhabited

abbrev ExpandOutput := String

def expandLine (line : String) (tabSize : Nat := 8) : String :=
  let rec go (chars : List Char) (col : Nat) (acc : List Char) : List Char :=
    match chars with
    | [] => acc.reverse
    | '\t' :: rest =>
      let spacesNeeded := tabSize - (col % tabSize)
      let spaces := List.replicate spacesNeeded ' '
      go rest (col + spacesNeeded) (spaces ++ acc)
    | c :: rest => go rest (col + 1) (c :: acc)
  String.ofList (go line.toList 0 [])

def expand (input : String) (tabSize : Nat := 8) : String :=
  let lines := input.splitOn "\n"
  let expanded := lines.map (λ l => expandLine l tabSize)
  String.intercalate "\n" expanded

def spec (input : ExpandInput) : ExpandOutput :=
  expand input.input input.tabSize

/--
I2: Empty input → empty output for tab size 8.
-/
theorem i_empty : expand "" 8 = "" := by
  native_decide

/--
I3: A line with no tabs is unchanged.
-/
theorem i_no_tabs : expandLine "hello" 8 = "hello" := by
  native_decide

/--
I4: A tab at start expands to 8 spaces.
-/
theorem i_tab_expands : expandLine "\t" 8 = "        " := by
  native_decide

/--
I5: Single-line inputs expand line-wise (parametric, proved by simp).
-/
theorem i_expand_single (l : String) (tabSize : Nat)
    (h : l.splitOn "\n" = [l]) :
    expand l tabSize = expandLine l tabSize := by
  simp [expand, h]

example : expand "" 8 = "" := i_empty
example : expandLine "hello" 8 = "hello" := i_no_tabs
example : expandLine "\thello" 8 = "        hello" := by native_decide

end Lentils.Expand.Logic
