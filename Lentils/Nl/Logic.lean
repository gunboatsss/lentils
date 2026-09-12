/-
Nl.Logic — Verified pure logic for `nl`. 0BSD
-/

namespace Lentils.Nl.Logic

structure NlInput where
  input : String := ""
  startNum : Nat := 1
  increment : Nat := 1
  deriving Inhabited

abbrev NlOutput := String

def numberLines (input : String) (startNum : Nat := 1) (incr : Nat := 1) : String :=
  if input.isEmpty then ""
  else
    let lines := input.splitOn "\n"
    let lines := match lines with
      | [] => []
      | _ :: _ => if lines.reverse.head? = some "" then (lines.reverse.tail).reverse else lines
    let rec go (remaining : List String) (num : Nat) (acc : List String) : List String :=
      match remaining with
      | [] => acc.reverse
      | line :: rest =>
        if line.isEmpty then go rest num ("       " :: acc)
        else
          let numStr := toString num
          let padding := if numStr.length < 6 then 6 - numStr.length else 0
          let numbered := s!"{String.ofList (List.replicate padding ' ')}{numStr}\t{line}"
          go rest (num + incr) (numbered :: acc)
    String.intercalate "\n" (go lines startNum [])

def spec (input : NlInput) : NlOutput :=
  numberLines input.input input.startNum input.increment

/--
I1: Empty input produces empty output.
-/
theorem i_empty : numberLines "" 1 1 = "" := rfl

/--
I3: Empty line is replaced by 7 spaces.
-/
theorem i_blank_line : numberLines "\n" 1 1 = "       " := by
  native_decide

/--
I4: A single non-empty line at default settings produces "     1\t{line}".
-/
theorem i_single_line_example : numberLines "a" 1 1 = "     1\ta" := by
  native_decide

/--
I5: List.replicate n spaces produces length n (∀ n).
-/
theorem i_replicate_spaces_len (n : Nat) : (String.ofList (List.replicate n ' ')).length = n := by
  simp

/--
I6: Two non-empty lines produce sequential numbering.
-/
theorem i_two_lines : numberLines "a\nb" 1 1 = "     1\ta\n     2\tb" := by
  native_decide

/--
I7: Start at 5 with increment 3.
-/
theorem i_start_5_incr_3 : numberLines "a\nb" 5 3 = "     5\ta\n     8\tb" := by
  native_decide

end Lentils.Nl.Logic
