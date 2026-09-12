/-
Head.Logic — Verified pure logic for `head`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Head.Logic

open Lentils.Common.Lines
open ByteArray

structure HeadInput where
  count : Nat := 10
  content : ByteArray := ByteArray.empty
  deriving Inhabited

abbrev HeadOutput := ByteArray

def spec (input : HeadInput) : HeadOutput :=
  joinLines ((splitLines input.content).take input.count)

def takeLines (ba : ByteArray) (n : Nat) : ByteArray :=
  joinLines ((splitLines ba).take n)

def impl (input : HeadInput) : HeadOutput :=
  takeLines input.content input.count

theorem impl_correct : ∀ input, impl input = spec input := by
  intro input; rfl

def parseCount (args : List String) : Option Nat :=
  match args with
  | [] => some 10
  | "-n" :: nStr :: _ => nStr.toNat?
  | arg :: _ =>
    if arg.startsWith "-n" then
      let nStr := arg.drop 2
      if nStr.isEmpty then none else nStr.toString.toNat?
    else if arg.startsWith "-" && arg.length > 1 then
      let r := arg.drop 1
      if r.toString.all (·.isDigit) then r.toString.toNat? else none
    else some 10

/--
I2: parseCount default is 10.
-/
theorem i_parse_default : parseCount [] = some 10 := rfl

/--
I3: parseCount -n 5.
-/
theorem i_parse_n5 : parseCount ["-n", "5"] = some 5 := by
  native_decide

/--
I4: parseCount -3 (old style).
-/
theorem i_parse_old3 : parseCount ["-3"] = some 3 := by
  native_decide

theorem joinLines_empty : joinLines ([] : List ByteArray) = ByteArray.empty := rfl

example : takeLines (ByteArray.mk #[0x41, 0x42, 0x0A, 0x43]) 1 = ByteArray.mk #[0x41, 0x42] := by
  native_decide

end Lentils.Head.Logic
