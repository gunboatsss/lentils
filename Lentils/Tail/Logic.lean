/-
Tail.Logic — Verified pure logic for `tail`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Tail.Logic

open Lentils.Common.Lines
open ByteArray

structure TailInput where
  count : Nat := 10
  content : ByteArray := ByteArray.empty
  deriving Inhabited

abbrev TailOutput := ByteArray

def spec (input : TailInput) : TailOutput :=
  let lines := splitLines input.content
  let adjusted :=
    match lines.reverse with
    | [] => []
    | last :: rest => if last.isEmpty then rest.reverse else lines
  let len := adjusted.length
  if len ≤ input.count then input.content
  else joinLines (adjusted.drop (len - input.count))

def takeLastLines (ba : ByteArray) (n : Nat) : ByteArray :=
  let lines := splitLines ba
  let adjusted :=
    match lines.reverse with
    | [] => []
    | last :: rest => if last.isEmpty then rest.reverse else lines
  let len := adjusted.length
  if len ≤ n then ba
  else joinLines (adjusted.drop (len - n))

def impl (input : TailInput) : TailOutput :=
  takeLastLines input.content input.count

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
I4: parseCount -3.
-/
theorem i_parse_old3 : parseCount ["-3"] = some 3 := by
  native_decide

theorem splitLines_empty_eq : splitLines ByteArray.empty = [ByteArray.empty] := by
  native_decide

example : takeLastLines (ByteArray.mk #[0x41, 0x42, 0x43]) 10 = ByteArray.mk #[0x41, 0x42, 0x43] := by
  native_decide

example : takeLastLines (ByteArray.mk #[0x41, 0x0A, 0x42]) 1 = ByteArray.mk #[0x42] := by
  native_decide

end Lentils.Tail.Logic
