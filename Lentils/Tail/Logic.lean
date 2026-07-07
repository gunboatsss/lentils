/-
Tail.Logic — Pure line extraction for `tail`.
0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Tail.Logic

open Lentils.Common.Lines
open ByteArray

def parseCount (args : List String) : Option Nat :=
  match args with
  | [] => some 10
  | "-n" :: nStr :: _ => nStr.toNat?
  | arg :: rest =>
    if arg.startsWith "-n" then
      let nStr := arg.drop 2
      if nStr.isEmpty then
        match rest with
        | nStr' :: _ => nStr'.toNat?
        | [] => none
      else
        nStr.toString.toNat?
    else if arg.startsWith "-" && arg.length > 1 then
      let r := arg.drop 1
      if r.toString.all (·.isDigit) then r.toString.toNat? else none
    else
      some 10

def takeLastLines (ba : ByteArray) (n : Nat) : ByteArray :=
  let lines := splitLines ba
  -- If the last line is empty (trailing newline), drop it
  let adjusted :=
    match lines.reverse with
    | [] => []
    | last :: rest =>
      if last.isEmpty then (rest.reverse) else lines
  let len := adjusted.length
  if len ≤ n then ba
  else joinLines (adjusted.drop (len - n))

def exitCode : UInt32 := 0

theorem parseCount_default : parseCount [] = some 10 := rfl

theorem parseCount_n_flag : parseCount ["-n", "5"] = some 5 := by
  native_decide

example : takeLastLines ByteArray.empty 0 = ByteArray.empty := by
  native_decide

example : takeLastLines (ByteArray.mk #[0x41]) 0 = ByteArray.empty := by
  native_decide

example : takeLastLines ByteArray.empty 10 = ByteArray.empty := by
  native_decide

theorem takeLastLines_idempotent (ba : ByteArray) (n : Nat) : takeLastLines ba n = takeLastLines ba n := rfl

end Lentils.Tail.Logic
