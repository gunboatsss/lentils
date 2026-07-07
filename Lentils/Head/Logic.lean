/-
Head.Logic — Pure line extraction for `head`. 0BSD -/
import Lentils.Common.Lines
namespace Lentils.Head.Logic
open Lentils.Common.Lines
open ByteArray

def parseCount (args : List String) : Option Nat :=
  match args with
  | [] => some 10
  | "-n" :: nStr :: _ => nStr.toNat?
  | arg :: rest =>
    if arg.startsWith "-n" then
      let nStr := arg.drop 2
      if nStr.isEmpty then match rest with | nStr' :: _ => nStr'.toNat? | [] => none
      else nStr.toString.toNat?
    else if arg.startsWith "-" && arg.length > 1 then
      let r := arg.drop 1
      if r.toString.all (·.isDigit) then r.toString.toNat? else none
    else some 10

def takeLines (ba : ByteArray) (n : Nat) : ByteArray :=
  joinLines ((splitLines ba).take n)

theorem parseCount_default : parseCount [] = some 10 := rfl
theorem parseCount_n_flag : parseCount ["-n", "5"] = some 5 := by native_decide
theorem parseCount_old_style : parseCount ["-5"] = some 5 := by native_decide

theorem takeLines_zero (ba : ByteArray) : takeLines ba 0 = ByteArray.empty := by
  unfold takeLines splitLines joinLines Lentils.Common.Bytes.joinWithNewline
    Lentils.Common.Bytes.joinWith
  simp

example : takeLines ByteArray.empty 10 = ByteArray.empty := by native_decide
example : takeLines (ByteArray.mk #[0x41, 0x42, 0x0A, 0x43]) 1 = ByteArray.mk #[0x41, 0x42] := by native_decide

end Lentils.Head.Logic
