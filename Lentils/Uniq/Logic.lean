/-
Uniq.Logic — Pure deduplication for `uniq`. 0BSD -/
import Lentils.Common.Lines
namespace Lentils.Uniq.Logic
open Lentils.Common.Lines
open ByteArray

inductive Mode | normal | unique | repeated deriving Inhabited

def parseArgs (args : List String) : Mode × List String :=
  let rec go (args : List String) (mode : Mode) : Mode × List String :=
    match args with | [] => (mode, []) | "-d" :: rest => go rest Mode.repeated | "-u" :: rest => go rest Mode.unique | arg :: rest => if arg.startsWith "-" && arg ≠ "-" then go rest mode else (mode, arg :: rest)
  go args Mode.normal

def groupAdjacentDuplicates (lines : List ByteArray) : List (ByteArray × Nat) :=
  let rec go (lines : List ByteArray) (current : ByteArray) (count : Nat) : List (ByteArray × Nat) :=
    match lines with | [] => [(current, count)] | l :: rest => if l = current then go rest current (count + 1) else (current, count) :: go rest l 1
  match lines with | [] => [] | first :: rest => go rest first 1

def processLines (input : ByteArray) (mode : Mode) : ByteArray :=
  let lines := splitLines input
  let groups := groupAdjacentDuplicates lines
  let filtered : List ByteArray :=
    match mode with
    | Mode.normal => groups.map (λ (h, _) => h)
    | Mode.unique => (groups.filter (λ (_, c) => c = 1)).map (λ (h, _) => h)
    | Mode.repeated => (groups.filter (λ (_, c) => c > 1)).map (λ (h, _) => h)
  joinLines filtered

theorem processLines_empty : processLines ByteArray.empty Mode.normal = ByteArray.empty := by
  unfold processLines splitLines joinLines groupAdjacentDuplicates
  native_decide

end Lentils.Uniq.Logic
