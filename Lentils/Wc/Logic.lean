/-
Wc.Logic — Pure counting for `wc`. 0BSD -/
import Lentils.Common.Bytes
namespace Lentils.Wc.Logic
open ByteArray
open Lentils.Common.Bytes

structure Flags where
  lines : Bool := false
  words : Bool := false
  bytes : Bool := false
  deriving Inhabited

def defaultFlags : Flags := { lines := true, words := true, bytes := true }

def parseArgs (args : List String) : Flags × List String :=
  let rec go (args : List String) (flags : Flags) : Flags × List String :=
    match args with
    | [] => (flags, [])
    | arg :: rest =>
      if arg.startsWith "-" && arg ≠ "-" then
        let flagChars := arg.drop 1
        let newFlags := flagChars.toString.foldl (λ (f : Flags) c =>
          match c with
          | 'l' => { f with lines := true }
          | 'w' => { f with words := true }
          | 'c' => { f with bytes := true }
          | _   => f) flags
        go rest newFlags
      else (flags, arg :: rest)
  let (flags, filenames) := go args {}
  let flags := if !flags.lines && !flags.words && !flags.bytes then defaultFlags else flags
  (flags, filenames)

def countLines (ba : ByteArray) : Nat := countNewlines ba

def countWords (ba : ByteArray) : Nat :=
  let rec go (i : Nat) (inWord : Bool) (count : Nat) : Nat :=
    if i < ba.size then
      let b := ba.get! i
      let isSpace := b == 0x20 || b == 0x09 || b == 0x0A || b == 0x0D || b == 0x0B || b == 0x0C
      if isSpace then go (i + 1) false count
      else if inWord then go (i + 1) true count
      else go (i + 1) true (count + 1)
    else count
  go 0 false 0

def countBytes (ba : ByteArray) : Nat := ba.size

def formatCounts (lines words bytes : Nat) (filename : String) (flags : Flags) : String :=
  let parts : List String := Id.run do
    let mut result : List String := []
    if flags.lines then result := result ++ [toString lines]
    if flags.words then result := result ++ [toString words]
    if flags.bytes then result := result ++ [toString bytes]
    if !filename.isEmpty then result := result ++ [filename]
    result
  String.intercalate " " parts ++ "\n"

theorem countLines_empty : countLines ByteArray.empty = 0 := rfl
theorem countWords_empty : countWords ByteArray.empty = 0 := by native_decide
example : countLines (ByteArray.mk #[0x0A]) = 1 := rfl

end Lentils.Wc.Logic
