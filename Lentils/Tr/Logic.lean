/-
Tr.Logic — Pure character translation for `tr`. 0BSD
-/

namespace Lentils.Tr.Logic

open ByteArray

inductive Mode | translate | delete | squeeze deriving Inhabited

def byteInString (b : UInt8) (chars : String) : Bool :=
  chars.toList.any (λ c => c.toUInt8 == b)

def indexOfByte (b : UInt8) (chars : String) : Option Nat :=
  let rec go (cs : List Char) (i : Nat) : Option Nat :=
    match cs with
    | [] => none
    | c :: cs => if c.toUInt8 == b then some i else go cs (i + 1)
  go chars.toList 0

def charAt (cs : List Char) (idx : Nat) : Option Char :=
  match cs, idx with
  | [], _ => none
  | c :: _, 0 => some c
  | _ :: rest, n+1 => charAt rest n

def translateByte (set1 : String) (set2Chars : List Char) (lastChar : Char) (b : UInt8) : UInt8 :=
  match indexOfByte b set1 with
  | some idx =>
    if idx < set2Chars.length then
      match charAt set2Chars idx with
      | some c => c.toUInt8
      | none => lastChar.toUInt8
    else
      lastChar.toUInt8
  | none => b

def translate (input : ByteArray) (set1 : String) (set2 : String) : ByteArray :=
  let set2Chars := set2.toList
  let lastChar :=
    match set2Chars with
    | [] => '?'
    | _ =>
      let rec last (cs : List Char) : Char :=
        match cs with
        | [] => '?'
        | [x] => x
        | _ :: rest => last rest
      last set2Chars
  input.foldl (λ acc b => acc.push (translateByte set1 set2Chars lastChar b)) ByteArray.empty

def delete (input : ByteArray) (chars : String) : ByteArray :=
  input.foldl (λ acc b =>
    if byteInString b chars then acc else acc.push b
  ) ByteArray.empty

def squeeze (input : ByteArray) (chars : String) : ByteArray :=
  let rec go (ba : ByteArray) (i : Nat) (prevWasMatch : Bool) (acc : ByteArray) : ByteArray :=
    if i >= ba.size then acc
    else
      let b := ba.get! i
      if byteInString b chars then
        if prevWasMatch then go ba (i + 1) true acc
        else go ba (i + 1) true (acc.push b)
      else
        go ba (i + 1) false (acc.push b)
  go input 0 false ByteArray.empty


-- Complement delete: delete bytes NOT in set1 (keep only bytes IN set1)
partial def deleteComplement (input : ByteArray) (set1 : String) : ByteArray :=
  input.foldl (λ acc b =>
    if byteInString b set1 then acc.push b else acc
  ) ByteArray.empty

-- Complement squeeze: squeeze runs of bytes NOT in set1
partial def squeezeComplement (input : ByteArray) (set1 : String) : ByteArray :=
  let rec go (ba : ByteArray) (i : Nat) (prevWasMatch : Bool) (acc : ByteArray) : ByteArray :=
    if i >= ba.size then acc
    else
      let b := ba.get! i
      if ¬ byteInString b set1 then
        if prevWasMatch then go ba (i + 1) true acc
        else go ba (i + 1) true (acc.push b)
      else
        go ba (i + 1) false (acc.push b)
  go input 0 false ByteArray.empty

def parseArgs (args : List String) : Mode × Bool × String × String :=
  let rec go (args : List String) (mode : Mode) (complement : Bool) : Mode × Bool × String × String :=
    match args with
    | [] => (mode, complement, "", "")
    | "-c" :: rest => go rest mode true
    | "-C" :: rest => go rest mode true
    | "-d" :: set1 :: _ => (Mode.delete, complement, set1, "")
    | "-s" :: set1 :: _ => (Mode.squeeze, complement, set1, "")
    | set1 :: set2 :: _ => (Mode.translate, complement, set1, set2)
    | set1 :: _ => (mode, complement, set1, "")
  go args Mode.translate false

def processInput (input : ByteArray) (mode : Mode) (complement : Bool) (set1 : String) (set2 : String) : ByteArray :=
  if complement then
    match mode with
    | Mode.delete => deleteComplement input set1
    | Mode.squeeze => squeezeComplement input set1
    | Mode.translate => translate input set1 set2
  else
    match mode with
    | Mode.translate => translate input set1 set2
    | Mode.delete => delete input set1
    | Mode.squeeze => squeeze input set1

-- Theorems

theorem delete_empty : delete ByteArray.empty "" = ByteArray.empty := rfl

theorem delete_none : delete (ByteArray.mk #[0x41, 0x42]) "" = ByteArray.mk #[0x41, 0x42] := by
  native_decide

theorem delete_one : delete (ByteArray.mk #[0x41, 0x42, 0x43]) "B" = ByteArray.mk #[0x41, 0x43] := by
  native_decide

theorem translate_identity : translate ByteArray.empty "" "" = ByteArray.empty := rfl

theorem translate_upper : translate (ByteArray.mk #[0x61, 0x62, 0x63]) "abc" "ABC" = ByteArray.mk #[0x41, 0x42, 0x43] := by
  native_decide

theorem squeeze_empty : squeeze ByteArray.empty "" = ByteArray.empty := by
  native_decide

theorem squeeze_single : squeeze (ByteArray.mk #[0x41, 0x41, 0x41, 0x42]) "A" = ByteArray.mk #[0x41, 0x42] := by
  native_decide

theorem parseArgs_delete : parseArgs ["-d", "abc"] = (Mode.delete, false, "abc", "") := rfl

theorem parseArgs_translate : parseArgs ["abc", "ABC"] = (Mode.translate, false, "abc", "ABC") := rfl

theorem parseArgs_complement : (parseArgs ["-c", "abc", "ABC"]).2.1 = true := rfl

end Lentils.Tr.Logic
