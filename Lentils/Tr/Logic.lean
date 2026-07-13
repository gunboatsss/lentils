/-
Tr.Logic — Pure character translation for `tr`. 0BSD
-/

namespace Lentils.Tr.Logic

open ByteArray

inductive Mode | translate | delete | squeeze deriving Inhabited, DecidableEq

/-- Expand ranges in a tr set string. E.g., "a-z0-9" → "abcdefghijklmnopqrstuvwxyz0123456789".
    - `c1-c2` where c1 and c2 are alphanumeric and c1 ≤ c2 → range
    - `-` at start or end of the set → literal
    - Everything else → literal -/
def expandRanges (s : String) : String :=
  let rec go (chars : List Char) (acc : List Char) : List Char :=
    match chars with
    | [] => acc.reverse
    | '-' :: rest => go rest ('-' :: acc)  -- dash at start: literal
    | [c] => go [] (c :: acc)
    | c1 :: '-' :: c2 :: rest =>
      let rangeOk :=
        (c1.isAlpha || c1.isDigit) && (c2.isAlpha || c2.isDigit) && c1 ≤ c2
      if rangeOk then
        let start := c1.toNat
        let stop  := c2.toNat
        let range := (List.range (stop - start + 1)).map (λ i => Char.ofNat (start + i))
        go rest (range.reverse ++ acc)
      else
        -- Not a valid range; treat all three as literals
        go (c2 :: rest) ('-' :: c1 :: acc)
    | c :: rest => go rest (c :: acc)
  String.ofList (go s.toList [])

-- ASCII-lower byte: same as toLowerByte in Grep but duplicated here for independence
def toLowerByte (b : UInt8) : UInt8 :=
  if b ≥ 0x41 && b ≤ 0x5A then b + 0x20 else b

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
def deleteComplement (input : ByteArray) (set1 : String) : ByteArray :=
  input.foldl (λ acc b =>
    if byteInString b set1 then acc.push b else acc
  ) ByteArray.empty

-- Complement squeeze: squeeze runs of bytes NOT in set1
def squeezeComplement (input : ByteArray) (set1 : String) : ByteArray :=
  let rec go (ba : ByteArray) (i : Nat) (prevWasMatch : Bool) (acc : ByteArray) : ByteArray :=
    if i >= ba.size then acc
    else
      let b := ba.get! i
      if ¬ byteInString b set1 then
        if prevWasMatch then go ba (i + 1) true acc
        else go ba (i + 1) true (acc.push b)
      else
        go ba (i + 1) false (acc.push b)
    termination_by ba.size - i
  go input 0 false ByteArray.empty

private def processCombined (chars : List Char) (mode : Mode) (complement : Bool) (needsArg : Bool)
    : Mode × Bool × Bool :=
  match chars with
  | [] => (mode, complement, needsArg)
  | 'c' :: rest => processCombined rest mode true needsArg
  | 'C' :: rest => processCombined rest mode true needsArg
  | 'd' :: rest => processCombined rest Mode.delete complement true
  | 's' :: rest => processCombined rest Mode.squeeze complement true
  | _ :: rest => processCombined rest mode complement needsArg

private def go (args : List String) (mode : Mode) (complement : Bool) : Mode × Bool × String × String :=
  match args with
  | [] => (mode, complement, "", "")
  | arg :: rest =>
    if arg.startsWith "-" && arg.length > 1 && !arg.startsWith "--" then
      let chars := arg.toList.drop 1
      -- Only process as flags if at least one known flag char is present
      let hasFlags := chars.any (λ c => c ∈ ['c', 'C', 'd', 's'])
      if hasFlags then
        let (mode', complement', needsArg) := processCombined chars mode complement false
        if needsArg then
          match rest with
          | set1 :: _ => (mode', complement', set1, "")
          | [] => (mode', complement', "", "")
        else
          go rest mode' complement'
      else
        -- No recognized flags: treat as positional set argument
        match mode with
        | Mode.translate =>
          match rest with
          | set2 :: _ => (mode, complement, arg, set2)
          | [] => (mode, complement, arg, "")
        | _ => (mode, complement, arg, "")
    else
      match mode with
      | Mode.translate =>
        match rest with
        | set2 :: _ => (mode, complement, arg, set2)
        | [] => (mode, complement, arg, "")
      | _ => (mode, complement, arg, "")

def parseArgs (args : List String) : Mode × Bool × String × String :=
  let (mode, complement, set1, set2) := go args Mode.translate false
  (mode, complement, expandRanges set1, expandRanges set2)

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

theorem parseArgs_delete : parseArgs ["-d", "abc"] = (Mode.delete, false, "abc", "") := by
  native_decide

theorem parseArgs_translate : parseArgs ["abc", "ABC"] = (Mode.translate, false, "abc", "ABC") := by
  native_decide

theorem parseArgs_complement : (parseArgs ["-c", "abc", "ABC"]).2.1 = true := by
  native_decide

end Lentils.Tr.Logic
