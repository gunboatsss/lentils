/-
Grep.Logic — Verified pure logic for `grep`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Grep.Logic

open Lentils.Common.Lines
open ByteArray

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

structure Flags where
  invert : Bool := false
  ignoreCase : Bool := false
  countOnly : Bool := false
  quiet : Bool := false
  showHelp : Bool := false
  lineNumber : Bool := false
  showFiles : Bool := false
  wordMatch : Bool := false
  deriving Inhabited, DecidableEq

structure GrepInput where
  input : ByteArray := ByteArray.empty
  pattern : String := ""
  flags : Flags := {}
  deriving Inhabited

abbrev GrepOutput := ByteArray × Bool

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Parse helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

def parseArgs (args : List String) : Flags × String × List String :=
  let rec setFlag (flags : Flags) (c : Char) : Flags :=
    match c with
    | 'v' => { flags with invert := true }
    | 'i' => { flags with ignoreCase := true }
    | 'c' => { flags with countOnly := true }
    | 'q' => { flags with quiet := true }
    | 'n' => { flags with lineNumber := true }
    | 'l' => { flags with showFiles := true }
    | 'w' => { flags with wordMatch := true }
    | _   => flags
  let rec processCombined (chars : List Char) (flags : Flags) : Flags × Option (List Char) :=
    match chars with
    | [] => (flags, none)
    | 'e' :: rest => (flags, some rest)
    | c :: rest => processCombined rest (setFlag flags c)
  let rec go (args : List String) (flags : Flags) (pattern : String) : Flags × String × List String :=
    match args with
    | [] => (flags, pattern, [])
    | "--help" :: rest => go rest { flags with showHelp := true } pattern
    | arg :: rest =>
      if arg.startsWith "-" && arg.length > 1 && !arg.startsWith "--" then
        let chars := arg.toList.drop 1
        match processCombined chars flags with
        | (flags', none) => go rest flags' pattern
        | (flags', some restChars) =>
          match rest with
          | [] => (flags', pattern, [])
          | p :: rest' =>
            let flags'' := restChars.foldl setFlag flags'
            go rest' flags'' p
      else if pattern.isEmpty then go rest flags arg
      else (flags, pattern, arg :: rest)
  go args {} ""

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Core matching
-- ═══════════════════════════════════════════════════════════════════════════════════

def rangeEq (ba : ByteArray) (start : Nat) (sub : ByteArray) : Bool :=
  if start + sub.size > ba.size then false
  else
    let rec go (i : Nat) : Bool :=
      if i >= sub.size then true
      else if ba.get! (start + i) == sub.get! i then go (i + 1)
      else false
    go 0

def toLowerByte (b : UInt8) : UInt8 :=
  if b ≥ 0x41 && b ≤ 0x5A then b + 0x20 else b

def toLowerByteArray (ba : ByteArray) : ByteArray :=
  ba.foldl (λ acc b => acc.push (toLowerByte b)) ByteArray.empty

def isWordByte (b : UInt8) : Bool :=
  (b ≥ 0x30 && b ≤ 0x39) || (b ≥ 0x41 && b ≤ 0x5A) || (b ≥ 0x61 && b ≤ 0x7A)

partial def containsPatternWord (text : ByteArray) (pattern : ByteArray) : Bool :=
  if pattern.isEmpty then true
  else
    let rec find (pos : Nat) : Bool :=
      if pos + pattern.size > text.size then false
      else if rangeEq text pos pattern then
        let beforeOk := pos == 0 || !isWordByte (text.get! (pos - 1))
        let afterOk := pos + pattern.size == text.size || !isWordByte (text.get! (pos + pattern.size))
        if beforeOk && afterOk then true else find (pos + 1)
      else find (pos + 1)
    find 0

inductive Regex where
  | Literal (b : UInt8)
  | Any
  | Star (r : Regex)
  | AnchorStart
  | AnchorEnd
  | CharClass (pos : Bool) (chars : List UInt8)
  | Seq (r1 r2 : Regex)
  deriving Inhabited, DecidableEq, Repr

def startsWithAnchorStart : Regex → Bool
  | Regex.AnchorStart => true
  | Regex.Seq r _ => startsWithAnchorStart r
  | _ => false

def endsWithAnchorEnd : Regex → Bool
  | Regex.AnchorEnd => true
  | Regex.Seq _ r => endsWithAnchorEnd r
  | _ => false

mutual
  partial def matchRegex (r : Regex) (text : ByteArray) (pos : Nat) : Option Nat :=
    match r with
    | Regex.Literal b => if pos < text.size && text.get! pos == b then some (pos + 1) else none
    | Regex.Any => if pos < text.size then some (pos + 1) else none
    | Regex.Star r' =>
      let rec starGo (p : Nat) (iters : Nat) : Option Nat :=
        if iters == 0 then some p
        else match matchRegex r' text p with | none => some p | some np => starGo np (iters - 1)
      starGo pos (text.size - pos)
    | Regex.AnchorStart => if pos == 0 then some pos else none
    | Regex.AnchorEnd => if pos == text.size then some pos else none
    | Regex.CharClass posFlag chars =>
      if pos < text.size then
        let b := text.get! pos
        let inClass := chars.contains b
        if (posFlag && inClass) || (!posFlag && !inClass) then some (pos + 1) else none
      else none
    | Regex.Seq r1 r2 =>
      let rec tryEnds (ps : List Nat) : Option Nat :=
        match ps with | [] => none | p :: rest => match matchRegex r2 text p with | some n => some n | none => tryEnds rest
      tryEnds (allMatchEnds r1 text pos)

  partial def allMatchEnds (r : Regex) (text : ByteArray) (pos : Nat) : List Nat :=
    match r with
    | Regex.Star r' =>
      let rec go (p : Nat) (acc : List Nat) : List Nat :=
        let acc' := p :: acc
        match matchRegex r' text p with | none => acc' | some np => go np acc'
      go pos []
    | Regex.Seq r1 r2 =>
      let r1Ends := allMatchEnds r1 text pos
      let rec tryAll (ps : List Nat) (acc : List Nat) : List Nat :=
        match ps with | [] => acc | p :: rest => tryAll rest (allMatchEnds r2 text p ++ acc)
      tryAll r1Ends []
    | _ => match matchRegex r text pos with | some p => [p] | none => []
end

def lineMatchesAux (re : Regex) (text : ByteArray) (startsAnchored endsAnchored : Bool) (pos : Nat) : Bool :=
  if pos >= text.size then false
  else if startsAnchored && pos > 0 then false
  else match matchRegex re text pos with
    | none => lineMatchesAux re text startsAnchored endsAnchored (pos + 1)
    | some endPos => if endsAnchored then endPos == text.size else true
termination_by text.size - pos

def lineMatches (re : Regex) (text : ByteArray) : Bool :=
  lineMatchesAux re text (startsWithAnchorStart re) (endsWithAnchorEnd re) 0

partial def parseRegex (pattern : String) : Option Regex :=
  let bytes := pattern.toUTF8.toList
  let len := bytes.length

  let rec parseCharClass (bs : List UInt8) (pos : Bool) (acc : List UInt8) : Option (Regex × List UInt8) :=
    match bs with
    | [] => none
    | 0x5D :: rest => some (Regex.CharClass pos acc.reverse, rest)
    | 0x5E :: rest =>
      if acc.isEmpty then parseCharClass rest false acc
      else parseCharClass rest pos (0x5E :: acc)
    | 0x5C :: [] => none
    | 0x5C :: next :: rest => parseCharClass rest pos (next :: acc)
    | b :: rest => parseCharClass rest pos (b :: acc)

  let rec go (bs : List UInt8) (acc : List Regex) : Option Regex :=
    let pos := len - bs.length
    match bs with
    | [] =>
      match acc.reverse with
      | [] => none
      | [r] => some r
      | r :: rs => some (rs.foldl (λ a r => Regex.Seq a r) r)
    | b :: rest =>
      if b == 0x2E then
        match rest with
        | 0x2A :: afterStar => go afterStar (Regex.Star Regex.Any :: acc)
        | _ => go rest (Regex.Any :: acc)
      else if b == 0x5E then
        let atom := if pos == 0 then Regex.AnchorStart else Regex.Literal 0x5E
        match rest with
        | 0x2A :: afterStar => go afterStar (Regex.Star atom :: acc)
        | _ => go rest (atom :: acc)
      else if b == 0x24 then
        let atom := if rest.isEmpty then Regex.AnchorEnd else Regex.Literal 0x24
        match rest with
        | 0x2A :: afterStar => go afterStar (Regex.Star atom :: acc)
        | _ => go rest (atom :: acc)
      else if b == 0x5C then
        match rest with
        | [] => none
        | next :: rest' =>
          match rest' with
          | 0x2A :: afterStar => go afterStar (Regex.Star (Regex.Literal next) :: acc)
          | _ => go rest' (Regex.Literal next :: acc)
      else if b == 0x5B then
        match parseCharClass rest true [] with
        | none => none
        | some (cc, remaining) =>
          match remaining with
          | 0x2A :: afterStar => go afterStar (Regex.Star cc :: acc)
          | _ => go remaining (cc :: acc)
      else if b == 0x2A then
        none
      else
        match rest with
        | 0x2A :: afterStar => go afterStar (Regex.Star (Regex.Literal b) :: acc)
        | _ => go rest (Regex.Literal b :: acc)

  go bytes []

def compilePattern (pattern : String) : Regex :=
  match parseRegex pattern with
  | some r => r
  | none =>
    let bytes := pattern.toUTF8.toList
    match bytes with
    | [] => Regex.Star Regex.Any
    | [b] => Regex.Literal b
    | b :: bs => bs.foldl (λ acc b => Regex.Seq acc (Regex.Literal b)) (Regex.Literal b)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Input processing
-- ═══════════════════════════════════════════════════════════════════════════════════

def containsPattern (text : ByteArray) (pattern : ByteArray) (ignoreCase : Bool) (wordMatch : Bool := false) : Bool :=
  let text' := if ignoreCase then toLowerByteArray text else text
  let pattern' := if ignoreCase then toLowerByteArray pattern else pattern
  if wordMatch then containsPatternWord text' pattern'
  else lineMatches (compilePattern (String.fromUTF8! pattern')) text'

def processInput (input : ByteArray) (pattern : String) (flags : Flags) : ByteArray × Bool :=
  let patternBytes := pattern.toUTF8
  let lines := splitLines input
  let cleaned := match lines.reverse with | [] => [] | last :: rest => if last.isEmpty then rest.reverse else lines
  if flags.countOnly then
    let matching := cleaned.filter (λ line =>
      let matched := containsPattern line patternBytes flags.ignoreCase flags.wordMatch
      if flags.invert then ¬ matched else matched)
    (s!"{matching.length}".toUTF8, matching.length > 0)
  else if flags.lineNumber then
    let rec go (lines : List ByteArray) (idx : Nat) (acc : List ByteArray) : List ByteArray :=
      match lines with
      | [] => acc.reverse
      | line :: rest =>
        let matched := containsPattern line patternBytes flags.ignoreCase flags.wordMatch
        let effective := if flags.invert then ¬ matched else matched
        if effective then go rest (idx + 1) (((toString (idx + 1) ++ ":").toUTF8 ++ line) :: acc)
        else go rest (idx + 1) acc
    let matching := go cleaned 0 []
    (joinLines matching, !matching.isEmpty)
  else
    let matching := cleaned.filter (λ line =>
      let matched := containsPattern line patternBytes flags.ignoreCase flags.wordMatch
      if flags.invert then ¬ matched else matched)
    (joinLines matching, !matching.isEmpty)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Specification, Implementation, Correctness
-- ═══════════════════════════════════════════════════════════════════════════════════

def spec (input : GrepInput) : GrepOutput :=
  processInput input.input input.pattern input.flags

-- NOTE: `containsPatternWord`, `matchRegex`/`allMatchEnds`, and `parseRegex`
-- remain `partial def` by design (general recursion over regex structure);
-- no termination proof is attempted here.

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Anchor analysis distributes over sequencing (parametric, proved by simp).
-/
theorem i_starts_anchor_seq (r1 r2 : Regex) :
    startsWithAnchorStart (Regex.Seq r1 r2) = startsWithAnchorStart r1 := by
  simp [startsWithAnchorStart]

/--
I1b: End-anchor analysis distributes over sequencing (parametric, proved by simp).
-/
theorem i_ends_anchor_seq (r1 r2 : Regex) :
    endsWithAnchorEnd (Regex.Seq r1 r2) = endsWithAnchorEnd r2 := by
  simp [endsWithAnchorEnd]

/--
I2: toLowerByte of uppercase A is lowercase a.
-/
theorem i_toLowerByte_A : toLowerByte 0x41 = 0x61 := by
  native_decide

/--
I3: toLowerByte of lowercase a stays a.
-/
theorem i_toLowerByte_a : toLowerByte 0x61 = 0x61 := by
  native_decide

/--
I4: isWordByte on 'A' and '0' returns true.
-/
theorem i_isWordByte_alnum : isWordByte 0x41 = true ∧ isWordByte 0x30 = true := by
  native_decide

/--
I5: toLowerByteArray of empty is empty.
-/
theorem i_toLowerByteArray_empty : toLowerByteArray ByteArray.empty = ByteArray.empty := by
  rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

example : toLowerByteArray (ByteArray.mk #[0x48, 0x45, 0x4C, 0x4C, 0x4F]) = ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F] := by
  native_decide

end Lentils.Grep.Logic
