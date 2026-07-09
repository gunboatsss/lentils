/-
Grep.Logic — Pure pattern matching for `grep`. 0BSD
-/

import Lentils.Common.Lines

namespace Lentils.Grep.Logic

open Lentils.Common.Lines
open ByteArray

structure Flags where
  invert : Bool := false
  ignoreCase : Bool := false
  countOnly : Bool := false
  quiet : Bool := false
  showHelp : Bool := false
deriving Inhabited, DecidableEq

def parseArgs (args : List String) : Flags × String × List String :=
  let rec setFlag (flags : Flags) (c : Char) : Flags :=
    match c with
    | 'v' => { flags with invert := true }
    | 'i' => { flags with ignoreCase := true }
    | 'c' => { flags with countOnly := true }
    | 'q' => { flags with quiet := true }
    | _   => flags
  let rec go (args : List String) (flags : Flags) (pattern : String) : Flags × String × List String :=
    match args with
    | [] => (flags, pattern, [])
    | "-v" :: rest => go rest { flags with invert := true } pattern
    | "-i" :: rest => go rest { flags with ignoreCase := true } pattern
    | "-c" :: rest => go rest { flags with countOnly := true } pattern
    | "-q" :: rest => go rest { flags with quiet := true } pattern
    | "-e" :: p :: rest => go rest flags p
    | "--help" :: rest => go rest { flags with showHelp := true } pattern
    | arg :: rest =>
      if arg.startsWith "-" && arg ≠ "-" && arg ≠ "-e" then
        -- Handle combined short flags like "-ci" = "-c" + "-i"
        if arg.length > 2 && !arg.startsWith "--" then
          let chars := arg.toList.drop 1
          let flags' := chars.foldl setFlag flags
          go rest flags' pattern
        else
          go rest flags pattern
      else if pattern.isEmpty then go rest flags arg
      else (flags, pattern, arg :: rest)
  go args {} ""

def rangeEq (ba : ByteArray) (start : Nat) (sub : ByteArray) : Bool :=
  if start + sub.size > ba.size then false
  else
    let rec go (i : Nat) : Bool :=
      if i >= sub.size then true
      else if ba.get! (start + i) == sub.get! i then go (i + 1)
      else false
    go 0

-- ─── ASCII case folding ───────────────────────────────────────────────────────

def toLowerByte (b : UInt8) : UInt8 :=
  if b ≥ 0x41 && b ≤ 0x5A then b + 0x20 else b

/-- Lowercase every byte in a ByteArray (ASCII only: 'A'-'Z' → 'a'-'z'). -/
def toLowerByteArray (ba : ByteArray) : ByteArray :=
  ba.foldl (λ acc b => acc.push (toLowerByte b)) ByteArray.empty

-- ─── Regex AST ─────────────────────────────────────────────────────────────────

inductive Regex where
  | Literal (b : UInt8)
  | Any
  | Star (r : Regex)
  | AnchorStart
  | AnchorEnd
  | CharClass (pos : Bool) (chars : List UInt8)
  | Seq (r1 r2 : Regex)
deriving Inhabited, DecidableEq, Repr

-- ─── Regex matching engine ─────────────────────────────────────────────────────

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
    | Regex.Literal b =>
      if pos < text.size && text.get! pos == b then some (pos + 1) else none
    | Regex.Any =>
      if pos < text.size then some (pos + 1) else none
    | Regex.Star r' =>
      let rec starGo (p : Nat) (iters : Nat) : Option Nat :=
        if iters == 0 then some p
        else match matchRegex r' text p with
          | none => some p
          | some np => starGo np (iters - 1)
      starGo pos (text.size - pos)
    | Regex.AnchorStart =>
      if pos == 0 then some pos else none
    | Regex.AnchorEnd =>
      if pos == text.size then some pos else none
    | Regex.CharClass posFlag chars =>
      if pos < text.size then
        let b := text.get! pos
        let inClass := chars.contains b
        if (posFlag && inClass) || (!posFlag && !inClass) then some (pos + 1) else none
      else none
    | Regex.Seq r1 r2 =>
      let rec tryEnds (ps : List Nat) : Option Nat :=
        match ps with
        | [] => none
        | p :: rest =>
          match matchRegex r2 text p with
          | some endPos => some endPos
          | none => tryEnds rest
      tryEnds (allMatchEnds r1 text pos)

  partial def allMatchEnds (r : Regex) (text : ByteArray) (pos : Nat) : List Nat :=
    match r with
    | Regex.Star r' =>
      let rec go (p : Nat) (acc : List Nat) : List Nat :=
        let acc' := p :: acc
        match matchRegex r' text p with
        | none => acc'
        | some np => go np acc'
      go pos []
    | Regex.Seq r1 r2 =>
      let r1Ends := allMatchEnds r1 text pos
      let rec tryAll (ps : List Nat) (acc : List Nat) : List Nat :=
        match ps with
        | [] => acc
        | p :: rest =>
          tryAll rest (allMatchEnds r2 text p ++ acc)
      tryAll r1Ends []
    | _ =>
      match matchRegex r text pos with
      | some p => [p]
      | none => []
end

def lineMatchesAux (re : Regex) (text : ByteArray) (startsAnchored endsAnchored : Bool)
    (pos : Nat) : Bool :=
  if pos >= text.size then false
  else if startsAnchored && pos > 0 then false
  else match matchRegex re text pos with
    | none => lineMatchesAux re text startsAnchored endsAnchored (pos + 1)
    | some endPos =>
      if endsAnchored then endPos == text.size
      else true
termination_by text.size - pos

def lineMatches (re : Regex) (text : ByteArray) : Bool :=
  lineMatchesAux re text (startsWithAnchorStart re) (endsWithAnchorEnd re) 0

-- ─── Regex parser / compiler ────────────────────────────────────────────────────

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

-- ─── Pattern matching / input processing ────────────────────────────────────────

def containsPattern (text : ByteArray) (pattern : ByteArray) (ignoreCase : Bool) : Bool :=
  let text' := if ignoreCase then toLowerByteArray text else text
  let pattern' := if ignoreCase then toLowerByteArray pattern else pattern
  let patternStr := String.fromUTF8! pattern'
  lineMatches (compilePattern patternStr) text'

def processInput (input : ByteArray) (pattern : String) (flags : Flags) : ByteArray × Bool :=
  let patternBytes := pattern.toUTF8
  let lines := splitLines input
  let cleaned :=
    match lines.reverse with
    | [] => []
    | last :: rest =>
      if last.isEmpty then rest.reverse else lines
  let matching := cleaned.filter (λ line =>
    let matched := containsPattern line patternBytes flags.ignoreCase
    if flags.invert then ¬ matched else matched)
  if flags.countOnly then
    let count := matching.length
    (s!"{count}".toUTF8, count > 0)
  else
    (joinLines matching, !matching.isEmpty)

-- ─── Proofs ─────────────────────────────────────────────────────────────────────

-- compilePattern: literal fallback produces Seq chain of Literals
example : compilePattern "hello" =
    Regex.Seq (Regex.Seq (Regex.Seq (Regex.Seq (Regex.Literal 0x68) (Regex.Literal 0x65))
      (Regex.Literal 0x6C)) (Regex.Literal 0x6C)) (Regex.Literal 0x6F) := by
  native_decide

-- compilePattern: empty pattern → Star Any (matches everything)
example : compilePattern "" = Regex.Star Regex.Any := by
  native_decide

-- compilePattern: single char → Literal
example : compilePattern "a" = Regex.Literal 0x61 := by
  native_decide

-- parseRegex: basic metacharacters parse correctly
example : parseRegex "." = some Regex.Any := by
  native_decide

example : parseRegex "a*" = some (Regex.Star (Regex.Literal 0x61)) := by
  native_decide

example : parseRegex "^a" = some (Regex.Seq Regex.AnchorStart (Regex.Literal 0x61)) := by
  native_decide

example : parseRegex "a$" = some (Regex.Seq (Regex.Literal 0x61) Regex.AnchorEnd) := by
  native_decide

example : parseRegex "[abc]" = some (Regex.CharClass true [0x61, 0x62, 0x63]) := by
  native_decide

example : parseRegex "[^abc]" = some (Regex.CharClass false [0x61, 0x62, 0x63]) := by
  native_decide

example : parseRegex "\\[" = some (Regex.Literal 0x5b) := by
  native_decide

example : parseRegex "a.*b" = some
    (Regex.Seq (Regex.Seq (Regex.Literal 0x61) (Regex.Star Regex.Any)) (Regex.Literal 0x62)) := by
  native_decide

-- parseRegex: invalid patterns return none
example : parseRegex "*" = none := by
  native_decide

example : parseRegex "[unclosed" = none := by
  native_decide

-- Anchor helpers
example : startsWithAnchorStart (Regex.AnchorStart) = true := rfl
example : startsWithAnchorStart (Regex.Seq Regex.AnchorStart (Regex.Literal 0x61)) = true := rfl
example : startsWithAnchorStart (Regex.Literal 0x61) = false := rfl

example : endsWithAnchorEnd (Regex.AnchorEnd) = true := rfl
example : endsWithAnchorEnd (Regex.Seq (Regex.Literal 0x61) Regex.AnchorEnd) = true := rfl
example : endsWithAnchorEnd (Regex.Literal 0x61) = false := rfl

-- matchRegex: Literal matches correct byte
example : matchRegex (Regex.Literal 0x61) (ByteArray.mk #[0x61]) 0 = some 1 := by
  native_decide

example : matchRegex (Regex.Literal 0x61) (ByteArray.mk #[0x62]) 0 = none := by
  native_decide

-- matchRegex: Any matches any single byte
example : matchRegex Regex.Any (ByteArray.mk #[0x78]) 0 = some 1 := by
  native_decide

example : matchRegex Regex.Any (ByteArray.mk #[]) 0 = none := by
  native_decide

-- matchRegex: Star matches zero or more
example : matchRegex (Regex.Star (Regex.Literal 0x61)) (ByteArray.mk #[]) 0 = some 0 := by
  native_decide

example : matchRegex (Regex.Star (Regex.Literal 0x61)) (ByteArray.mk #[0x61, 0x61, 0x61]) 0 = some 3 := by
  native_decide

-- matchRegex: AnchorStart/AnchorEnd
example : matchRegex Regex.AnchorStart (ByteArray.mk #[0x61, 0x62]) 0 = some 0 := by
  native_decide

example : matchRegex Regex.AnchorStart (ByteArray.mk #[0x61, 0x62]) 1 = none := by
  native_decide

example : matchRegex Regex.AnchorEnd (ByteArray.mk #[]) 0 = some 0 := by
  native_decide

example : matchRegex Regex.AnchorEnd (ByteArray.mk #[0x61]) 1 = some 1 := by
  native_decide

-- matchRegex: CharClass
example : matchRegex (Regex.CharClass true [0x61, 0x62]) (ByteArray.mk #[0x61]) 0 = some 1 := by
  native_decide

example : matchRegex (Regex.CharClass true [0x61, 0x62]) (ByteArray.mk #[0x63]) 0 = none := by
  native_decide

example : matchRegex (Regex.CharClass false [0x61, 0x62]) (ByteArray.mk #[0x63]) 0 = some 1 := by
  native_decide

-- matchRegex: Seq
example : matchRegex (Regex.Seq (Regex.Literal 0x61) (Regex.Literal 0x62))
    (ByteArray.mk #[0x61, 0x62]) 0 = some 2 := by
  native_decide

-- matchRegex: Seq with Star backtracking (h.*d on "hello world")
example : matchRegex
    (Regex.Seq (Regex.Seq (Regex.Literal 0x68) (Regex.Star Regex.Any)) (Regex.Literal 0x64))
    (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x77, 0x6F, 0x72, 0x6C, 0x64]) 0 = some 11 := by
  native_decide

-- lineMatches: finds match at position > 0
example : lineMatches (Regex.Literal 0x61) (ByteArray.mk #[0x62, 0x61]) = true := by
  native_decide

example : lineMatches (Regex.Literal 0x61) (ByteArray.mk #[0x62, 0x63]) = false := by
  native_decide

-- lineMatches: ^ and $ anchors
example : lineMatches (Regex.Seq Regex.AnchorStart (Regex.Literal 0x61))
    (ByteArray.mk #[0x61, 0x62]) = true := by
  native_decide

example : lineMatches (Regex.Seq Regex.AnchorStart (Regex.Literal 0x61))
    (ByteArray.mk #[0x62, 0x61]) = false := by
  native_decide

example : lineMatches (Regex.Seq (Regex.Literal 0x61) Regex.AnchorEnd)
    (ByteArray.mk #[0x61]) = true := by
  native_decide

example : lineMatches (Regex.Seq (Regex.Literal 0x61) Regex.AnchorEnd)
    (ByteArray.mk #[0x61, 0x62]) = false := by
  native_decide

-- lineMatches: regex with Star and backtracking
example : lineMatches (compilePattern "h.*d")
    (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x20, 0x77, 0x6F, 0x72, 0x6C, 0x64]) = true := by
  native_decide

-- containsPattern: backward compatibility — literal behaves like original
example : containsPattern (ByteArray.mk #[0x61, 0x62, 0x63]) (ByteArray.mk #[0x62]) false = true := by
  native_decide

example : containsPattern (ByteArray.mk #[0x61, 0x62, 0x63]) (ByteArray.mk #[0x64]) false = false := by
  native_decide

-- toLowerByte: uppercase letters become lowercase
example : toLowerByte 0x41 = 0x61 := by native_decide
example : toLowerByte 0x5A = 0x7A := by native_decide
example : toLowerByte 0x61 = 0x61 := by native_decide
example : toLowerByte 0x30 = 0x30 := by native_decide

-- containsPattern with ignoreCase
example : containsPattern (ByteArray.mk #[0x48, 0x65, 0x6C, 0x6C, 0x6F]) (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F]) true = true := by
  native_decide

example : containsPattern (ByteArray.mk #[0x68, 0x65, 0x6C, 0x6C, 0x6F]) (ByteArray.mk #[0x48, 0x45, 0x4C, 0x4C, 0x4F]) true = true := by
  native_decide

end Lentils.Grep.Logic
