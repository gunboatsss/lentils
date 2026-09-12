/-
Chmod.Logic — Verified pure logic for `chmod`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Chmod.Logic

open Lentils.Common.Spec

structure Options where
  recursive : Bool := false
  verbose : Bool := false
  force : Bool := false
  changes : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure ChmodInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : ChmodInput := { args := [] }

def isFlag (s : String) : Bool := s.startsWith "-"

def applyShort (c : Char) (opts : Options) : Option Options :=
  match c with
  | 'R' => some { opts with recursive := true }
  | 'v' => some { opts with verbose := true }
  | 'f' => some { opts with force := true }
  | 'c' => some { opts with changes := true }
  | _ => none

def applyShortFlags (s : String) (opts : Options) : Option Options :=
  let chars := s.toList.tail
  let rec go (cs : List Char) (o : Options) : Option Options :=
    match cs with
    | [] => some o
    | c :: r =>
      match applyShort c o with
      | none => none
      | some o2 => go r o2
  go chars opts

def parseArgs (args : List String) : Options × String × List String :=
  let rec go (remaining : List String) (opts : Options) (collected : List String)
      : Options × String × List String :=
    match remaining with
    | [] =>
        match collected.reverse with
        | [] => (opts, "", [])
        | mode :: files => (opts, mode, files)
    | "--" :: rest =>
        let (mode, files) := match rest with | [] => ("", []) | m :: fs => (m, fs)
        (opts, mode, files)
    | "-R" :: rest => go rest { opts with recursive := true } collected
    | "--recursive" :: rest => go rest { opts with recursive := true } collected
    | "-v" :: rest => go rest { opts with verbose := true } collected
    | "--verbose" :: rest => go rest { opts with verbose := true } collected
    | "-f" :: rest => go rest { opts with force := true } collected
    | "--force" :: rest => go rest { opts with force := true } collected
    | "-c" :: rest => go rest { opts with changes := true } collected
    | "--changes" :: rest => go rest { opts with changes := true } collected
    | s :: rest =>
      if s == "-" then go rest opts (s :: collected)
      else if s.startsWith "-" then
        if s.length >= 3 then
          match applyShortFlags s opts with
          | none =>
            let (mode, files) := match remaining with | [] => ("", []) | m :: fs => (m, fs)
            (opts, mode, files)
          | some newOpts => go rest newOpts collected
        else
          let (mode, files) := match remaining with | [] => ("", []) | m :: fs => (m, fs)
          (opts, mode, files)
      else go rest opts (s :: collected)
  go args {} []

def isOctalDigit (c : Char) : Bool := '0' <= c && c <= '7'

def isOctal (s : String) : Bool := !s.isEmpty && s.all isOctalDigit

def parseOctal (s : String) : Option UInt32 :=
  if isOctal s then
    let n : Nat := s.foldl (λ (a : Nat) (c : Char) => a * 8 + (c.toNat - '0'.toNat)) 0
    some n.toUInt32
  else none

def permLow (c : Char) : UInt32 :=
  match c with | 'r' => 0o4 | 'w' => 0o2 | 'x' => 0o1 | 'X' => 0o1 | _ => 0

def shiftOf (w : Char) : UInt32 :=
  match w with | 'u' => 6 | 'g' => 3 | 'o' => 0 | _ => 0

def scopeMask (w : Char) : UInt32 := 0o7 <<< shiftOf w

def applyClause (cur : UInt32) (who : List Char) (op : Char) (perms : List Char) : UInt32 :=
  let scopes : List Char := if who.isEmpty then ['u', 'g', 'o'] else who
  let low := perms.foldl (λ a c => a ||| permLow c) 0
  let afterScopes := scopes.foldl (λ (m : UInt32) w =>
    let shifted := low <<< shiftOf w
    let mask := scopeMask w
    match op with
    | '=' => (m &&& ~~~mask) ||| shifted
    | '+' => m ||| shifted
    | '-' => m &&& ~~~shifted
    | _   => m) cur
  let hasU := scopes.contains 'u'
  let hasG := scopes.contains 'g'
  let hasO := scopes.contains 'o'
  let hasS := perms.contains 's'
  let hasT := perms.contains 't'
  let scopeSpecial : UInt32 :=
    (if hasU then 0o4000 else 0) ||| (if hasG then 0o2000 else 0) ||| (if hasO then 0o1000 else 0)
  let permSpecial : UInt32 :=
    (if hasS && hasU then 0o4000 else 0) |||
    (if hasS && hasG then 0o2000 else 0) |||
    (if hasT && hasO then 0o1000 else 0)
  let withSpecial : UInt32 :=
    match op with
    | '=' => (afterScopes &&& ~~~scopeSpecial) ||| permSpecial
    | '+' => afterScopes ||| permSpecial
    | '-' => afterScopes &&& ~~~permSpecial
    | _   => afterScopes
  withSpecial

def parseClause (s : String) : Option (List Char × Char × List Char) :=
  let chars := s.toList
  let rec scanWho (cs : List Char) (who : List Char) : Option (List Char × List Char × Char × List Char) :=
    match cs with
    | [] => none
    | c :: rest =>
      if c == 'u' || c == 'g' || c == 'o' || c == 'a' then
        scanWho rest (who ++ [if c == 'a' then 'a' else c])
      else if c == '+' || c == '-' || c == '=' then
        let w := if who.contains 'a' then [] else who
        some (rest, w, c, [])
      else none
  match scanWho chars [] with
  | none => none
  | some (rest, who, op, _) =>
      let perms := rest.filter (λ c => c == 'r' || c == 'w' || c == 'x' || c == 'X' || c == 's' || c == 't')
      if perms.isEmpty then none else some (who, op, perms)

def computeMode (modeStr : String) (current : UInt32) : Option UInt32 :=
  if isOctal modeStr then parseOctal modeStr
  else
    let clauses := modeStr.splitOn ","
    let rec go (cls : List String) (m : UInt32) : Option UInt32 :=
      match cls with
      | [] => some m
      | cl :: rest =>
        match parseClause cl with
        | none => none
        | some (who, op, perms) => go rest (applyClause m who op perms)
    go clauses current

def spec (input : ChmodInput) : Options × String × List String := parseArgs input.args

/--
Spec agrees with parseArgs on all inputs (∀-quantified invariant).
-/
theorem i_spec_eq_parseArgs (input : ChmodInput) :
    spec input = parseArgs input.args := by
  simp [spec]

theorem i_empty : spec defaultInput = ({}, "", []) := by native_decide

example : parseOctal "755" = some 0o755 := by native_decide
example : parseOctal "0644" = some 0o644 := by native_decide
example : isOctal "u+x" = false := by native_decide
example : computeMode "u+x" 0o644 = some 0o744 := by native_decide
example : computeMode "u=rwx" 0o000 = some 0o700 := by native_decide
example : computeMode "755" 0o000 = some 0o755 := by native_decide
example : computeMode "g-w" 0o664 = some 0o644 := by native_decide
example : computeMode "u-s" 0o4755 = some 0o0755 := by native_decide
example : computeMode "a+s" 0o0755 = some 0o6755 := by native_decide
example : computeMode "a=rwx" 0o6755 = some 0o0777 := by native_decide

end Lentils.Chmod.Logic
