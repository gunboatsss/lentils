/-
Mknod.Logic — Verified pure logic for `mknod`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Mknod.Logic

open Lentils.Common.Spec

inductive NodeType
  | block
  | character
  deriving Repr, BEq, DecidableEq, Inhabited

structure Options where
  nodeType : NodeType := .character
  mode : UInt32 := 0o666
  major : UInt32 := 0
  minor : UInt32 := 0
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq, Inhabited

structure MknodInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : MknodInput := { args := [] }

def parseType (s : String) : NodeType :=
  if s = "b" then .block else .character

def parseNum (s : String) : UInt32 :=
  match s.toNat? with
  | some n => UInt32.ofNat n
  | none => 0

def parseArgs (args : List String) : Option (Options × String) :=
  let rec go (remaining : List String) (opts : Options) : Option (Options × String) :=
    match remaining with
    | [] => none
    | "--" :: rest => go rest opts
    | "-m" :: modeStr :: rest => go rest { opts with mode := parseNum modeStr }
    | "--mode" :: modeStr :: rest => go rest { opts with mode := parseNum modeStr }
    | "-v" :: rest => go rest { opts with verbose := true }
    | "--verbose" :: rest => go rest { opts with verbose := true }
    | s :: rest =>
      if s.startsWith "-" && s != "-" then none
      else
        let name := s
        match rest with
        | typeStr :: majorStr :: minorStr :: _ =>
          some ({ opts with
            nodeType := parseType typeStr
            major := parseNum majorStr
            minor := parseNum minorStr
          }, name)
        | _ => none
  go args {}

def spec (input : MknodInput) : Option (Options × String) := parseArgs input.args

/-- Unfolding lemma: `spec` delegates to `parseArgs`. -/
theorem i_spec_unfold (input : MknodInput) : spec input = parseArgs input.args := by
  simp [spec]

theorem i_empty : spec defaultInput = none := by native_decide

def expectedChar : Options := { nodeType := .character, major := 1, minor := 2 }
def expectedBlock : Options := { nodeType := .block, major := 8, minor := 0 }

example : parseArgs ["node", "c", "1", "2"] = some (expectedChar, "node") := by
  native_decide

example : parseArgs ["node", "b", "8", "0"] = some (expectedBlock, "node") := by
  native_decide

example : parseType "b" = .block := by native_decide
example : parseType "c" = .character := by native_decide
example : parseType "u" = .character := by native_decide
example : parseType "x" = .character := by native_decide
example : parseNum "0" = 0 := by native_decide
example : parseNum "42" = 42 := by native_decide
example : parseNum "abc" = 0 := by native_decide
example : parseNum "" = 0 := by native_decide
example : parseArgs ["node"] = none := by native_decide
example : parseArgs ["node", "c"] = none := by native_decide

end Lentils.Mknod.Logic
