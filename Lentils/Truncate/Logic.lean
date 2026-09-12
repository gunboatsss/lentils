/-
Truncate.Logic — Verified pure logic for `truncate`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Truncate.Logic

open Lentils.Common.Spec

structure Options where
  size : Option UInt64 := none
  reference : Option String := none
  noCreate : Bool := false
  files : List String := []
  deriving Repr, BEq, DecidableEq, Inhabited

structure TruncateInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : TruncateInput := { args := [] }

def parseSize (s : String) : Option UInt64 :=
  if s.isEmpty then none else
    let chars := s.toList
    if chars.isEmpty then none else
      let lastChar := chars.getLast? |>.getD ' '
      if lastChar ∈ ['K', 'k', 'M', 'm', 'G', 'g'] then
        let numStr := String.ofList (chars.dropLast)
        match numStr.toNat? with
        | none => none
        | some n =>
          let multiplier : UInt64 :=
            match lastChar with
            | 'K' | 'k' => 1024
            | 'M' | 'm' => 1024 * 1024
            | 'G' | 'g' => 1024 * 1024 * 1024
            | _ => 1
          some (n.toUInt64 * multiplier)
      else
        match s.toNat? with
        | none => none
        | some n => some n.toUInt64

def parseArgs (args : List String) : Options :=
  let rec go (remaining : List String) (opts : Options) : Options :=
    match remaining with
    | [] => opts
    | "--" :: rest => { opts with files := opts.files ++ rest }
    | "-c" :: rest => go rest { opts with noCreate := true }
    | "--no-create" :: rest => go rest { opts with noCreate := true }
    | "-s" :: s :: rest => go rest { opts with size := parseSize s }
    | "--size" :: s :: rest => go rest { opts with size := parseSize s }
    | "-r" :: s :: rest => go rest { opts with reference := some s }
    | "--reference" :: s :: rest => go rest { opts with reference := some s }
    | s :: rest =>
      if s.startsWith "-" && s.length > 1 then
        { opts with files := opts.files ++ s :: rest }
      else go rest { opts with files := opts.files ++ [s] }
  go args {}

def spec (input : TruncateInput) : Options := parseArgs input.args

theorem i_empty : spec defaultInput = {} := by native_decide

/--
Parsing after `--` preserves the remaining files (parametric over all lists).
-/
theorem parseArgs_double_dash_files (rest : List String) :
    (parseArgs ("--" :: rest)).files = rest := by
  unfold parseArgs; rfl

example : (parseArgs ["-c", "file"]).noCreate = true := by native_decide
example : (parseArgs ["file"]).files = ["file"] := by native_decide
example : parseSize "1K" = some 1024 := by native_decide
example : parseSize "1M" = some (1024 * 1024) := by native_decide
example : parseSize "1G" = some (1024 * 1024 * 1024) := by native_decide
example : parseSize "42" = some 42 := by native_decide
example : parseSize "" = none := by native_decide
example : parseSize "abc" = none := by native_decide

end Lentils.Truncate.Logic
