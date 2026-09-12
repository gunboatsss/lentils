/-
Install.Logic — Verified pure logic for `install`. 0BSD
Spec-first methodology.
-/

import Lentils.Common.Spec

namespace Lentils.Install.Logic

open Lentils.Common.Spec

structure Options where
  directory : Bool := false
  mode : UInt32 := 0o755
  verbose : Bool := false
  compare : Bool := false
  backup : Bool := false
  strip : Bool := false
  owner : Option String := none
  group : Option String := none
  operands : List String := []
  deriving Repr, BEq, DecidableEq, Inhabited

structure InstallInput where
  args : List String
  deriving Inhabited, BEq

def defaultInput : InstallInput := { args := [] }

def parseMode (s : String) : Option UInt32 :=
  if s.isEmpty then none else
  let rec go (chars : List Char) (acc : UInt32) : Option UInt32 :=
    match chars with
    | [] => some acc
    | c :: rest =>
      if '0' ≤ c && c ≤ '7' then
        go rest (acc * 8 + (c.toNat - '0'.toNat).toUInt32)
      else none
  go (s.toList) 0

def parseArgs (args : List String) : Options :=
  let rec go (remaining : List String) (opts : Options) : Options :=
    match remaining with
    | [] => opts
    | "--" :: rest => { opts with operands := opts.operands ++ rest }
    | "-d" :: rest => go rest { opts with directory := true }
    | "--directory" :: rest => go rest { opts with directory := true }
    | "-v" :: rest => go rest { opts with verbose := true }
    | "--verbose" :: rest => go rest { opts with verbose := true }
    | "-C" :: rest => go rest { opts with compare := true }
    | "--compare" :: rest => go rest { opts with compare := true }
    | "-b" :: rest => go rest { opts with backup := true }
    | "--backup" :: rest => go rest { opts with backup := true }
    | "-s" :: rest => go rest { opts with strip := true }
    | "--strip" :: rest => go rest { opts with strip := true }
    | "-m" :: s :: rest =>
      match parseMode s with
      | some m => go rest { opts with mode := m }
      | none => go rest opts
    | "--mode" :: s :: rest =>
      match parseMode s with
      | some m => go rest { opts with mode := m }
      | none => go rest opts
    | "-o" :: s :: rest => go rest { opts with owner := some s }
    | "--owner" :: s :: rest => go rest { opts with owner := some s }
    | "-g" :: s :: rest => go rest { opts with group := some s }
    | "--group" :: s :: rest => go rest { opts with group := some s }
    | s :: rest =>
      if s.startsWith "-" && s.length > 1 then
        { opts with operands := opts.operands ++ s :: rest }
      else go rest { opts with operands := opts.operands ++ [s] }
  go args {}

def splitOperands (opts : Options) : List String × Option String :=
  if opts.directory then (opts.operands, none)
  else match opts.operands.reverse with
    | [] => ([], none)
    | dest :: revSrcs => (revSrcs.reverse, some dest)

def spec (input : InstallInput) : Options := parseArgs input.args

theorem i_empty : spec defaultInput = {} := by native_decide

theorem i_parseMode_of_empty (s : String) (h : s.isEmpty = true) :
    parseMode s = none := by
  simp [parseMode, h]

example : (parseArgs ["-d"]).directory = true := by native_decide
example : (parseArgs ["-m", "755"]).mode = 0o755 := by native_decide
example : (parseArgs ["-v"]).verbose = true := by native_decide
example : (parseArgs ["src", "dst"]).operands = ["src", "dst"] := by native_decide
example : parseMode "644" = some 0o644 := by native_decide
example : parseMode "755" = some 0o755 := by native_decide

end Lentils.Install.Logic
