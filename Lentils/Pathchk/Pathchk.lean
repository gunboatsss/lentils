/-
Pathchk — IO wrapper for the `pathchk` utility. 0BSD
-/

import Lentils.Pathchk.Logic

namespace Lentils.Pathchk

open Logic

structure PathchkOpts where
  portable : Bool := false
  strict : Bool := false
  paths : List String := []

/-- Parse pathchk options from args. -/
def parseOpts (args : List String) : PathchkOpts :=
  let rec go (remaining : List String) (opts : PathchkOpts) : PathchkOpts :=
    match remaining with
    | [] => opts
    | "-p" :: rest => go rest { opts with portable := true }
    | "-P" :: rest => go rest { opts with strict := true }
    | "--" :: rest => { opts with paths := opts.paths.reverse ++ rest }
    | f :: rest => if f.startsWith "-" then go rest opts else go rest { opts with paths := f :: opts.paths }
  go args {}

def run (args : List String) : IO UInt32 := do
  let opts := parseOpts args
  if opts.paths.isEmpty then
    IO.eprintln "pathchk: missing operand"
    IO.eprintln "Try 'pathchk --help' for more information."
    return 1
  let mut exitCode : UInt32 := 0
  for path in opts.paths do
    let errors := checkPath path opts.portable opts.strict
    if !errors.isEmpty then
      exitCode := 1
      for err in errors do
        IO.eprintln s!"pathchk: {err} in file name '{path}'"
  return exitCode

end Lentils.Pathchk
