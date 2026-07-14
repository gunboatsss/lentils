/-
Md5sum - IO wrapper for the md5sum utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.Md5sum.Logic

namespace Lentils.Md5sum

open Logic
open Lentils.Common.IO.Native

-- Parse hash line format: "<hash>  <filename>" returns (hash, filename)
def parseHashLine (line : String) : Option (String × String) :=
  let parts := line.splitOn "  "
  match parts with
  | [hash, file] => some (hash, file)
  | _ => none

-- Check hashes from file
def checkFromFile (hashfile : String) : IO UInt32 := do
  let content ← IO.FS.readFile hashfile
  let lines := content.splitOn "\n"
  let mut anyFailed := false
  for line in lines do
    match parseHashLine line with
    | some (expected, file) =>
      try
        let fileContent ← readAll (← openFileRead file)
        let actual := formatHex (md5 fileContent)
        if actual != expected then
          IO.print s!"{file}: FAILED\n"
          anyFailed := true
        else
          IO.print s!"{file}: OK\n"
      catch _ =>
        IO.print s!"{file}: FAILED (missing)\n"
        anyFailed := true
    | none => pure ()
  if anyFailed then return 1 else return 0

-- Check hashes from stdin
def checkFromStdin : IO UInt32 := do
  let lines ← readStdinLines
  let mut anyFailed := false
  for line in lines do
    match parseHashLine line with
    | some (expected, file) =>
      try
        let content ← readAll (← openFileRead file)
        let actual := formatHex (md5 content)
        if actual != expected then
          IO.print s!"{file}: FAILED\n"
          anyFailed := true
        else
          IO.print s!"{file}: OK\n"
      catch _ =>
        IO.print s!"{file}: FAILED (missing)\n"
        anyFailed := true
    | none => pure ()
  if anyFailed then return 1 else return 0

def run (args : List String) : IO UInt32 := do
  let hasCheck := args.contains "--check" || args.contains "-c"
  if hasCheck then
    let nonFlag := args.filter (fun a => !a.startsWith "-")
    if nonFlag.length > 0 then
      checkFromFile (nonFlag[0]!)
    else
      checkFromStdin
  else
    if args.isEmpty then
      let input ← readStdin
      IO.print (formatStdin input)
      return 0
    else
      let mut failed := false
      for file in args do
        try
          let content ← readAll (← openFileRead file)
          IO.print (formatHex (md5 content) ++ "  " ++ file ++ "\n")
        catch _ =>
          failed := true
      if failed then return 1 else return 0

end Lentils.Md5sum
