/-
B2sum - IO wrapper for the b2sum utility. 0BSD
-/

import Lentils.Common.IO.Native
import Lentils.B2sum.Logic

namespace Lentils.B2sum

open Logic
open Lentils.Common.IO.Native

-- Parse length argument from args. Returns 64 (default) or parsed value in bytes.
def parseLength (args : List String) : UInt64 :=
  match args with
  | [] => 64
  | "-l" :: n :: _ =>
    match n.toNat? with
    | some bits => if bits <= 512 && bits % 8 == 0 then UInt64.ofNat (bits / 8) else 64
    | none => 64
  | "--length" :: n :: _ =>
    match n.toNat? with
    | some bits => if bits <= 512 && bits % 8 == 0 then UInt64.ofNat (bits / 8) else 64
    | none => 64
  | _ => 64

-- Check if string is a flag (starts with -)
def isFlag (s : String) : Bool := s.startsWith "-"

-- Skip length/check opts and return remaining args as files
def extractFiles (args : List String) : List String :=
  let rec go (remaining : List String) (acc : List String) :=
    match remaining with
    | [] => acc.reverse
    | "-l" :: _ :: rest => go rest acc
    | "--length" :: _ :: rest => go rest acc
    | "-c" :: rest => go rest acc
    | "--check" :: rest => go rest acc
    | a :: rest => go rest (a :: acc)
  go args []

-- Parse hash line format: "<hash>  <filename>" returns (hash, filename)
def parseHashLine (line : String) : Option (String × String) :=
  let parts := line.splitOn "  "
  match parts with
  | [hash, file] => some (hash, file)
  | _ => none

-- Check hashes from file
def checkFromFile (hashfile : String) (outLen : UInt64) : IO UInt32 := do
  let content ← IO.FS.readFile hashfile
  let lines := content.splitOn "\n"
  let mut anyFailed := false
  for line in lines do
    match parseHashLine line with
    | some (expected, file) =>
      try
        let fileContent ← readAll (← openFileRead file)
        let actual := formatHex (blake2b fileContent outLen)
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
def checkFromStdin (outLen : UInt64) : IO UInt32 := do
  let lines ← readStdinLines
  let mut anyFailed := false
  for line in lines do
    match parseHashLine line with
    | some (expected, file) =>
      try
        let content ← readAll (← openFileRead file)
        let actual := formatHex (blake2b content outLen)
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
  let outLen := parseLength args
  let files := extractFiles args
  let hasCheck := args.contains "--check" || args.contains "-c"
  if hasCheck then
    if files.length > 0 then
      checkFromFile (files[0]!) outLen
    else
      checkFromStdin outLen
  else
    if files.isEmpty then
      let input ← readStdin
      let hash := blake2b input outLen
      IO.print (formatHex hash)
      IO.print "  -\n"
      return 0
    else
      let mut failed := false
      for file in files do
        try
          let content ← readAll (← openFileRead file)
          let hash := blake2b content outLen
          IO.print (formatHex hash ++ "  " ++ file ++ "\n")
        catch _ =>
          failed := true
      if failed then return 1 else return 0

end Lentils.B2sum
