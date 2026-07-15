/-
Shred — IO wrapper for the `shred` utility. 0BSD
-/

import Lentils.Shred.Logic
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Shred

open Logic
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/-- Get file size from stat. Returns 0 on error. -/
def getFileSize (path : String) : IO UInt64 := do
  try
    let arr ← statAll path
    if arr.size > 1 then pure arr[1]! else pure 0
  catch _ => pure 0

/-- Write a pattern byte to a file descriptor for a given size, in chunks. -/
def writePattern (fd : UInt32) (pattern : UInt8) (size : UInt64) : IO Bool := do
  let chunkSize : UInt64 := 65536
  let chunk := ByteArray.mk (List.toArray (List.replicate 65536 pattern))
  let mut remaining := size
  while remaining > 0 do
    let thisChunk := if remaining > chunkSize then chunkSize else remaining
    try
      let _ ← writeFd fd (chunk.extract 0 thisChunk.toNat)
      remaining := remaining - thisChunk
    catch _ =>
      return false
  return true

def run (args : List String) : IO UInt32 := do
  let (opts, files) := parseArgs args
  if files.isEmpty then
    return ← exitUsage "shred" "missing file operand"

  let mut failed := false
  for file in files do
    if opts.verbose then
      IO.eprintln s!"shred: {file}: processing..."

    let size ← getFileSize file
    if size == 0 then
      if opts.verbose then
        IO.eprintln s!"shred: {file}: skipping (empty or error)"
      if opts.remove then
        try unlink file catch _ => pure ()
      continue

    -- Open file for writing without truncation
    let fd ←
      try openWronly file
      catch e =>
        IO.eprintln s!"shred: {file}: failed to open for writing: {e.toString}"
        failed := true
        continue

    -- Write pattern passes
    for pass in List.range opts.passes do
      if opts.verbose then
        IO.eprintln s!"shred: {file}: pass {pass + 1}/{opts.passes}..."
      let pattern := patternForPass pass
      try
        let _ ← lseek fd 0 0
      catch _ =>
        IO.eprintln s!"shred: {file}: seek failed"
        break
      if !(← writePattern fd pattern size) then
        IO.eprintln s!"shred: {file}: write error on pass {pass + 1}"
        failed := true
        break

    -- Final zero pass
    if !failed && opts.zero then
      try
        let _ ← lseek fd 0 0
        let chunkSize : UInt64 := 65536
        let zeroChunk := ByteArray.mk (List.toArray (List.replicate 65536 0))
        let mut remaining := size
        while remaining > 0 do
          let thisChunk := if remaining > chunkSize then chunkSize else remaining
          let _ ← writeFd fd (zeroChunk.extract 0 thisChunk.toNat)
          remaining := remaining - thisChunk
      catch _ => pure ()

    -- fsync and close
    try fsync fd catch _ => pure ()
    try close fd catch _ => pure ()

    -- Optionally remove
    if opts.remove then
      try
        unlink file
        if opts.verbose then
          IO.eprintln s!"shred: {file}: removed"
      catch e =>
        IO.eprintln s!"shred: {file}: failed to remove: {e.toString}"

  return if failed then 1 else 0

end Lentils.Shred