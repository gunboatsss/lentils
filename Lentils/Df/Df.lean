/-
Df — IO wrapper for the `df` utility.
0BSD

Reports file system disk space usage using statvfs(2) via C FFI.
When no operands are given, shows all mounted filesystems.
Supports -h (human-readable) and --block-size options.
-/

import Lentils.Df.Logic
import Lentils.Common.Array
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Df

open Logic
open Lentils.Common.Array
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Format size in human-readable form using 1024-based units.
-/
def formatHuman (sizeK : UInt64) : String :=
  if sizeK < 1024 then toString sizeK else
  if sizeK < 1024*1024 then
    toString (sizeK / 1024) ++ "M"
  else if sizeK < 1024*1024*1024 then
    toString (sizeK / (1024*1024)) ++ "G"
  else
    toString (sizeK / (1024*1024*1024)) ++ "T"

/--
Format a single df output line, respecting blockSize and human options.
-/
def formatLineOpts (fs : String) (totalBlocks freeBlocks availBlocks usePct : UInt64) (mounted : String) (opts : Options) : String :=
  if opts.human then
    s!"{fs}  {formatHuman totalBlocks}  {formatHuman (totalBlocks - freeBlocks)}  {formatHuman availBlocks}  {usePct}%  {mounted}\n"
  else
    let scale := opts.blockSize / 1024
    if scale == 0 then
      s!"{fs}  {totalBlocks}  {totalBlocks - freeBlocks}  {availBlocks}  {usePct}%  {mounted}\n"
    else
      let total := totalBlocks / scale
      let used := (totalBlocks - freeBlocks) / scale
      let avail := availBlocks / scale
      s!"{fs}  {total}  {used}  {avail}  {usePct}%  {mounted}\n"

/--
Format header line, respecting blockSize and human options.
-/
def headerLineOpts (opts : Options) : String :=
  if opts.human then
    "Filesystem    Size  Used  Avail  Use%  Mounted on\n"
  else
    let sizeLabel := if opts.blockSize == 1024 then "1K-blocks" else s!"{opts.blockSize}-blocks"
    s!"Filesystem      {sizeLabel}     Used    Available  Use%  Mounted on\n"

/--
Run the `df` utility.

Parses arguments, then shows disk space for each file operand.
Defaults to showing all mounted filesystems when no operand is given.
-/
def run (progName : String) (args : List String) : IO UInt32 := do
  let opts := parseArgs args
  let paths ←
    if opts.files.isEmpty then do
      -- List all mounted filesystems
      match ← try some <$> getMounts catch _ => pure none with
      | some mounts =>
        if Array.size mounts == 0 then
          pure ["/"]
        else
          pure (Array.toList mounts)
      | none => pure ["/"]
    else
      pure opts.files
  let mut failed := false
  IO.print (headerLineOpts opts)
  for path in paths do
    match ← try some <$> statvfsAll path catch _ => pure none with
    | none =>
      -- Silently skip paths that can't be statvfs'd (like fuse/doc mounts)
      pure ()
    | some arr =>
      if arr.size ≥ 5 then
        let frsize := arrGet arr 1  -- fundamental block size
        let totalBlocks := arrGet arr 2
        let freeBlocks := arrGet arr 3
        let availBlocks := arrGet arr 4
        -- Convert to 1K-block units
        let factor := frsize / 1024
        if factor == 0 then
          -- Skip filesystems with invalid block size (e.g. efivarfs)
          pure ()
        else
          let totalK := totalBlocks * factor
          let freeK := freeBlocks * factor
          let availK := availBlocks * factor
          let used := totalK - freeK
          let usePct := if totalK > 0 then ((used * 100) / totalK) else 0
          IO.print (formatLineOpts path totalK freeK availK usePct path opts)
      else
        IO.eprintln s!"{progName}: {path}: insufficient statvfs data"
        failed := true
  if failed then
    return 1
  else
    return 0

end Lentils.Df
