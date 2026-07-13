/-
Stat — IO wrapper for the `stat` utility.
0BSD

Displays file or file system status using stat(2) / lstat(2) / statvfs(2) via C FFI.
With no flags, uses lstat(2) (does not follow symlinks).
With -L / --dereference, uses stat(2) (follows symlinks).
-/

import Lentils.Stat.Logic
import Lentils.Common.Array
import Lentils.Common.Errors
import Lentils.Common.IO.Native

namespace Lentils.Stat

open Logic
open Lentils.Common.Array
open Lentils.Common.Errors
open Lentils.Common.IO.Native

/--
Run the `stat` utility.

Parses arguments, then stats each file operand.
Returns exit code 0 on success, or non-zero if any file could not be stat'd.
-/
def run (progName : String) (args : List String) : IO UInt32 := do
  let opts := parseArgs args
  if opts.files.isEmpty then
    return ← exitUsage progName "missing operand"
  let mut failed := false
  for file in opts.files do
    if opts.filesys then
      try
        let arr ← statvfsAll file
        if arr.size ≥ 9 then
          let bs := arrGet arr 0
          let fr := arrGet arr 1
          let bl := arrGet arr 2
          let bf := arrGet arr 3
          let ba := arrGet arr 4
          let fi := arrGet arr 5
          let ff := arrGet arr 6
          let fa := arrGet arr 7
          let nm := arrGet arr 8
          IO.print (formatFsLine bs fr bl bf ba fi ff fa nm file)
        else
          IO.eprintln s!"{progName}: {file}: insufficient statvfs data"
          failed := true
      catch _ =>
        IO.eprintln s!"{progName}: cannot statx '{file}': No such file or directory"
        failed := true
    else
      -- Use lstat by default, stat when -L is given
      let statFn := if opts.follow then statAll else lstatAll
      try
        let arr ← statFn file
        if arr.size ≥ 8 then
          let mode := arrGet arr 0
          let size := arrGet arr 1
          let nlink := arrGet arr 2
          let uid := arrGet arr 3
          let gid := arrGet arr 4
          let blocks := arrGet arr 5
          let blksize := arrGet arr 6
          let dev := arrGet arr 7
          let ino := arrGet arr 8
          let rdev := arrGet arr 9
          match opts.format with
          | some fmt =>
            IO.print (formatCustom fmt mode size nlink uid gid blocks blksize dev ino file)
            IO.print "\n"
          | none =>
            if opts.terse then
              IO.print (formatTerse mode size nlink uid gid blocks blksize dev ino rdev file)
            else
              IO.print (formatStatLine mode size nlink uid gid blocks blksize file)
        else
          IO.eprintln s!"{progName}: {file}: insufficient stat data"
          failed := true
      catch _ =>
        IO.eprintln s!"{progName}: cannot statx '{file}': No such file or directory"
        failed := true
  if failed then
    return 1
  else
    return 0

end Lentils.Stat
