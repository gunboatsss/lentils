/-
Uname — IO wrapper for the `uname` utility. 0BSD -/
import Lentils.Uname.Logic

namespace Lentils.Uname

open Logic

def readProcFile (path : String) : IO String := do
  try
    let c ← IO.FS.readFile path
    pure (c.trimAscii.toString)
  catch _ =>
    pure ""

def run (args : List String) : IO UInt32 := do
  let hasFlags := args.any (λ a => a.startsWith "-" && a != "-")
  let mut info : UnameInfo := {}
  info := { info with
    sysname  := ← readProcFile "/proc/sys/kernel/ostype"
    nodename := ← readProcFile "/proc/sys/kernel/hostname"
    release  := ← readProcFile "/proc/sys/kernel/osrelease"
    machine  := ← readProcFile "/proc/sys/kernel/arch"
  }
  -- version: read /proc/sys/kernel/version (just the #N part) or /proc/version
  let fullVersion ← readProcFile "/proc/sys/kernel/version"
  info := { info with version := fullVersion }

  if info.sysname.isEmpty then info := { info with sysname := "Linux" }

  if hasFlags then
    -- Support -s, -n, -r, -v, -m, -a
    let out := args.foldl (λ (acc : String) a =>
      match a with
      | "-a" | "--all" => formatAll info
      | "-s" | "--kernel-name" => formatSysname info
      | "-n" | "--nodename" => formatNodename info
      | "-r" | "--kernel-release" => formatRelease info
      | "-v" | "--kernel-version" => formatVersion info
      | "-m" | "--machine" => formatMachine info
      | _ => acc
    ) ""
    if out.isEmpty then
      IO.println (formatSysname info)
    else
      IO.println out
  else
    IO.println (formatSysname info)
  return 0

end Lentils.Uname
