/-
Groups — IO wrapper for the `groups` utility. 0BSD -/
import Lentils.Groups.Logic

namespace Lentils.Groups

open Logic

/-- FFI: look up group name by GID. Returns "" if not found. -/
@[extern "lean_coreutils_getgrgid"]
opaque getgrgid (gid : UInt32) : IO String

def run (args : List String) : IO UInt32 := do
  let content ←
    try IO.FS.readFile "/proc/self/status"
    catch _ => pure ""
  if content.isEmpty then
    IO.eprintln "groups: cannot read /proc/self/status"
    return 1
  match findLine content "Groups:" with
  | none =>
    -- No supplementary groups; print primary group via /proc
    IO.println ""
    return 0
  | some line =>
    let gidStrs := parseGroupsLine line
    let mut names : List String := []
    for gidStr in gidStrs do
      let gid := String.toNat? gidStr |>.getD 0
      let name ← getgrgid (UInt32.ofNat gid)
      names := names ++ [if name.isEmpty then gidStr else name]
    IO.println (String.intercalate " " names)
    return 0

end Lentils.Groups
