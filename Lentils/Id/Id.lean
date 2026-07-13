/-
Id — IO wrapper for the `id` utility. 0BSD -/
import Lentils.Id.Logic

namespace Lentils.Id

open Logic

/-- FFI: look up username by UID. Returns "" if not found. -/
@[extern "lean_coreutils_getpwuid"]
opaque getpwuid (uid : UInt32) : IO String

/-- FFI: look up group name by GID. Returns "" if not found. -/
@[extern "lean_coreutils_getgrgid"]
opaque getgrgid (gid : UInt32) : IO String

def readStatus : IO String :=
  try IO.FS.readFile "/proc/self/status"
  catch _ => pure ""

def run (_args : List String) : IO UInt32 := do
  let content ← readStatus
  if content.isEmpty then
    IO.eprintln "id: cannot read /proc/self/status"
    return 1

  -- Parse Uid line
  let uidLine := findLine content "Uid:" |>.getD ""
  let uidParts := parseUidLine uidLine
  let uid := match uidParts with | x :: _ => x | [] => "0"
  let euid := match uidParts with | _ :: x :: _ => x | _ => uid

  -- Parse Gid line
  let gidLine := findLine content "Gid:" |>.getD ""
  let gidParts := parseGidLine gidLine
  let gid := match gidParts with | x :: _ => x | [] => "0"
  let egid := match gidParts with | _ :: x :: _ => x | _ => gid

  -- Parse Groups line
  let groupsLine := findLine content "Groups:" |>.getD ""
  let gidStrs := parseGroupsLine groupsLine

  -- Look up names
  let uidName ← getpwuid (UInt32.ofNat (String.toNat? uid |>.getD 0))
  let gidName ← getgrgid (UInt32.ofNat (String.toNat? gid |>.getD 0))

  -- Look up group names
  let mut groupPairs : List (String × String) := []
  for gidStr in gidStrs do
    let gidNat := String.toNat? gidStr |>.getD 0
    let name ← getgrgid (UInt32.ofNat gidNat)
    groupPairs := groupPairs ++ [(gidStr, name)]

  let info : IdInfo := {
    uid := uid
    euid := euid
    gid := gid
    egid := egid
    groups := gidStrs
  }

  let out := formatId info uidName gidName groupPairs
  IO.println out
  return 0

end Lentils.Id
