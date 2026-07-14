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

/-- FFI: look up UID:GID by user name. Returns "" if not found. -/
@[extern "lean_coreutils_getpwnam"]
opaque getpwnam (name : String) : IO String

def readStatus : IO String :=
  try IO.FS.readFile "/proc/self/status"
  catch _ => pure ""

/-- Show id info for the current process. -/
def idCurrent : IO String := do
  let content ← readStatus
  if content.isEmpty then
    return ""

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

  -- Look up primary group name
  let primaryGroupName ← getgrgid (UInt32.ofNat (String.toNat? gid |>.getD 0))
  -- Build groups list: primary GID first, then supplementary GIDs
  let mut groupPairs : List (String × String) :=
    [(gid, if primaryGroupName.isEmpty then gid else primaryGroupName)]
  for gidStr in gidStrs do
    -- Skip if it's the same as the primary GID
    if gidStr ≠ gid then
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

  pure (formatId info uidName gidName groupPairs)

/-- Show id info for a user looked up by name. -/
def idForUser (username : String) : IO String := do
  let uidGidStr ← getpwnam username
  if uidGidStr.isEmpty then
    IO.eprintln s!"id: {username}: no such user"
    return ""
  let parts := uidGidStr.splitOn ":"
  let uidStr := parts.head? |>.getD "0"
  let gidStr := match parts with | _ :: g :: _ => g | _ => "0"
  let uid := String.toNat? uidStr |>.getD 0
  let gid := String.toNat? gidStr |>.getD 0
  let uidName ← getpwuid (UInt32.ofNat uid)
  let gidName ← getgrgid (UInt32.ofNat gid)
  let name := if uidName.isEmpty then uidStr else uidName
  let gname := if gidName.isEmpty then gidStr else gidName
  -- Simplified: just show primary group (no supplementary groups lookup for other users)
  let groupPairs : List (String × String) := [(gidStr, gname)]
  let info : IdInfo := {
    uid := uidStr
    euid := uidStr
    gid := gidStr
    egid := gidStr
    groups := [gidStr]
  }
  pure (formatId info name gname groupPairs)

def run (args : List String) : IO UInt32 := do
  match args with
  | username :: _ =>
    let out ← idForUser username
    if out.isEmpty then
      return 1
    IO.println out
    return 0
  | [] =>
    let out ← idCurrent
    if out.isEmpty then
      IO.eprintln "id: cannot read /proc/self/status"
      return 1
    IO.println out
    return 0

end Lentils.Id
