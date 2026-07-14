/-
Groups — IO wrapper for the `groups` utility. 0BSD -/
import Lentils.Groups.Logic

namespace Lentils.Groups

open Logic

/-- FFI: look up group name by GID. Returns "" if not found. -/
@[extern "lean_coreutils_getgrgid"]
opaque getgrgid (gid : UInt32) : IO String

/-- FFI: look up user name by UID. Returns "" if not found. -/
@[extern "lean_coreutils_getpwuid"]
opaque getpwuid (uid : UInt32) : IO String

/-- FFI: look up UID by user name. Returns "" if not found. -/
@[extern "lean_coreutils_getpwnam"]
opaque getpwnam (name : String) : IO String

/-- Parse the Uid: line from /proc/self/status, return the real UID. -/
def parseUid (content : String) : String :=
  match findLine content "Uid:" with
  | none => "0"
  | some line =>
    let rest := line.drop 4
    (rest.trimAscii.toString).splitOn "\t" |>.filter (fun s => !s.isEmpty) |>.head? |>.getD "0"

/-- Parse the Gid: line from /proc/self/status, return the real GID. -/
def parseGid (content : String) : String :=
  match findLine content "Gid:" with
  | none => "0"
  | some line =>
    let rest := line.drop 4
    (rest.trimAscii.toString).splitOn "\t" |>.filter (fun s => !s.isEmpty) |>.head? |>.getD "0"

/-- Get groups for the current process (read from /proc/self/status). -/
def groupsCurrent : IO String := do
  let content ←
    try IO.FS.readFile "/proc/self/status"
    catch _ => pure ""
  if content.isEmpty then
    return ""

  let uidStr := parseUid content
  let uid := String.toNat? uidStr |>.getD 0
  let userName ← getpwuid (UInt32.ofNat uid)
  let name := if userName.isEmpty then uidStr else userName
  let primaryGid := parseGid content

  let mut out := name
  match findLine content "Groups:" with
  | none => pure out
  | some line =>
    let gidStrs := parseGroupsLine line
    let mut names : List String := []
    for gidStr in gidStrs do
      if gidStr ≠ primaryGid then
        let gid := String.toNat? gidStr |>.getD 0
        let gname ← getgrgid (UInt32.ofNat gid)
        names := names ++ [if gname.isEmpty then gidStr else gname]
    if !names.isEmpty then
      out := out ++ " " ++ String.intercalate " " names
    pure out

/-- Get groups for a user looked up by name (simplified: primary group only). -/
def groupsForUser (username : String) : IO String := do
  let uidGidStr ← getpwnam username
  if uidGidStr.isEmpty then
    IO.eprintln s!"groups: {username}: no such user"
    return ""
  -- uidGidStr format is "uid:gid"
  let parts := uidGidStr.splitOn ":"
  let uidStr := parts.head? |>.getD "0"
  let gidStr := match parts with | _ :: g :: _ => g | _ => "0"
  let uid := String.toNat? uidStr |>.getD 0
  let gid := String.toNat? gidStr |>.getD 0
  let userName ← getpwuid (UInt32.ofNat uid)
  let groupName ← getgrgid (UInt32.ofNat gid)
  let name := if userName.isEmpty then uidStr else userName
  let gname := if groupName.isEmpty then gidStr else groupName
  pure (name ++ " : " ++ gname)

def run (args : List String) : IO UInt32 := do
  match args with
  | username :: _ =>
    let out ← groupsForUser username
    if out.isEmpty then
      return 1
    IO.println out
    return 0
  | [] =>
    let out ← groupsCurrent
    if out.isEmpty then
      IO.eprintln "groups: cannot read /proc/self/status"
      return 1
    IO.println out
    return 0

end Lentils.Groups
