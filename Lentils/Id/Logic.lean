/-
Id.Logic — Pure identity-formatting logic for `id`. 0BSD -/
namespace Lentils.Id.Logic

structure IdInfo where
  uid : String := ""
  euid : String := ""
  gid : String := ""
  egid : String := ""
  groups : List String := []
deriving Inhabited

/--
Parse a /proc/self/status line like "Uid:	1000	1000	1000	1000"
Returns the four values (real, effective, saved, filesystem).
-/
def parseUidLine (line : String) : List String :=
  if !String.startsWith line "Uid:" then []
  else
    let rest := (line.drop 4).toString
    (rest.trimAscii.toString).splitOn "\t" |>.filter (fun s => !s.isEmpty)

/--
Parse a /proc/self/status line like "Gid:	1000	1000	1000	1000"
Returns the four values (real, effective, saved, filesystem).
-/
def parseGidLine (line : String) : List String :=
  if !String.startsWith line "Gid:" then []
  else
    let rest := (line.drop 4).toString
    (rest.trimAscii.toString).splitOn "\t" |>.filter (fun s => !s.isEmpty)

/--
Parse Groups line like "Groups:	4 24 27 30 46 100 114 118 131 1000"
-/
def parseGroupsLine (line : String) : List String :=
  if !String.startsWith line "Groups:" then []
  else
    let rest := (line.drop 7).toString
    (rest.trimAscii.toString).splitOn " " |>.filter (fun s => !s.isEmpty)

/--
Find a line in /proc/self/status starting with the given prefix.
-/
def findLine (content : String) (pref : String) : Option String :=
  let lines := content.splitOn "\n"
  lines.find? (fun l => String.startsWith l pref)

/--
Format id output: "uid=N(NAME) gid=N(NAME) groups=N(NAME),..."
groupPairs is a list of (gid, name) pairs.
-/
def formatId (info : IdInfo) (uidName gidName : String) (groupPairs : List (String × String)) : String :=
  let uidStr := if uidName.isEmpty then info.uid else s!"{info.uid}({uidName})"
  let gidStr := if gidName.isEmpty then info.gid else s!"{info.gid}({gidName})"
  let groupStrs := groupPairs.map (fun (gid, name) =>
    if name.isEmpty then gid else s!"{gid}({name})"
  )
  s!"uid={uidStr} gid={gidStr} groups={String.intercalate "," groupStrs}"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- Parse Uid line from /proc/self/status. -/
example : parseUidLine "Uid:\t0\t0\t0\t0" = ["0","0","0","0"] := by
  native_decide

/-- Parse Gid line from /proc/self/status. -/
example : parseGidLine "Gid:\t1000\t1000\t1000\t1000" = ["1000","1000","1000","1000"] := by
  native_decide

/-- findLine can locate a line by prefix. -/
example : findLine "Uid:\t0\nGid:\t1000\n" "Gid:" = some "Gid:\t1000" := by
  native_decide

end Lentils.Id.Logic
