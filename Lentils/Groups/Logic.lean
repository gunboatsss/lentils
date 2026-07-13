/-
Groups.Logic — Pure group-listing logic for `groups`. 0BSD -/
namespace Lentils.Groups.Logic

/--
Parse the "Groups:" line from /proc/self/status into a list of numeric GID strings.
-/
def parseGroupsLine (line : String) : List String :=
  if !line.startsWith "Groups:" then []
  else
    let rest := (line.drop 7).toString  -- skip "Groups:"
    (rest.trimAscii.toString).splitOn " " |>.filter (fun s => !s.isEmpty)

/--
Find a line in /proc/self/status starting with the given prefix.
-/
def findLine (content : String) (pref : String) : Option String :=
  let lines := content.splitOn "\n"
  lines.find? (fun l => String.startsWith l pref)

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- Parse single group line. -/
example : parseGroupsLine "Groups:\t1000" = ["1000"] := by
  native_decide

/-- Parse empty groups line. -/
example : parseGroupsLine "" = [] := by
  native_decide

end Lentils.Groups.Logic
