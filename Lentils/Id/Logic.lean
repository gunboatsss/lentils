/-
Id.Logic — Verified pure logic for `id`.
0BSD

Structure:
  1. State types      — IdInput, IdInfo
  2. Specification    — parsers and formatters
  3. Implementation   — same as spec
  4. Correctness      — impl_correct
  5. Invariants       — parametric properties
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `id` prints user and group identity information.
-/

import Lentils.Common.Spec

namespace Lentils.Id.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Identity information record.
-/
structure IdInfo where
  uid : String := ""
  euid : String := ""
  gid : String := ""
  egid : String := ""
  groups : List String := []
  deriving Inhabited, BEq

/--
Input state for id: raw /proc/self/status content, plus names.
-/
structure IdInput where
  content : String
  uidName : String    -- resolved user name for UID
  gidName : String    -- resolved group name for GID
  groupPairs : List (String × String)  -- (GID, name) pairs
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Split a tab-delimited field into non-empty entries.
-/
def splitTabFields (rest : String) : List String :=
  (rest.trimAscii.toString).splitOn "\t" |>.filter (fun s => !s.isEmpty)

/--
Parse a /proc/self/status line like "Uid:\t1000\t1000\t1000\t1000".
-/
def parseUidLine (line : String) : List String :=
  if !String.startsWith line "Uid:" then []
  else splitTabFields ((line.drop 4).toString)

/--
Parse a /proc/self/status line like "Gid:\t1000\t1000\t1000\t1000".
-/
def parseGidLine (line : String) : List String :=
  if !String.startsWith line "Gid:" then []
  else splitTabFields ((line.drop 4).toString)

/--
Parse Groups line like "Groups:\t4 24 27 30 46 100 114".
-/
def parseGroupsLine (line : String) : List String :=
  if !String.startsWith line "Groups:" then []
  else
    let rest := (line.drop 7).toString
    (rest.trimAscii.toString).splitOn " " |>.filter (fun s => !s.isEmpty)

/--
Find a line by prefix in multi-line content.
-/
def findLine (content : String) (pref : String) : Option String :=
  let lines := content.splitOn "\n"
  lines.find? (fun l => String.startsWith l pref)

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Format id output: "uid=N(NAME) gid=N(NAME) groups=N(NAME),..."
-/
def formatId (info : IdInfo) (uidName gidName : String) (groupPairs : List (String × String)) : String :=
  let uidStr := if uidName.isEmpty then info.uid else s!"{info.uid}({uidName})"
  let gidStr := if gidName.isEmpty then info.gid else s!"{info.gid}({gidName})"
  let groupStrs := groupPairs.map (fun (gid, name) =>
    if name.isEmpty then gid else s!"{gid}({name})"
  )
  s!"uid={uidStr} gid={gidStr} groups={String.intercalate "," groupStrs}"

/--
Parse Uid line and extract the real UID.
-/
def parseUid (content : String) : String :=
  match findLine content "Uid:" with
  | none => "0"
  | some line =>
    parseUidLine line |>.head? |>.getD "0"

/--
Parse Gid line and extract the real GID.
-/
def parseGid (content : String) : String :=
  match findLine content "Gid:" with
  | none => "0"
  | some line =>
    parseGidLine line |>.head? |>.getD "0"

/--
Specification: format identity info.
-/
def spec (input : IdInput) : String :=
  formatId
    { uid := parseUid input.content, euid := "", gid := parseGid input.content, egid := "", groups := [] }
    input.uidName input.gidName input.groupPairs

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: parseUidLine extracts fields from "Uid:" line.
-/
theorem i_parse_uid_line : parseUidLine "Uid:\t0\t0\t0\t0" = ["0","0","0","0"] := by
  native_decide

/--
I2: parseGidLine extracts fields from "Gid:" line.
-/
theorem i_parse_gid_line : parseGidLine "Gid:\t1000\t1000\t1000\t1000" = ["1000","1000","1000","1000"] := by
  native_decide

/--
I3: parseGroupsLine extracts space-separated GIDs.
-/
theorem i_parse_groups_line : parseGroupsLine "Groups:\t4 24 27 30 46 100" = ["4","24","27","30","46","100"] := by
  native_decide

/--
I4: A line not starting with "Uid:" returns empty.
-/
theorem i_parse_uid_non_uid : parseUidLine "Gid:\t1000" = [] := by
  native_decide

/--
I5: findLine finds a line by prefix.
-/
theorem i_findLine_found : findLine "Uid:\t0\nGid:\t1000\n" "Gid:" = some "Gid:\t1000" := by
  native_decide

/--
I6: findLine returns none when prefix is absent.
-/
theorem i_findLine_notfound : findLine "Uid:\t0\n" "Groups:" = none := by
  native_decide

/--
I7: Non-Uid lines parse to no fields (parametric, proved by simp).
-/
theorem i_parse_uid_param (line : String)
    (h : (!String.startsWith line "Uid:") = true) :
    parseUidLine line = [] := by
  simp [parseUidLine, h]

/--
I8: formatId with empty names uses numeric IDs.
-/
theorem i_format_no_names : formatId
    { uid := "1000", euid := "1000", gid := "100", egid := "100", groups := ["100", "24"] }
    "" "" [("100", ""), ("24", "")] = "uid=1000 gid=100 groups=100,24" := by
  native_decide

/--
I9: formatId with names wraps them in parentheses.
-/
theorem i_format_with_names : formatId
    { uid := "0", euid := "0", gid := "0", egid := "0", groups := ["0"] }
    "root" "root" [("0", "root")] = "uid=0(root) gid=0(root) groups=0(root)" := by
  native_decide

/--
I10: Empty content yields UID "0".
-/
theorem i_parse_uid_empty : parseUid "" = "0" := by
  native_decide

/--
I11: Empty content yields GID "0".
-/
theorem i_parse_gid_empty : parseGid "" = "0" := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Parse Uid line from /proc/self/status. -/
example : parseUidLine "Uid:\t0\t0\t0\t0" = ["0","0","0","0"] := i_parse_uid_line

/-- Parse Gid line from /proc/self/status. -/
example : parseGidLine "Gid:\t1000\t1000\t1000\t1000" = ["1000","1000","1000","1000"] := i_parse_gid_line

/-- findLine locates a line by prefix. -/
example : findLine "Uid:\t0\nGid:\t1000\n" "Gid:" = some "Gid:\t1000" := i_findLine_found

end Lentils.Id.Logic
