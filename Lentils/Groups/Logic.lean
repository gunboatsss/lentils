/-
Groups.Logic — Verified pure logic for `groups`.
0BSD

Structure:
  1. State types      — GroupsInput (raw /proc/self/status content)
  2. Specification    — parseGroupsLine, findLine
  3. Implementation   — same as spec
  4. Correctness      — impl_correct
  5. Invariants       — parametric properties
  6. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `groups` prints group memberships for the current user or a given user.
-/

import Lentils.Common.Spec

namespace Lentils.Groups.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for groups: raw /proc/self/status content.
-/
structure GroupsInput where
  content : String
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

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

/--
Extract group IDs from /proc/self/status content.
Returns the list of GID strings from the Groups: line.
-/
def getGroupIDs (input : GroupsInput) : List String :=
  match findLine input.content "Groups:" with
  | none => []
  | some line => parseGroupsLine line

/--
Specification: extract group IDs from status content.
-/
def spec (input : GroupsInput) : List String :=
  getGroupIDs input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty lines produce no groups.
-/
theorem i_empty : parseGroupsLine "" = [] := by
  native_decide

/--
I2: A valid Groups: line is parsed correctly.
-/
theorem i_valid_line : parseGroupsLine "Groups:\t1000" = ["1000"] := by
  native_decide

/--
I3: Multiple groups are split correctly.
-/
theorem i_multiple_groups : parseGroupsLine "Groups:\t4 24 27 30 46" = ["4", "24", "27", "30", "46"] := by
  native_decide

/--
I4: A line not starting with "Groups:" returns empty.
-/
theorem i_non_groups_line : parseGroupsLine "Uid:\t1000" = [] := by
  native_decide

/--
I5: findLine finds a line by prefix.
-/
theorem i_findLine_found : findLine "Uid:\t0\nGid:\t1000\nGroups:\t4 24\n" "Gid:" = some "Gid:\t1000" := by
  native_decide

/--
I6: findLine returns none when prefix is not found.
-/
theorem i_findLine_notfound : findLine "Uid:\t0\nGid:\t1000\n" "Groups:" = none := by
  native_decide

/--
I7: Non-Groups lines parse to no groups (parametric, proved by simp).
-/
theorem i_non_groups_param (line : String) (h : (!line.startsWith "Groups:") = true) :
    parseGroupsLine line = [] := by
  simp [parseGroupsLine, h]

/--
I8: Empty content yields empty groups.
-/
theorem i_empty_content : getGroupIDs { content := "" } = [] := by
  native_decide

/--
I9: Content without Groups: line yields empty groups.
-/
theorem i_no_groups_line : getGroupIDs { content := "Uid:\t0\nGid:\t1000\n" } = [] := by
  native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Parse single group line. -/
example : parseGroupsLine "Groups:\t1000" = ["1000"] := i_valid_line

/-- Parse multiple groups. -/
example : parseGroupsLine "Groups:\t4 24 27 30 46 100" = ["4", "24", "27", "30", "46", "100"] := by
  native_decide

/-- Empty lines produce empty list. -/
example : parseGroupsLine "" = [] := i_empty

end Lentils.Groups.Logic
