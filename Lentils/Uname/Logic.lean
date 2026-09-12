/-
Uname.Logic — Verified pure logic for `uname`.
0BSD

Structure:
  1. State types      — UnameInput (info + flags)
  2. Specification    — format functions (single implementation; no duplicate `impl` alias)
  3. Invariants       — parametric properties
  4. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `uname` prints system information.
-/

import Lentils.Common.Spec

namespace Lentils.Uname.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
System information record.
-/
structure UnameInfo where
  sysname  : String := ""
  nodename : String := ""
  release  : String := ""
  version  : String := ""
  machine  : String := ""
  deriving Inhabited, BEq

/--
Input state for uname: the system info and command-line flags.
-/
structure UnameInput where
  info : UnameInfo
  flags : List String  -- e.g., ["-a"], ["-s"], ["-n"]
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Format helpers
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Format all fields: "sysname nodename release version machine".
-/
def formatAll (info : UnameInfo) : String :=
  s!"{info.sysname} {info.nodename} {info.release} {info.version} {info.machine}"

def formatSysname (info : UnameInfo) : String := info.sysname
def formatNodename (info : UnameInfo) : String := info.nodename
def formatRelease (info : UnameInfo) : String := info.release
def formatVersion (info : UnameInfo) : String := info.version
def formatMachine (info : UnameInfo) : String := info.machine

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Apply flags to select the output field(s).
If no flag is given, default to sysname.
Supports: -a, -s, -n, -r, -v, -m.
-/
def format (input : UnameInput) : String :=
  if input.flags.isEmpty then
    formatSysname input.info
  else
    let hasA := input.flags.contains "-a" || input.flags.contains "--all"
    if hasA then
      formatAll input.info
    else
      let parts := input.flags.filterMap λ f =>
        match f with
        | "-s" | "--kernel-name"    => some (formatSysname input.info)
        | "-n" | "--nodename"       => some (formatNodename input.info)
        | "-r" | "--kernel-release" => some (formatRelease input.info)
        | "-v" | "--kernel-version" => some (formatVersion input.info)
        | "-m" | "--machine"        => some (formatMachine input.info)
        | _ => none
      if parts.isEmpty then
        formatSysname input.info
      else
        String.intercalate " " parts

/--
Specification: format system info according to flags.
-/
def spec (input : UnameInput) : String :=
  format input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Without flags, format returns sysname.
-/
theorem i_default_sysname (info : UnameInfo) :
    format { info := info, flags := [] } = info.sysname := rfl

/--
I2: With -s, format returns sysname.
-/
theorem i_flag_sysname (info : UnameInfo) :
    format { info := info, flags := ["-s"] } = info.sysname := rfl

/--
I3: With -n, format returns nodename.
-/
theorem i_flag_nodename (info : UnameInfo) :
    format { info := info, flags := ["-n"] } = info.nodename := rfl

/--
I4: With -r, format returns release.
-/
theorem i_flag_release (info : UnameInfo) :
    format { info := info, flags := ["-r"] } = info.release := rfl

/--
I5: With -v, format returns version.
-/
theorem i_flag_version (info : UnameInfo) :
    format { info := info, flags := ["-v"] } = info.version := rfl

/--
I6: With -m, format returns machine.
-/
theorem i_flag_machine (info : UnameInfo) :
    format { info := info, flags := ["-m"] } = info.machine := rfl

/--
I7: With -a, format returns all fields space-separated.
-/
theorem i_flag_all (info : UnameInfo) :
    format { info := info, flags := ["-a"] } =
    s!"{info.sysname} {info.nodename} {info.release} {info.version} {info.machine}" := rfl

/--
I8: With --all, format returns all fields.
-/
theorem i_flag_all_long (info : UnameInfo) :
    format { info := info, flags := ["--all"] } = formatAll info := rfl

/--
I9: Multiple specific flags produce the corresponding fields.
-/
theorem i_multiple_flags (info : UnameInfo) :
    format { info := info, flags := ["-s", "-n"] } =
    info.sysname ++ " " ++ info.nodename := rfl

/--
Default (no-flag) output unfolds to `formatSysname` (parametric, proved by simp).
-/
theorem format_default_unfold (info : UnameInfo) :
    format { info := info, flags := [] } = formatSysname info := by
  simp [format]

/--
I11: -a is equivalent to -s -n -r -v -m combined.
-/
theorem i_a_equivalence (info : UnameInfo) :
    format { info := info, flags := ["-a"] } =
    format { info := info, flags := ["-s", "-n", "-r", "-v", "-m"] } := rfl

/--
I12: field access lemma: formatSysname extracts sysname.
-/
theorem i_sysname_eq (s : String) :
    formatSysname { sysname := s, nodename := "", release := "", version := "", machine := "" } = s := rfl

/--
I13: field access lemma: formatNodename extracts nodename.
-/
theorem i_nodename_eq (s : String) :
    formatNodename { sysname := "", nodename := s, release := "", version := "", machine := "" } = s := rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- formatSysname returns the sysname field. -/
example : formatSysname { sysname := "Linux", nodename := "", release := "", version := "", machine := "" } = "Linux" := by
  native_decide

/-- formatNodename returns the nodename field. -/
example : formatNodename { sysname := "", nodename := "myhost", release := "", version := "", machine := "" } = "myhost" := by
  native_decide

end Lentils.Uname.Logic
