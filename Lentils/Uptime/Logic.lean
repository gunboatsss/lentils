/-
Uptime.Logic — Verified pure logic for `uptime`.
0BSD

Structure:
  1. State types      — UptimeInput (raw /proc/uptime content)
  2. Specification    — parseUptime, formatUptime (single implementation; no duplicate `impl` alias)
  3. Invariants       — parametric properties
  4. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `uptime` prints system uptime in human-readable form.
On Linux this reads /proc/uptime.
-/

import Lentils.Common.Spec
import Lentils.Common.Float

namespace Lentils.Uptime.Logic

open Lentils.Common.Spec
open Lentils.Common.Float

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for uptime: the raw /proc/uptime content.
-/
structure UptimeInput where
  content : String
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Parse /proc/uptime content. Returns the first token (uptime seconds) as a Float.
Returns 0.0 on failure.
-/
def parseUptime (content : String) : Float :=
  match content.splitOn " " with
  | [] => 0.0
  | secs :: _ =>
    let trimmed := secs.trimAscii.toString
    if trimmed.isEmpty then 0.0
    else
      match Lentils.Common.Float.parse trimmed with
      | some f => f
      | none => 0.0

/--
Format a number of seconds (as Nat) into a human-readable uptime string.
-/
def formatUptimeNat (total : Nat) : String :=
  let days := total / 86400
  let hours := (total % 86400) / 3600
  let minutes := (total % 3600) / 60
  if days > 0 then
    s!"{days} day{if days > 1 then "s" else ""}, {hours} hour{if hours > 1 && hours != 0 then "s" else ""}, {minutes} minute{if minutes > 1 && minutes != 0 then "s" else ""}"
  else if hours > 0 then
    s!"{hours} hour{if hours > 1 then "s" else ""}, {minutes} minute{if minutes > 1 then "s" else ""}"
  else
    s!"{minutes} minute{if minutes != 1 then "s" else ""}"

/--
Format a number of seconds (as Float) into a human-readable uptime string.
This is the version called by the IO wrapper.
-/
def formatUptime (seconds : Float) : String :=
  formatUptimeNat seconds.toUInt64.toNat

/--
Full pipeline: parse raw content and format.
-/
def format (input : UptimeInput) : String :=
  let seconds := parseUptime input.content
  formatUptime seconds

/--
Specification: format uptime from raw /proc/uptime.
-/
def spec (input : UptimeInput) : String :=
  format input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Zero seconds yields "0 minutes".
-/
theorem i_zero_seconds : formatUptimeNat 0 = "0 minutes" := by
  native_decide

/--
I2: 60 seconds (1 minute) yields "1 minute".
-/
theorem i_one_minute : formatUptimeNat 60 = "1 minute" := by
  native_decide

/--
I3: 120 seconds (2 minutes) yields "2 minutes".
-/
theorem i_two_minutes : formatUptimeNat 120 = "2 minutes" := by
  native_decide

/--
I4: 3600 seconds (1 hour) yields "1 hour, 0 minute".
-/
theorem i_one_hour : formatUptimeNat 3600 = "1 hour, 0 minute" := by
  native_decide

/--
I5: 86400 seconds (1 day) yields "1 day, 0 hour, 0 minute".
-/
theorem i_one_day : formatUptimeNat 86400 = "1 day, 0 hour, 0 minute" := by
  native_decide

/--
I8: formatUptimeNat for 3600 seconds (1 hour) has expected structure.
-/
theorem i_3600_formatted : formatUptimeNat 3600 = "1 hour, 0 minute" := by
  native_decide

/--
Sub-minute uptimes always render as "0 minutes" (parametric over all small inputs).
-/
theorem formatUptimeNat_small (n : Nat) (h : n < 60) :
    formatUptimeNat n = "0 minutes" := by
  unfold formatUptimeNat
  have hdays : n / 86400 = 0 := by omega
  have hhours : (n % 86400) / 3600 = 0 := by omega
  have hmins : (n % 3600) / 60 = 0 := by omega
  simp [hdays, hhours, hmins]
  rfl

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- 3661 seconds = 1 hour, 1 minute. -/
example : formatUptimeNat 3661 = "1 hour, 1 minute" := by
  native_decide

/-- 2 days = "2 days, 0 hour, 0 minute". -/
example : formatUptimeNat 172800 = "2 days, 0 hour, 0 minute" := by
  native_decide

/-- 30 seconds = "0 minutes". -/
example : formatUptimeNat 30 = "0 minutes" := by
  native_decide

end Lentils.Uptime.Logic
