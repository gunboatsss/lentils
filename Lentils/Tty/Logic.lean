/-
Tty.Logic — Verified pure logic for `tty`.
0BSD

Structure:
  1. State types      — TtyInput (path string)
  2. Specification    — isTty (single implementation; no duplicate `impl` alias)
  3. Invariants       — parametric properties
  4. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `tty` prints the terminal file name.
Returns 0 if stdin is a terminal, 1 otherwise.
-/

import Lentils.Common.Spec

namespace Lentils.Tty.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for tty: the path to check.
-/
structure TtyInput where
  path : String
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Check if a path identifies a TTY device.
A path is considered a TTY if it starts with "/dev/".
-/
def isTty (path : String) : Bool :=
  path.startsWith "/dev/"

/--
Return the output string: the path if it's a TTY, "not a tty" otherwise.
-/
def format (input : TtyInput) : String :=
  if isTty input.path then input.path else "not a tty"

/--
Specification: determine and format TTY status.
-/
def spec (input : TtyInput) : String :=
  format input

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I2: /dev/pts/0 is a TTY.
-/
theorem i_dev_pts0_is_tty : isTty "/dev/pts/0" = true := by
  native_decide

/--
I3: /etc/passwd is not a TTY.
-/
theorem i_etc_passwd_not_tty : isTty "/etc/passwd" = false := by
  native_decide

/--
I4: A TTY path is returned unchanged by format.
-/
theorem i_format_dev_pts0 : format { path := "/dev/pts/0" } = "/dev/pts/0" := by
  native_decide

/--
I5: A non-TTY path yields "not a tty".
-/
theorem i_format_notty : format { path := "/etc/passwd" } = "not a tty" := by
  native_decide

/--
I6: The empty path is not a TTY.
-/
theorem i_empty_not_tty : format { path := "" } = "not a tty" := by
  native_decide

/--
I6: /dev/null is considered a TTY by path convention.
-/
theorem i_dev_null : format { path := "/dev/null" } = "/dev/null" := by
  native_decide

/--
Any path under `/dev/` is a TTY (parametric over all suffixes).
-/
theorem isTty_dev_prefix (s : String) : isTty ("/dev/" ++ s) = true := by
  simp [isTty]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- A typical PTY path is a TTY. -/
example : isTty "/dev/pts/0" = true := by
  native_decide

/-- /dev/null is a TTY (by path convention, not by actual property). -/
example : isTty "/dev/null" = true := by
  native_decide

/-- A regular file path is not a TTY. -/
example : isTty "/etc/passwd" = false := by
  native_decide

end Lentils.Tty.Logic
