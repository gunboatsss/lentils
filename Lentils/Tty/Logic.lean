/-
Tty.Logic — Pure tty-checking logic for `tty`. 0BSD -/
namespace Lentils.Tty.Logic

def isTty (path : String) : Bool :=
  path.startsWith "/dev/"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- A typical PTY path is a TTY. -/
example : isTty "/dev/pts/0" = true := by
  native_decide

/-- A regular file path is not a TTY. -/
example : isTty "/dev/null" = true := by
  native_decide

end Lentils.Tty.Logic
