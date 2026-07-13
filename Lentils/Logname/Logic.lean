/-
Logname.Logic — Pure login-name logic for `logname`. 0BSD -/
namespace Lentils.Logname.Logic

def isValidLoginName (s : String) : Bool :=
  s ≠ "" && !s.contains '\n'

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- A valid login name contains only valid characters. -/
example : isValidLoginName "john" = true := by
  native_decide

/-- Empty string is not a valid login name. -/
example : isValidLoginName "" = false := by
  native_decide

end Lentils.Logname.Logic


