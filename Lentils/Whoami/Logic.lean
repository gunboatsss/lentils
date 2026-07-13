/-
Whoami.Logic — Pure identity logic for `whoami`. 0BSD -/
namespace Lentils.Whoami.Logic

def isValidUserName (s : String) : Bool :=
  s ≠ "" && !s.contains '\n'

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- A valid user name. -/
example : isValidUserName "root" = true := by
  native_decide

/-- Empty string is not a valid user name. -/
example : isValidUserName "" = false := by
  native_decide

end Lentils.Whoami.Logic


