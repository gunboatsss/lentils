/-
Users.Logic — Verified pure logic for `users`.
0BSD

Structure:
  1. State types      — UsersInput (list of user names)
  2. Specification    — formatUsers (single implementation; no duplicate `impl` alias)
  3. Invariants       — parametric properties
  4. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

POSIX: `users` prints logged-in user names, space-separated.
-/

import Lentils.Common.Spec

namespace Lentils.Users.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for users: a list of user names.
-/
structure UsersInput where
  usernames : List String
  deriving Inhabited, BEq

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Format a list of usernames space-separated.
-/
def formatUsers (usernames : List String) : String :=
  String.intercalate " " usernames

/--
Specification: format the user list.
-/
def spec (input : UsersInput) : String :=
  formatUsers input.usernames

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Parametric Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty user list yields empty string.
-/
theorem i_empty : formatUsers [] = "" := by
  native_decide

/--
I2: A single user name is returned unchanged.
Parametric: ∀ s.
-/
theorem i_singleton (s : String) : formatUsers [s] = s := by
  simp [formatUsers]

/--
I3: Two user names are space-separated.
Parametric: ∀ s1 s2.
-/
theorem i_pair (s1 s2 : String) : formatUsers [s1, s2] = s1 ++ " " ++ s2 := by
  simp [formatUsers]

/--
I4: Three user names are space-separated.
Parametric: ∀ s1 s2 s3.
-/
theorem i_triple (s1 s2 s3 : String) :
    formatUsers [s1, s2, s3] = s1 ++ " " ++ s2 ++ " " ++ s3 := by
  simp [formatUsers, String.append_assoc]

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Empty user list yields empty string. -/
example : formatUsers [] = "" := i_empty

/-- Single user name. -/
example : formatUsers ["root"] = "root" := i_singleton "root"

/-- Two user names separated by space. -/
example : formatUsers ["root", "jane"] = "root jane" := i_pair "root" "jane"

end Lentils.Users.Logic
