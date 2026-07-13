/-
Users.Logic — Pure user-listing logic for `users`. 0BSD -/
namespace Lentils.Users.Logic

/--
Format a list of usernames space-separated.
-/
def formatUsers (usernames : List String) : String :=
  String.intercalate " " usernames

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- Empty user list yields empty string. -/
example : formatUsers [] = "" := by
  native_decide

/-- Single user name. -/
example : formatUsers ["root"] = "root" := by
  native_decide

/-- Two user names separated by space. -/
example : formatUsers ["root", "jane"] = "root jane" := by
  native_decide

end Lentils.Users.Logic
