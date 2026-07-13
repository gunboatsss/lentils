/-
Readlink.Logic - Pure logic for readlink. 0BSD -/
namespace Lentils.Readlink.Logic

/--
Validate that a path argument is provided.
-/
def getPath (args : List String) : Option String :=
  args.head?

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- getPath returns the first argument. -/
example : getPath ["/some/path"] = some "/some/path" := by
  native_decide

/-- getPath on empty args returns none. -/
example : getPath [] = none := by
  native_decide

end Lentils.Readlink.Logic
