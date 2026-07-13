/-
Common.Array — Verified array utilities for lean-coreutils.
0BSD

Provides shared array-safe access helpers used across multiple utilities.
Eliminates code duplication (arrGet was defined in 4 separate modules).
-/

namespace Lentils.Common.Array

/--
Get a UInt64 from an Array at index i, with default 0 if out of bounds.
-/
def arrGet (arr : Array UInt64) (i : Nat) : UInt64 :=
  if h : i < arr.size then arr[i] else 0

/--
Get a UInt64 from an Array at index i, with a specified default if out of bounds.
-/
def arrGetD (arr : Array UInt64) (i : Nat) (default : UInt64) : UInt64 :=
  if h : i < arr.size then arr[i] else default

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- arrGet of empty array is 0. -/
theorem arrGet_empty (i : Nat) : arrGet (Array.empty : Array UInt64) i = 0 := rfl

/-- arrGet at index 0 returns first element. -/
example : arrGet (List.toArray [(42 : UInt64), (99 : UInt64)]) 0 = 42 := rfl

/-- arrGet out of bounds returns 0. -/
example : arrGet (List.toArray [(42 : UInt64)]) 5 = 0 := rfl

/-- arrGetD returns default for out-of-bounds access. -/
example : arrGetD (List.toArray [(42 : UInt64)]) 5 99 = 99 := rfl

end Lentils.Common.Array
