/-
Factor.Logic — Pure prime-factorization logic for `factor`. 0BSD

Trial division producing prime factors in ascending order with
multiplicity. Works on arbitrary-precision `Nat`.
-/

namespace Lentils.factor.Logic

/--
Factor `n` into prime factors (ascending, with multiplicity).
Returns `[]` for `n ≤ 1` (0 and 1 have no prime factors).
-/
def factorize (n : Nat) : List Nat :=
  if n ≤ 1 then []
  else
    let rec go (m : Nat) (p : Nat) (acc : List Nat) : List Nat :=
      if p * p > m then
        if m = 1 then acc.reverse else (m :: acc).reverse
      else if m % p = 0 then
        go (m / p) p (p :: acc)
      else
        go m (p + 1) acc
    termination_by (2 * m + if p ≤ m then m - p else 0)
    go n 2 []

/--
Format a number and its factors as a single line: `"n: f1 f2 ..."`.
-/
def formatFactorization (n : Nat) : String :=
  let fs := factorize n
  if fs.isEmpty then s!"{n}:"
  else s!"{n}: {String.intercalate " " (fs.map toString)}"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : factorize 1 = [] := by native_decide
example : factorize 0 = [] := by native_decide
example : factorize 7 = [7] := by native_decide
example : factorize 12 = [2, 2, 3] := by native_decide
example : factorize 100 = [2, 2, 5, 5] := by native_decide

end Lentils.factor.Logic
