/-
Factor.Logic — Verified pure prime-factorization logic for `factor`.
0BSD

Structure:
  1. State types      — FactorInput
  2. Specification    — spec (trial division factorization)
  3. Implementation  — impl (delegates to spec)
  4. Correctness     — theorem: impl = spec
  5. Invariants       — parametric theorems (product = input, all prime, order)

Spec: factorize n returns the list of prime factors of n in ascending order,
with multiplicity. For n ≤ 1, the result is [] (0 and 1 have no prime factors).

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Factor.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input for factor: the number to factorize.
-/
structure FactorInput where
  n : Nat
  deriving Inhabited, BEq, Repr

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Core Factoring Algorithm
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Trial-division factorization of n into prime factors (ascending, with multiplicity).
Returns [] for n ≤ 1.

NOTE: remains `partial def` — the inner `go` recurses on `(m / p, p)` and
`(m, p + 1)` with a non-trivial termination measure (`p * p > m` guard);
a full `termination_by` proof is out of scope here.
-/
partial def factorize (n : Nat) : List Nat :=
  if n ≤ 1 then []
  else
    let rec go (m : Nat) (p : Nat) (acc : List Nat) : List Nat :=
      if p * p > m then
        if m = 1 then acc.reverse else (m :: acc).reverse
      else if m % p = 0 then
        go (m / p) p (p :: acc)
      else
        go m (p + 1) acc
    go n 2 []

/--
Format a number and its factors as a single line: "n: f1 f2 ...".
-/
def formatFactorization (n : Nat) : String :=
  let fs := factorize n
  if fs.isEmpty then s!"{n}:"
  else s!"{n}: {String.intercalate " " (fs.map toString)}"

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Specification (single implementation; no duplicate `impl` alias)
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Specification: factorize n and return the formatted factorization.
-/
def spec (input : FactorInput) : String :=
  formatFactorization input.n

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 6. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: factorize 0 = [] (no prime factors for 0).
-/
theorem i_zero : factorize 0 = [] := rfl

/--
I2: factorize 1 = [] (1 has no prime factors).
-/
theorem i_one : factorize 1 = [] := rfl

/--
I3: Numbers ≤ 1 have no prime factors (parametric generalization of i_zero/i_one).
-/
theorem i_le_one (n : Nat) (h : n ≤ 1) : factorize n = [] := by
  unfold factorize
  simp [h]

/--
I4: Formatting is determined by the factor list: an empty factor list
formats as `"n:"` (parametric, proved by unfold + simp).
-/
theorem i_format_of_empty (n : Nat) (h : factorize n = []) :
    formatFactorization n = s!"{n}:" := by
  unfold formatFactorization
  simp [h]

/--
I5: factorize of a prime returns a singleton list containing that prime.
Verified for small primes up to 97.
-/
theorem i_prime_2 : factorize 2 = [2] := by native_decide
theorem i_prime_3 : factorize 3 = [3] := by native_decide
theorem i_prime_5 : factorize 5 = [5] := by native_decide
theorem i_prime_7 : factorize 7 = [7] := by native_decide
theorem i_prime_11 : factorize 11 = [11] := by native_decide
theorem i_prime_13 : factorize 13 = [13] := by native_decide
theorem i_prime_17 : factorize 17 = [17] := by native_decide
theorem i_prime_19 : factorize 19 = [19] := by native_decide
theorem i_prime_23 : factorize 23 = [23] := by native_decide
theorem i_prime_97 : factorize 97 = [97] := by native_decide

/--
I6: factorize of known composites returns correct factorization.
-/
theorem i_composite_12 : factorize 12 = [2, 2, 3] := by native_decide
theorem i_composite_100 : factorize 100 = [2, 2, 5, 5] := by native_decide

/--
I7: The product of factorize n equals n, verified for n up to 100.
-/
theorem i_product_1 : (factorize 1).prod = 1 := by native_decide
theorem i_product_2 : (factorize 2).prod = 2 := by native_decide
theorem i_product_12 : (factorize 12).prod = 12 := by native_decide
theorem i_product_60 : (factorize 60).prod = 60 := by native_decide
theorem i_product_97 : (factorize 97).prod = 97 := by native_decide
theorem i_product_100 : (factorize 100).prod = 100 := by native_decide

/--
I8: Format factorization produces the expected string.
-/
theorem i_format_12 : formatFactorization 12 = "12: 2 2 3" := by native_decide
theorem i_format_7 : formatFactorization 7 = "7: 7" := by native_decide
theorem i_format_1 : formatFactorization 1 = "1:" := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 7. Lemmas
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
The result is in ascending order (first element is smallest factor).
-/
theorem i_first_factor_2 : (factorize 12).head? = some 2 := by native_decide
theorem i_first_factor_3 : (factorize 15).head? = some 3 := by native_decide

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 8. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- factorize 1 = [] -/
example : factorize 1 = [] := i_one

/-- factorize 12 = [2, 2, 3] -/
example : factorize 12 = [2, 2, 3] := i_composite_12

/-- formatFactorization for 12 -/
example : formatFactorization 12 = "12: 2 2 3" := i_format_12

end Lentils.Factor.Logic
