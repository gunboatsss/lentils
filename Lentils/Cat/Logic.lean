/-
Cat.Logic — Verified pure logic for `cat`.
0BSD

Structure:
  1. State types      — CatInput (list of byte arrays)
  2. Specification    — concat
  3. Invariants       — parametric properties with proofs
  4. Concrete examples

No IO, no FFI, no `sorry` or `admit`.

cat concatenates byte sequences in order.
The core operation is list fold with ByteArray.append,
which is associative with identity ByteArray.empty.
-/

import Lentils.Common.Spec

namespace Lentils.Cat.Logic

open Lentils.Common.Spec
open ByteArray

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input to cat: a list of byte arrays to concatenate.
-/
abbrev CatInput := List ByteArray

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification = Implementation
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Concatenate a list of ByteArrays in order.
-/
def concat (inputs : CatInput) : ByteArray :=
  inputs.foldl (· ++ ·) ByteArray.empty

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Structural Lemma
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Lemma: foldl (++) (x ++ y) ys = x ++ foldl (++) y ys.
-/
private theorem foldl_append_left (x y : ByteArray) (ys : List ByteArray) :
    List.foldl (· ++ ·) (x ++ y) ys = x ++ List.foldl (· ++ ·) y ys := by
  induction ys generalizing x y with
  | nil => rfl
  | cons z zs ih =>
      simp [List.foldl]
      rw [ByteArray.append_assoc (a := x) (b := y) (c := z)]
      exact ih x (y ++ z)

/--
concat (x :: xs) = x ++ concat xs.
-/
theorem concat_cons (x : ByteArray) (xs : CatInput) : concat (x :: xs) = x ++ concat xs := by
  simp [concat, List.foldl]
  have h := foldl_append_left x ByteArray.empty xs
  simpa using h

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Core Invariants
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty list → empty ByteArray.
-/
theorem i_empty : concat [] = ByteArray.empty := rfl

/--
I2: Singleton → that byte array. Parametric: ∀ ba.
-/
theorem i_singleton (ba : ByteArray) : concat [ba] = ba := by
  simp [concat]

/--
I3: Pair → concatenation. Parametric: ∀ ba1 ba2.
-/
theorem i_pair (ba1 ba2 : ByteArray) : concat [ba1, ba2] = ba1 ++ ba2 := by
  simp [concat]

/--
I4: Triple → left-associative concatenation.
-/
theorem i_triple (ba1 ba2 ba3 : ByteArray) :
    concat [ba1, ba2, ba3] = (ba1 ++ ba2) ++ ba3 := by
  simp [concat]

/--
I5: Associativity — concat (xs ++ ys) = concat xs ++ concat ys.
This is the fundamental algebraic law of cat.
-/
theorem i_assoc (xs ys : CatInput) :
    concat (xs ++ ys) = concat xs ++ concat ys := by
  induction xs with
  | nil => simp [concat]
  | cons x xs ih =>
      have h_cons_cat : concat (x :: (xs ++ ys)) = x ++ concat (xs ++ ys) :=
        concat_cons x (xs ++ ys)
      calc
        concat ((x :: xs) ++ ys) = concat (x :: (xs ++ ys)) := rfl
        _ = x ++ concat (xs ++ ys) := h_cons_cat
        _ = x ++ (concat xs ++ concat ys) := by rw [ih]
        _ = (x ++ concat xs) ++ concat ys :=
          (ByteArray.append_assoc (a := x) (b := concat xs) (c := concat ys)).symm
        _ = concat (x :: xs) ++ concat ys := by rw [concat_cons x xs]

/--
I6: Empty inputs don't affect output.
-/
theorem i_empty_neutral (xs : CatInput) : concat (xs ++ [ByteArray.empty]) = concat xs := by
  rw [i_assoc]
  simp [concat]

/--
I7: Length of concat = sum of input sizes.
-/
theorem i_length (xs : CatInput) :
    (concat xs).size = (xs.map ByteArray.size).sum := by
  induction xs with
  | nil => simp [concat]
  | cons x xs ih =>
      rw [concat_cons x xs]
      rw [ByteArray.size_append]
      simp [ih]

/--
I9: If concat of two singleton lists are equal, the elements are equal
(follows directly from i_singleton).
-/
theorem i_injective_singleton (ba1 ba2 : ByteArray) (h : concat [ba1] = concat [ba2]) :
    ba1 = ba2 := by
  simpa [i_singleton] using h

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 5. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- Cat of empty list is empty -/
example : concat [] = ByteArray.empty := i_empty

/-- Cat of a single byte array -/
example (ba : ByteArray) : concat [ba] = ba := i_singleton ba

/-- Cat of two byte arrays -/
example (a b : ByteArray) : concat [a, b] = a ++ b := i_pair a b

/-- Cat of three byte arrays -/
example (a b c : ByteArray) : concat [a, b, c] = (a ++ b) ++ c := i_triple a b c

/-- Associativity example -/
example (a b c d : ByteArray) :
    concat [a, b] ++ concat [c, d] = concat [a, b, c, d] := by
  calc
    concat [a, b] ++ concat [c, d] = concat ([a, b] ++ [c, d]) := by rw [i_assoc]
    _ = concat [a, b, c, d] := rfl

end Lentils.Cat.Logic
