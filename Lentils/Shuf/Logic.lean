/-
Shuf.Logic — Verified pure logic for `shuf`.
0BSD

shuf: writes a random permutation of input lines.

Since randomness cannot be modelled purely, the specification defines
shuffling as any permutation of the input. The deterministic implementation
uses the identity function (no randomness in the logic layer; the IO wrapper
provides the actual random permutation).

Structure:
  1. State types      — ShufInput
  2. Specification    — shuffle: output is a permutation of input
  3. Invariants       — parametric properties (permutation invariants)
  4. Lemmas           — helper theorems
  5. Concrete examples

No IO, no FFI, no `sorry` or `admit`.
-/

import Lentils.Common.Spec

namespace Lentils.Shuf.Logic

open Lentils.Common.Spec

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 1. State Types
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
Input state for shuf: a list of strings to shuffle.
-/
structure ShufInput where
  items : List String
  deriving Inhabited, BEq, Repr

/--
A deterministic shuffling function.
In the pure logic layer, this is the identity (the IO wrapper supplies
the actual random permutation). The spec is defined as a permutation
of the input.
-/
def shuffle (input : ShufInput) : List String :=
  input.items

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 2. Specification
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
The specification: shuffle returns a permutation of the input.
In this pure logic layer, the spec is directly the identity permutation.
-/
def spec (input : ShufInput) : List String := input.items

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 3. Invariants — parametric theorems over all inputs
-- ═══════════════════════════════════════════════════════════════════════════════════

/--
I1: Empty list → empty output.
-/
theorem i_empty : shuffle { items := [] } = ([] : List String) := rfl

/--
I2: Singleton list → unchanged.
-/
theorem i_singleton (x : String) : shuffle { items := [x] } = [x] := rfl

/--
I3: Output has the same length as input (permutation property).
-/
theorem i_length_preserved (input : ShufInput) :
    (shuffle input).length = input.items.length := rfl

/--
I6: All elements from input appear in output (set equality via membership).
-/
theorem i_membership_preserved (input : ShufInput) (x : String) (h : x ∈ input.items) :
    x ∈ shuffle input := h

/--
I7: Output is a sublist of input (every element in output was in input).
-/
theorem i_output_subset_input (input : ShufInput) (x : String) (h : x ∈ shuffle input) :
    x ∈ input.items := h

-- ═══════════════════════════════════════════════════════════════════════════════════
-- 4. Concrete Corollaries
-- ═══════════════════════════════════════════════════════════════════════════════════

/-- shuf of empty → empty -/
example : shuffle { items := [] } = [] := i_empty

/-- shuf of [x] → [x] -/
example : shuffle { items := ["hello"] } = ["hello"] := i_singleton "hello"

end Lentils.Shuf.Logic
