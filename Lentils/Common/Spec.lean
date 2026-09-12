/-
Common.Spec — Formal specification framework for lean-coreutils.
0BSD

Provides the shared vocabulary and patterns for writing verified coreutils:
- State types (input flags, arguments, output)
- Specification vs implementation separation
- Common proof tactics and lemma patterns

Each utility follows this structure in its Logic.lean:

  1. State types    — define input state (flags + args), output, intermediate states
  2. Specification  — the "what": a pure function defining correct behavior
  3. Implementation — the "how": a pure function that computes the specification
  4. Correctness     — theorem: ∀ input, implementation input = specification input
  5. Invariants      — parametric properties that hold for all inputs
  6. Concrete tests  — derived corollaries for documentation (optional)

No `sorry` or `admit` allowed anywhere in the codebase.
-/

namespace Lentils.Common.Spec

/--
Exit code type. 0 = success, non-zero = failure.
-/
abbrev ExitCode := UInt32

/--
Default success exit code.
-/
def exitSuccess : ExitCode := 0
def exitFailure : ExitCode := 1

/--
The type of a pure utility implementation:
takes input state, returns (output, exit code).
-/
abbrev PureUtil α β := α → β

/--
Specification correctness statement:
the implementation always produces the specification's result.
-/
def specCorrect {α β : Type} [BEq β] (spec impl : α → β) : Prop :=
  ∀ x, impl x = spec x

/--
Specification correctness for utilities with exit codes:
implementation returns the same output and exit code as spec.
-/
def specCorrectWithExit {α β : Type} [BEq β] (spec impl : α → β × ExitCode) : Prop :=
  ∀ x, impl x = spec x

/--
Inhabited instance for ExitCode to satisfy typeclass requirements.
-/
instance : Inhabited ExitCode := ⟨0⟩

/--
A verified utility bundles its spec, implementation, and proof of correctness.
-/
structure VerifiedUtil (α β : Type) [BEq β] where
  spec : α → β
  impl : α → β
  correct : ∀ x, impl x = spec x

/--
A verified utility with exit codes.
-/
structure VerifiedUtilE (α β : Type) [BEq β] where
  spec : α → β × ExitCode
  impl : α → β × ExitCode
  correct : ∀ x, impl x = spec x

/--
Trivial proof that identical functions satisfy specCorrect.
-/
theorem specCorrect_rfl {α β : Type} [BEq β] (f : α → β) : specCorrect f f :=
  λ _ => rfl

/--
Composition: if f = g and g satisfies spec h, then f satisfies spec h.
-/
theorem specCorrect_trans {α β : Type} [BEq β] (f g h : α → β)
    (h1 : ∀ x, f x = g x) (h2 : specCorrect h g) : specCorrect h f :=
  λ x => by
    rw [h1 x, h2 x]

end Lentils.Common.Spec
