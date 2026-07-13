/-
Nproc.Logic — Pure processor-count logic for `nproc`. 0BSD -/
namespace Lentils.Nproc.Logic

def countProcessors (cpuinfo : String) : Nat :=
  let lines := cpuinfo.splitOn "\n"
  (lines.filter (λ (l : String) => l.startsWith "processor")).length

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- Empty cpuinfo yields 0 processors. -/
example : countProcessors "" = 0 := by
  native_decide

/-- One processor entry. -/
example : countProcessors "processor	: 0
" = 1 := by
  native_decide

end Lentils.Nproc.Logic


