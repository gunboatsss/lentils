/-
Nproc.Logic — Pure processor-count logic for `nproc`. 0BSD -/
namespace Lentils.Nproc.Logic

def countProcessors (cpuinfo : String) : Nat :=
  let lines := cpuinfo.splitOn "\n"
  (lines.filter (λ (l : String) => l.startsWith "processor")).length

end Lentils.Nproc.Logic
