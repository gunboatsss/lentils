/-
Nproc — IO wrapper for the `nproc` utility. 0BSD -/
import Lentils.Nproc.Logic

namespace Lentils.Nproc

open Logic

def countCpuStat (stat : String) : Nat :=
  let lines := stat.splitOn "\n"
  -- Count lines like "cpu0", "cpu1", ... but not "cpu" (aggregate)
  (lines.filter (λ (l : String) =>
    l.startsWith "cpu" && !l.startsWith "cpu ")).length

def run (_args : List String) : IO UInt32 := do
  let count ←
    try
      let content ← IO.FS.readFile "/proc/cpuinfo"
      pure (countProcessors content)
    catch _ =>
      try
        let stat ← IO.FS.readFile "/proc/stat"
        pure (countCpuStat stat)
      catch _ =>
        pure 1
  let count := if count == 0 then 1 else count
  IO.println (toString count)
  return 0

end Lentils.Nproc
