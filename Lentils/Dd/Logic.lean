/-
Dd.Logic — Pure block processing logic for `dd`. 0BSD
-/

namespace Lentils.Dd.Logic

/-- Parse a number with optional multiplicative suffixes. -/
def parseSize (s : String) : Option Nat :=
  let (numPart, mult) :=
    if s.endsWith "c" then (s.dropEnd 1, 1)
    else if s.endsWith "w" then (s.dropEnd 1, 2)
    else if s.endsWith "b" then (s.dropEnd 1, 512)
    else if s.endsWith "kB" then (s.dropEnd 2, 1000)
    else if s.endsWith "K" then (s.dropEnd 1, 1024)
    else if s.endsWith "M" then (s.dropEnd 1, 1024 * 1024)
    else if s.endsWith "G" then (s.dropEnd 1, 1024 * 1024 * 1024)
    else if s.endsWith "T" then (s.dropEnd 1, 1024 * 1024 * 1024 * 1024)
    else (s, 1)
  match numPart.toNat? with
  | some n => some (n * mult)
  | none => none

structure Params where
  ibs : Nat := 512
  obs : Nat := 512
  bs : Nat := 512
  count : Option Nat := none
  seek : Nat := 0
  skip : Nat := 0
  conv : List String := []
  deriving Repr, BEq, DecidableEq

/-- Parse dd-style "key=value" arguments. -/
def parseArgs (args : List String) : Params :=
  let rec go (remaining : List String) (p : Params) : Params :=
    match remaining with
    | [] => p
    | arg :: rest =>
      let parts := arg.splitOn "="
      match parts with
      | ["ibs", v] => go rest { p with ibs := (parseSize v).getD 512 }
      | ["obs", v] => go rest { p with obs := (parseSize v).getD 512 }
      | ["bs", v] => go rest { p with bs := (parseSize v).getD 512 }
      | ["count", v] => go rest { p with count := parseSize v }
      | ["seek", v] => go rest { p with seek := (parseSize v).getD 0 }
      | ["skip", v] => go rest { p with skip := (parseSize v).getD 0 }
      | ["conv", v] => go rest { p with conv := v.splitOn "," }
      | _ => go rest p
  go args {}

def inputBlockSize (p : Params) : Nat :=
  if p.bs != 512 then p.bs else p.ibs

def outputBlockSize (p : Params) : Nat :=
  if p.bs != 512 then p.bs else p.obs

structure Result where
  inputRecords : Nat := 0
  outputRecords : Nat := 0
  inputBytes : Nat := 0
  outputBytes : Nat := 0
  deriving Repr, BEq, DecidableEq

/-- Process input bytes according to dd parameters. Uses partial for inner recursion. -/
partial def process (input : ByteArray) (p : Params) : ByteArray × Result :=
  let ibs := inputBlockSize p
  let _obs := outputBlockSize p
  let skipBytes := min (p.skip * ibs) input.size
  let afterSkip := input.extract skipBytes input.size
  let limited := match p.count with
    | some cnt => afterSkip.extract 0 (min (cnt * ibs) afterSkip.size)
    | none => afterSkip
  let rec readBlocks (offset : Nat) (acc : List ByteArray) : List ByteArray :=
    if offset + ibs ≤ limited.size then
      let block := limited.extract offset (offset + ibs)
      readBlocks (offset + ibs) (block :: acc)
    else
      if offset < limited.size then
        let block := limited.extract offset limited.size
        (block :: acc).reverse
      else
        acc.reverse
  let inBlocks := readBlocks 0 []
  let inputRecords := inBlocks.length
  let inputBytes := limited.size
  let processed : List ByteArray :=
    inBlocks.map (λ block =>
      let convBlock :=
        if p.conv.contains "lcase" then
          block.foldl (λ (acc : ByteArray) b =>
            acc.push (if b ≥ 0x41 && b ≤ 0x5A then b + 0x20 else b)
          ) ByteArray.empty
        else if p.conv.contains "ucase" then
          block.foldl (λ (acc : ByteArray) b =>
            acc.push (if b ≥ 0x61 && b ≤ 0x7A then b - 0x20 else b)
          ) ByteArray.empty
        else if p.conv.contains "swab" then
          let rec swap (i : Nat) (acc : ByteArray) : ByteArray :=
            if i + 1 < block.size then
              let b0 := block.get! i
              let b1 := block.get! (i + 1)
              swap (i + 2) ((acc.push b1).push b0)
            else if i < block.size then
              swap (i + 1) (acc.push (block.get! i))
            else acc
          swap 0 ByteArray.empty
        else
          block
      convBlock)
  let outputRecords := processed.length
  let outputBytes := (processed.map (·.size)).foldl (· + ·) 0
  let output := processed.foldl (λ acc b => acc ++ b) ByteArray.empty
  (output, { inputRecords, outputRecords, inputBytes, outputBytes })

/-- Format the dd status summary. -/
def formatSummary (r : Result) : String :=
  s!"{r.inputRecords}+0 records in\n{r.outputRecords}+0 records out\n{r.inputBytes} bytes ({r.inputBytes} B) copied"

-- ─── Proofs ──────────────────────────────────────────────────────────────────

example : parseSize "512" = some 512 := by native_decide
example : parseSize "1K" = some 1024 := by native_decide
example : parseSize "1M" = some 1048576 := by native_decide
example : parseArgs ["ibs=1024"] = { ibs := 1024, obs := 512, bs := 512, count := none, seek := 0, skip := 0, conv := [] } := by native_decide
example : parseArgs ["count=5"] = { ibs := 512, obs := 512, bs := 512, count := some 5, seek := 0, skip := 0, conv := [] } := by native_decide

end Lentils.Dd.Logic
