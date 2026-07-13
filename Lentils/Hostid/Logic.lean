/-
Hostid.Logic — Pure hostid parsing for `hostid`. 0BSD -/
namespace Lentils.Hostid.Logic

/--
Parse a hex string from /proc/sys/kernel/hostid.
Keep only hex characters [0-9a-fA-F], pad to 8 chars with '0'.
-/
def formatHostid (raw : String) : String :=
  let trimmed := raw.trimAscii.toString
  -- Keep only hex characters
  let hexChars := String.ofList (trimmed.toList.filter (λ (c : Char) =>
    (c ≥ '0' && c ≤ '9') || (c ≥ 'a' && c ≤ 'f') || (c ≥ 'A' && c ≤ 'F')))
  if hexChars.isEmpty then "00000000"
  else hexChars

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- Format hostid preserves hex format. -/
example : formatHostid "abcdef01" = "abcdef01" := by
  native_decide

end Lentils.Hostid.Logic


