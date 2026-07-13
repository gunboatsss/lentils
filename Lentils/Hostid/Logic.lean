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
  let hexChars := String.mk (trimmed.toList.filter (λ (c : Char) =>
    (c ≥ '0' && c ≤ '9') || (c ≥ 'a' && c ≤ 'f') || (c ≥ 'A' && c ≤ 'F')))
  if hexChars.isEmpty then "00000000"
  else hexChars

end Lentils.Hostid.Logic
