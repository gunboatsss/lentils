/-
Od.Logic — Pure octal dump formatting logic for `od`. 0BSD -/
namespace Lentils.Od.Logic

/--
Format a Nat as an octal string with a minimum width (zero-padded).
-/
def toOctal (n : Nat) (width : Nat) : String :=
  let rec go (m : Nat) (acc : List Char) : List Char :=
    if m = 0 then acc
    else go (m / 8) ((Char.ofNat ((m % 8) + 48)) :: acc)
  let digits := if n = 0 then ['0'] else go n []
  -- Pad to width
  let padding := List.replicate (width - digits.length) '0'
  String.ofList (padding ++ digits)

/--
Format a byte as a 3-digit octal string.
-/
def byteToOctal (b : UInt8) : String :=
  toOctal b.toNat 3

/--
Format a byte as a character for display (for `-c` format).
Non-printable bytes get C-style escapes.
-/
def byteToChar (b : UInt8) : String :=
  let n := b.toNat
  if n >= 32 && n <= 126 then
    String.ofList [Char.ofNat n]
  else
    let escapes : List (Nat × String) := [
      (0, "\\0"), (7, "\\a"), (8, "\\b"), (9, "\\t"),
      (10, "\\n"), (11, "\\v"), (12, "\\f"), (13, "\\r")
    ]
    match escapes.find? (λ (code, _) => code = n) with
    | some (_, esc) => esc
    | none => byteToOctal b

/--
Dump a byte array in traditional octal format (like `od -A o -t o1`).
Each line shows a 7-digit octal address offset and up to 16 bytes in octal.
-/
def octalDump (data : ByteArray) : String :=
  let len := data.size
  if len = 0 then "" else
  let lines := List.range ((len + 15) / 16) |>.map (λ lineIdx =>
    let addr := lineIdx * 16
    let addrStr := toOctal addr 7
    let bytes := List.range 16 |>.filterMap (λ i =>
      let idx := lineIdx * 16 + i
      if h : idx < len then some (data.get idx) else none)
    let octals := bytes.map (λ b => byteToOctal b)
    let body := String.intercalate " " octals
    addrStr ++ " " ++ body)
  String.intercalate "\n" lines

-- ─── Proofs ──────────────────────────────────────────────────────────────────

/-- toOctal 0 with width 3 is "000". -/
example : toOctal 0 3 = "000" := by
  native_decide

/-- toOctal 8 (decimal) with width 3 is "010" (8 = 0o10). -/
example : toOctal 8 3 = "010" := by
  native_decide
