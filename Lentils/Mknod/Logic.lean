/-
Mknod.Logic — Pure logic for the `mknod` utility.
0BSD

Contains only pure functions: argument parsing. No IO is performed here.
All filesystem interaction lives in `mknod.lean` via the C FFI `mknod(2)`.

Provenance: POSIX.1-2017, Section "mknod — make block/character special files".
No GPL source was consulted.
-/

namespace Lentils.Mknod.Logic

/--
The type of device node to create.
-/
inductive NodeType
  | block
  | character
  deriving Repr, BEq, DecidableEq

/--
Options controlling `mknod` behaviour.
-/
structure Options where
  nodeType : NodeType := .character
  mode : UInt32 := 0o666
  major : UInt32 := 0
  minor : UInt32 := 0
  verbose : Bool := false
  deriving Repr, BEq, DecidableEq

/--
Parse type string: 'b' → block, anything else → character.
-/
def parseType (s : String) : NodeType :=
  if s = "b" then .block else .character

/--
Parse a decimal number string. Returns 0 on failure.
-/
def parseNum (s : String) : UInt32 :=
  match s.toNat? with
  | some n => UInt32.ofNat n
  | none => 0

/--
Parse `mknod` arguments into `(options, name)`.

  mknod [-m MODE] NAME TYPE MAJOR MINOR

TYPE is 'b' for block or 'c'/'u' for character.
-/
def parseArgs (args : List String) : Option (Options × String) :=
  let rec go (remaining : List String) (opts : Options) : Option (Options × String) :=
    match remaining with
    | [] => none
    | "--" :: rest => go rest opts
    | "-m" :: modeStr :: rest => go rest { opts with mode := parseNum modeStr }
    | "--mode" :: modeStr :: rest => go rest { opts with mode := parseNum modeStr }
    | "-v" :: rest => go rest { opts with verbose := true }
    | "--verbose" :: rest => go rest { opts with verbose := true }
    | s :: rest =>
      if s.startsWith "-" && s != "-" then
        none
      else
        -- First non-flag is name, expect TYPE MAJOR MINOR after it
        let name := s
        match rest with
        | typeStr :: majorStr :: minorStr :: _ =>
          some ({ opts with
            nodeType := parseType typeStr
            major := parseNum majorStr
            minor := parseNum minorStr
          }, name)
        | _ => none
  go args {}

-- ─── Theorems ──────────────────────────────────────────────────────────────────

theorem parse_char :
  (parseArgs ["node", "c", "1", "2"]) = some (({ nodeType := .character, major := 1, minor := 2 } : Options), "node") := by
  native_decide

theorem parse_block :
  (parseArgs ["node", "b", "8", "0"]) = some (({ nodeType := .block, major := 8, minor := 0 } : Options), "node") := by
  native_decide

/-- parseType 'b' returns block. -/
theorem parseType_block : parseType "b" = .block := by native_decide

/-- parseType 'c' returns character. -/
theorem parseType_char : parseType "c" = .character := by native_decide

/-- parseType 'u' returns character. -/
theorem parseType_u : parseType "u" = .character := by native_decide

/-- parseType anything else returns character. -/
theorem parseType_other : parseType "x" = .character := by native_decide

/-- parseNum "0" returns 0. -/
theorem parseNum_zero : parseNum "0" = 0 := by native_decide

/-- parseNum "42" returns 42. -/
theorem parseNum_42 : parseNum "42" = 42 := by native_decide

/-- parseNum invalid returns 0. -/
theorem parseNum_invalid : parseNum "abc" = 0 := by native_decide

/-- parseNum empty returns 0. -/
theorem parseNum_empty : parseNum "" = 0 := by native_decide

/-- Missing args returns none. -/
theorem parse_missing_args :
  parseArgs ["node"] = none := by native_decide

/-- Missing major/minor returns none. -/
theorem parse_missing_major_minor :
  parseArgs ["node", "c"] = none := by native_decide

end Lentils.Mknod.Logic
