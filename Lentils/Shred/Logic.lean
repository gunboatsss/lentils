namespace Lentils.Shred.Logic

/-- Options controlling `shred` behaviour. -/
structure Options where
  passes : Nat := 3
  force : Bool := false
  verbose : Bool := false
  remove : Bool := true
  exact : Bool := false
  zero : Bool := true
  deriving Repr

/-- Parse shred arguments into (options, files). -/
def parseArgs (args : List String) : Options × List String :=
  let rec go (remaining : List String) (opts : Options) (files : List String)
      : Options × List String :=
    match remaining with
    | [] => (opts, files.reverse)
    | "--" :: rest => (opts, files.reverse ++ rest)
    | "-n" :: n :: rest =>
      let passes := match n.toNat? with | some p => p | none => 3
      go rest { opts with passes := passes } files
    | "--iterations" :: n :: rest =>
      let passes := match n.toNat? with | some p => p | none => 3
      go rest { opts with passes := passes } files
    | "-f" :: rest => go rest { opts with force := true } files
    | "--force" :: rest => go rest { opts with force := true } files
    | "-v" :: rest => go rest { opts with verbose := true } files
    | "--verbose" :: rest => go rest { opts with verbose := true } files
    | "-u" :: rest => go rest { opts with remove := true } files
    | "--remove" :: rest => go rest { opts with remove := true } files
    | "-z" :: rest => go rest { opts with zero := true } files
    | "--zero" :: rest => go rest { opts with zero := true } files
    | s :: rest =>
      if s.startsWith "-" && s != "-" then (opts, files.reverse)
      else go rest opts (s :: files)
  go args {} []

/-- Generate a pattern for pass number p (0-indexed). -/
def patternForPass (p : Nat) : UInt8 :=
  match p % 3 with
  | 0 => 0x00    -- all zeros
  | 1 => 0xFF    -- all ones
  | _ => 0x55    -- alternating 01010101

-- ─── Proofs ──────────────────────────────────────────────────────────────────

theorem pattern_pass0 : patternForPass 0 = 0x00 := by native_decide

theorem pattern_pass1 : patternForPass 1 = 0xFF := by native_decide

theorem pattern_pass2 : patternForPass 2 = 0x55 := by native_decide

theorem pattern_pass3 : patternForPass 3 = 0x00 := by native_decide

theorem parse_single_file :
  (parseArgs ["file.txt"]).2 = ["file.txt"] := by native_decide

theorem parse_passes :
  (parseArgs ["-n", "5", "f"]).1.passes = 5 := by native_decide

theorem parse_verbose :
  (parseArgs ["-v", "f"]).1.verbose = true := by native_decide

end Lentils.Shred.Logic
