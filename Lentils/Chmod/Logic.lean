/-
Chmod.Logic — Pure logic for the `chmod` utility. 0BSD

Contains only pure functions: argument parsing and mode computation. No IO is
performed here. Filesystem interaction lives in `chmod.lean` via the C FFI
call `chmod(2)`; the current mode needed for symbolic modes comes from the
`stat(2)` FFI wrapper `statMode`.

Supported mode specifications:
  * octal: 1–4 octal digits (e.g. `755`, `0644`, `4755`)
  * symbolic: comma-separated clauses `[ugoa]*[+-=][rwxXst]*`
-/

namespace Lentils.Chmod.Logic

/--
Options controlling `chmod` behaviour.

| flag                 | field      |
|----------------------|------------|
| `-R`/`--recursive`   | `recursive`|
| `-v`/`--verbose`     | `verbose`  |
| `-f`/`--force`       | `force`    |
| `-c`/`--changes`     | `changes`  |
-/
structure Options where
  recursive : Bool := false
  verbose : Bool := false
  force : Bool := false
  changes : Bool := false
  deriving Repr, BEq, DecidableEq

/--
Check whether a token looks like a flag (starts with `-`).
-/
def isFlag (s : String) : Bool :=
  s.startsWith "-"

/-- Apply a single recognised short flag character to `opts`. -/
def applyShort (c : Char) (opts : Options) : Option Options :=
  match c with
  | 'R' => some { opts with recursive := true }
  | 'v' => some { opts with verbose := true }
  | 'f' => some { opts with force := true }
  | 'c' => some { opts with changes := true }
  | _ => none

/--
Apply a combined short-flag token (e.g. `-Rv`) to `opts`, returning `none`
if any character is an unrecognised short flag.
-/
def applyShortFlags (s : String) (opts : Options) : Option Options :=
  let chars := s.toList.tail
  let rec go (cs : List Char) (o : Options) : Option Options :=
    match cs with
    | [] => some o
    | c :: r =>
      match applyShort c o with
      | none => none
      | some o2 => go r o2
  go chars opts

/--
Parse `chmod` arguments into `(options, mode, files)`.

The first non-flag operand is the mode specification; the remaining operands
are the files. A `--` separator terminates flag parsing. Combined short flags
(e.g. `-Rv`) are expanded. Unknown flags terminate flag parsing (silent
POSIX-ish behaviour).
-/
def parseArgs (args : List String) : Options × String × List String :=
  let rec go (remaining : List String) (opts : Options) (collected : List String)
      : Options × String × List String :=
    match remaining with
    | [] =>
        match collected.reverse with
        | [] => (opts, "", [])
        | mode :: files => (opts, mode, files)
    | "--" :: rest =>
        let (mode, files) :=
          match rest with
          | [] => ("", [])
          | m :: fs => (m, fs)
        (opts, mode, files)
    | "-R" :: rest => go rest { opts with recursive := true } collected
    | "--recursive" :: rest => go rest { opts with recursive := true } collected
    | "-v" :: rest => go rest { opts with verbose := true } collected
    | "--verbose" :: rest => go rest { opts with verbose := true } collected
    | "-f" :: rest => go rest { opts with force := true } collected
    | "--force" :: rest => go rest { opts with force := true } collected
    | "-c" :: rest => go rest { opts with changes := true } collected
    | "--changes" :: rest => go rest { opts with changes := true } collected
    | s :: rest =>
      if s == "-" then
        go rest opts (s :: collected)
      else if s.startsWith "-" then
        if s.length >= 3 then
          match applyShortFlags s opts with
          | none =>
            let (mode, files) :=
              match remaining with
              | [] => ("", [])
              | m :: fs => (m, fs)
            (opts, mode, files)
          | some newOpts => go rest newOpts collected
        else
          let (mode, files) :=
            match remaining with
            | [] => ("", [])
            | m :: fs => (m, fs)
            (opts, mode, files)
      else
        go rest opts (s :: collected)
  go args {} []

def optionsOf (p : Options × String × List String) : Options := p.1
def modeOf (p : Options × String × List String) : String := (p.2).1
def filesOf (p : Options × String × List String) : List String := (p.2).2

-- ─── Octal mode parsing ────────────────────────────────────────────────────────

/-- A digit is an octal digit iff it is in `0`..`7`. -/
def isOctalDigit (c : Char) : Bool :=
  '0' <= c && c <= '7'

/-- A string is an octal mode iff it is non-empty and all digits are octal. -/
def isOctal (s : String) : Bool :=
  !s.isEmpty && s.all isOctalDigit

/--
Parse an octal mode string into a `UInt32`.
Returns `none` if the string is not octal.
-/
def parseOctal (s : String) : Option UInt32 :=
  if isOctal s then
    let n : Nat := s.foldl (λ (a : Nat) (c : Char) =>
            a * 8 + (c.toNat - '0'.toNat)) 0
    some n.toUInt32
  else
    none

-- ─── Symbolic mode parsing ──────────────────────────────────────────────────────

/-- Permission bits in the low (owner) position. -/
def permLow (c : Char) : UInt32 :=
  match c with
  | 'r' => 0o4
  | 'w' => 0o2
  | 'x' => 0o1
  | 'X' => 0o1
  | _   => 0

/-- Bit shift for a `who` scope: `u`→6, `g`→3, `o`→0. -/
def shiftOf (w : Char) : UInt32 :=
  match w with
  | 'u' => 6
  | 'g' => 3
  | 'o' => 0
  | _   => 0

/-- A 3-bit mask covering one `who` scope. -/
def scopeMask (w : Char) : UInt32 :=
  0o7 <<< shiftOf w

/--
Apply a single symbolic clause `(who, op, perms)` to `cur`, returning the new
mode. `who` may be empty (meaning all scopes). `op` is one of `+`, `-`, `=`.
-/
def applyClause (cur : UInt32) (who : List Char) (op : Char) (perms : List Char) : UInt32 :=
  let scopes : List Char := if who.isEmpty then ['u', 'g', 'o'] else who
  let low := perms.foldl (λ a c => a ||| permLow c) 0
  let afterScopes := scopes.foldl (λ (m : UInt32) w =>
    let shifted := low <<< shiftOf w
    let mask := scopeMask w
    match op with
    | '=' => (m &&& ~~~mask) ||| shifted
    | '+' => m ||| shifted
    | '-' => m &&& ~~~shifted
    | _   => m) cur
  -- Special bits: setuid (0o4000, owned by the u scope), setgid
  -- (0o2000, owned by the g scope), sticky (0o1000, owned by the o scope).
  -- The operator MUST be honored so that `-` clears and `+`/`=` set these
  -- bits (GNU-compatible). The `s` permission only affects the setuid/setgid
  -- bits for the who scopes present; `t` only affects the sticky bit when the
  -- o scope is present.
  let hasU := scopes.contains 'u'
  let hasG := scopes.contains 'g'
  let hasO := scopes.contains 'o'
  let hasS := perms.contains 's'
  let hasT := perms.contains 't'
  -- Bits belonging to the affected scopes; `=` always resets these before
  -- optionally re-setting them according to `perms`.
  let scopeSpecial : UInt32 :=
    (if hasU then 0o4000 else 0) ||| (if hasG then 0o2000 else 0) ||| (if hasO then 0o1000 else 0)
  -- Bits to set (for `+`/`=`) or clear (for `-`) based on the perms present.
  let permSpecial : UInt32 :=
    (if hasS && hasU then 0o4000 else 0) |||
    (if hasS && hasG then 0o2000 else 0) |||
    (if hasT && hasO then 0o1000 else 0)
  let withSpecial : UInt32 :=
    match op with
    | '=' => (afterScopes &&& ~~~scopeSpecial) ||| permSpecial
    | '+' => afterScopes ||| permSpecial
    | '-' => afterScopes &&& ~~~permSpecial
    | _   => afterScopes
  withSpecial

/--
Parse a single symbolic clause into `(who, op, perms)`.
Returns `none` if the clause is malformed (missing op or perms).
-/
def parseClause (s : String) : Option (List Char × Char × List Char) :=
  let chars := s.toList
  let rec scanWho (cs : List Char) (who : List Char) : Option (List Char × List Char × Char × List Char) :=
    match cs with
    | [] => none
    | c :: rest =>
      if c == 'u' || c == 'g' || c == 'o' || c == 'a' then
        scanWho rest (who ++ [if c == 'a' then 'a' else c])
      else if c == '+' || c == '-' || c == '=' then
        let w := if who.contains 'a' then [] else who
        some (rest, w, c, [])
      else
        none
  match scanWho chars [] with
  | none => none
  | some (rest, who, op, _) =>
      let perms := rest.filter (λ c => c == 'r' || c == 'w' || c == 'x' || c == 'X' || c == 's' || c == 't')
      if perms.isEmpty then none else some (who, op, perms)

/--
Compute the new mode from a (possibly symbolic) mode string, given the
current mode. Octal strings yield an absolute mode; symbolic strings are
applied relative to `current`. Returns `none` if the mode string is invalid.
-/
def computeMode (modeStr : String) (current : UInt32) : Option UInt32 :=
  if isOctal modeStr then
    parseOctal modeStr
  else
    let clauses := modeStr.splitOn ","
    let rec go (cls : List String) (m : UInt32) : Option UInt32 :=
      match cls with
      | [] => some m
      | cl :: rest =>
        match parseClause cl with
        | none => none
        | some (who, op, perms) => go rest (applyClause m who op perms)
    go clauses current

-- ─── Theorems ──────────────────────────────────────────────────────────────────

/-- `755` parses to 0o755 = 493 decimal. -/
theorem octal_755 : parseOctal "755" = some 0o755 := by native_decide

/-- `0644` parses to 0o644. -/
theorem octal_0644 : parseOctal "0644" = some 0o644 := by native_decide

/-- A non-octal string is not octal. -/
theorem not_octal : isOctal "u+x" = false := by native_decide

/-- Adding execute to owner of mode 0o644 yields 0o744. -/
theorem sym_add_x : computeMode "u+x" 0o644 = some 0o744 := by native_decide

/-- `=` replaces the owner scope with rwx. -/
theorem sym_eq_rwx : computeMode "u=rwx" 0o000 = some 0o700 := by native_decide

/-- Octal takes precedence and is absolute. -/
theorem octal_takes_precedence : computeMode "755" 0o000 = some 0o755 := by native_decide

/-- Removing write from group of 0o664 yields 0o644. -/
theorem sym_del_gw : computeMode "g-w" 0o664 = some 0o644 := by native_decide

-- Removing setuid from owner of 0o4755 yields 0o0755 (operator `-` honored). -/
theorem sym_us_minus_s : computeMode "u-s" 0o4755 = some 0o0755 := by native_decide

/-- Removing setgid from group of 0o2755 yields 0o0755. -/
theorem sym_gs_minus_s : computeMode "g-s" 0o2755 = some 0o0755 := by native_decide

/-- `a-s` clears both setuid and setgid. -/
theorem sym_as_minus_s : computeMode "a-s" 0o6755 = some 0o0755 := by native_decide

/-- `o-t` clears the sticky bit. -/
theorem sym_ot_minus_t : computeMode "o-t" 0o1755 = some 0o0755 := by native_decide

/-- `a-t` clears the sticky bit regardless of scope list. -/
theorem sym_at_minus_t : computeMode "a-t" 0o1755 = some 0o0755 := by native_decide

/-- Adding setuid to owner of 0o0755 yields 0o4755. -/
theorem sym_us_plus_s : computeMode "u+s" 0o0755 = some 0o4755 := by native_decide

/-- `a+s` sets both setuid and setgid. -/
theorem sym_as_plus_s : computeMode "a+s" 0o0755 = some 0o6755 := by native_decide

/-- `o+s` is a no-op (s does not affect the o scope). -/
theorem sym_os_plus_s_noop : computeMode "o+s" 0o0755 = some 0o0755 := by native_decide

/-- `u+t` is a no-op (t only affects the o scope). -/
theorem sym_ut_plus_t_noop : computeMode "u+t" 0o0755 = some 0o0755 := by native_decide

/-- `=` resets then sets: `u=rws` on 0o4755 keeps setuid and owner rw. -/
theorem sym_us_eq_rws : computeMode "u=rws" 0o4755 = some 0o4655 := by native_decide

/-- `=` clears the setuid bit of the u scope when s is absent. -/
theorem sym_us_eq_rwx_clears_setuid : computeMode "u=rwx" 0o4755 = some 0o0755 := by native_decide

/-- `=` clears the setuid bit of the u scope when only r is given. -/
theorem sym_us_eq_r_clears_setuid : computeMode "u=r" 0o4755 = some 0o0455 := by native_decide

/-- `=` clears all special bits when the whole mode is reset. -/
theorem sym_a_eq_rwx_clears_all : computeMode "a=rwx" 0o6755 = some 0o0777 := by native_decide

end Lentils.Chmod.Logic
