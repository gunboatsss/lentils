namespace Lentils.Dircolors.Logic

/-- A color database entry: key (type or extension) and color code. -/
structure Entry where
  key : String
  code : String
  deriving Repr

/-- The full in-memory database: terminal filters, color entries, etc. -/
structure Database where
  terms : List String          -- TERM/COLORTERM patterns
  types : List Entry           -- Basic file type colors (DIR, LINK, etc.)
  exts : List Entry            -- File extension colors (.tar, .jpg, etc.)
  deriving Repr

/--
Built-in default database.
-/
def defaultDB : Database :=
  { terms := [
    "COLORTERM ?*",
    "TERM Eterm", "TERM ansi", "TERM *color*", "TERM con[0-9]*x[0-9]*",
    "TERM cons25", "TERM console", "TERM cygwin", "TERM *direct*",
    "TERM dtterm", "TERM gnome", "TERM hurd", "TERM jfbterm",
    "TERM konsole", "TERM kterm", "TERM linux", "TERM linux-c",
    "TERM mlterm", "TERM putty", "TERM rxvt*", "TERM screen*",
    "TERM st", "TERM terminator", "TERM tmux*", "TERM vt100", "TERM xterm*"
  ],
    types := [
      ⟨"RESET", "0"⟩, ⟨"DIR", "01;34"⟩, ⟨"LINK", "01;36"⟩,
      ⟨"MULTIHARDLINK", "00"⟩, ⟨"FIFO", "40;33"⟩, ⟨"SOCK", "01;35"⟩,
      ⟨"DOOR", "01;35"⟩, ⟨"BLK", "40;33;01"⟩, ⟨"CHR", "40;33;01"⟩,
      ⟨"ORPHAN", "40;31;01"⟩, ⟨"MISSING", "00"⟩, ⟨"SETUID", "37;41"⟩,
      ⟨"SETGID", "30;43"⟩, ⟨"CAPABILITY", "00"⟩,
      ⟨"STICKY_OTHER_WRITABLE", "30;42"⟩, ⟨"OTHER_WRITABLE", "34;42"⟩,
      ⟨"STICKY", "37;44"⟩, ⟨"EXEC", "01;32"⟩
    ],
    exts := [
      ⟨".tar", "01;31"⟩, ⟨".tgz", "01;31"⟩, ⟨".arc", "01;31"⟩,
      ⟨".arj", "01;31"⟩, ⟨".taz", "01;31"⟩, ⟨".lha", "01;31"⟩,
      ⟨".lz4", "01;31"⟩, ⟨".lzh", "01;31"⟩, ⟨".lzma", "01;31"⟩,
      ⟨".tlz", "01;31"⟩, ⟨".txz", "01;31"⟩, ⟨".tzo", "01;31"⟩,
      ⟨".t7z", "01;31"⟩, ⟨".zip", "01;31"⟩, ⟨".z", "01;31"⟩,
      ⟨".dz", "01;31"⟩, ⟨".gz", "01;31"⟩, ⟨".lrz", "01;31"⟩,
      ⟨".lz", "01;31"⟩, ⟨".lzo", "01;31"⟩, ⟨".xz", "01;31"⟩,
      ⟨".zst", "01;31"⟩, ⟨".tzst", "01;31"⟩, ⟨".bz2", "01;31"⟩,
      ⟨".bz", "01;31"⟩, ⟨".tbz", "01;31"⟩, ⟨".tbz2", "01;31"⟩,
      ⟨".tz", "01;31"⟩, ⟨".deb", "01;31"⟩, ⟨".rpm", "01;31"⟩,
      ⟨".jar", "01;31"⟩, ⟨".war", "01;31"⟩, ⟨".ear", "01;31"⟩,
      ⟨".sar", "01;31"⟩, ⟨".rar", "01;31"⟩, ⟨".alz", "01;31"⟩,
      ⟨".ace", "01;31"⟩, ⟨".zoo", "01;31"⟩, ⟨".cpio", "01;31"⟩,
      ⟨".7z", "01;31"⟩, ⟨".rz", "01;31"⟩, ⟨".cab", "01;31"⟩,
      ⟨".wim", "01;31"⟩, ⟨".swm", "01;31"⟩, ⟨".dwm", "01;31"⟩,
      ⟨".esd", "01;31"⟩,
      ⟨".avif", "01;35"⟩, ⟨".jpg", "01;35"⟩, ⟨".jpeg", "01;35"⟩,
      ⟨".mjpg", "01;35"⟩, ⟨".mjpeg", "01;35"⟩, ⟨".gif", "01;35"⟩,
      ⟨".bmp", "01;35"⟩, ⟨".pbm", "01;35"⟩, ⟨".pgm", "01;35"⟩,
      ⟨".ppm", "01;35"⟩, ⟨".tga", "01;35"⟩, ⟨".xbm", "01;35"⟩,
      ⟨".xpm", "01;35"⟩, ⟨".tif", "01;35"⟩, ⟨".tiff", "01;35"⟩,
      ⟨".png", "01;35"⟩, ⟨".svg", "01;35"⟩, ⟨".svgz", "01;35"⟩,
      ⟨".mng", "01;35"⟩, ⟨".pcx", "01;35"⟩, ⟨".mov", "01;35"⟩,
      ⟨".mpg", "01;35"⟩, ⟨".mpeg", "01;35"⟩, ⟨".m2v", "01;35"⟩,
      ⟨".mkv", "01;35"⟩, ⟨".webm", "01;35"⟩, ⟨".webp", "01;35"⟩,
      ⟨".ogm", "01;35"⟩, ⟨".mp4", "01;35"⟩, ⟨".m4v", "01;35"⟩,
      ⟨".mp4v", "01;35"⟩, ⟨".vob", "01;35"⟩, ⟨".qt", "01;35"⟩,
      ⟨".nuv", "01;35"⟩, ⟨".wmv", "01;35"⟩, ⟨".asf", "01;35"⟩,
      ⟨".rm", "01;35"⟩, ⟨".rmvb", "01;35"⟩, ⟨".flc", "01;35"⟩,
      ⟨".avi", "01;35"⟩, ⟨".fli", "01;35"⟩, ⟨".flv", "01;35"⟩,
      ⟨".gl", "01;35"⟩, ⟨".dl", "01;35"⟩, ⟨".xcf", "01;35"⟩,
      ⟨".xwd", "01;35"⟩, ⟨".yuv", "01;35"⟩, ⟨".cgm", "01;35"⟩,
      ⟨".emf", "01;35"⟩, ⟨".ogv", "01;35"⟩, ⟨".ogx", "01;35"⟩,
      ⟨".aac", "00;36"⟩, ⟨".au", "00;36"⟩, ⟨".flac", "00;36"⟩,
      ⟨".m4a", "00;36"⟩, ⟨".mid", "00;36"⟩, ⟨".midi", "00;36"⟩,
      ⟨".mka", "00;36"⟩, ⟨".mp3", "00;36"⟩, ⟨".mpc", "00;36"⟩,
      ⟨".ogg", "00;36"⟩, ⟨".ra", "00;36"⟩, ⟨".wav", "00;36"⟩,
      ⟨".oga", "00;36"⟩, ⟨".opus", "00;36"⟩, ⟨".spx", "00;36"⟩,
      ⟨".xspf", "00;36"⟩,
      ⟨"*~", "00;90"⟩, ⟨"*#", "00;90"⟩, ⟨".bak", "00;90"⟩,
      ⟨".crdownload", "00;90"⟩, ⟨".dpkg-dist", "00;90"⟩,
      ⟨".dpkg-new", "00;90"⟩, ⟨".dpkg-old", "00;90"⟩,
      ⟨".dpkg-tmp", "00;90"⟩, ⟨".old", "00;90"⟩, ⟨".orig", "00;90"⟩,
      ⟨".part", "00;90"⟩, ⟨".rej", "00;90"⟩, ⟨".rpmnew", "00;90"⟩,
      ⟨".rpmorig", "00;90"⟩, ⟨".rpmsave", "00;90"⟩, ⟨".swp", "00;90"⟩,
      ⟨".tmp", "00;90"⟩, ⟨".ucf-dist", "00;90"⟩, ⟨".ucf-new", "00;90"⟩,
      ⟨".ucf-old", "00;90"⟩
    ]
  }

/-- Options for shell output format. -/
structure Options where
  csh : Bool := false
  sh : Bool := false
  printDatabase : Bool := false
  printLsColors : Bool := false
  deriving Repr

/-- Parse dircolors arguments. -/
def parseArgs (args : List String) : Options × String :=
  let rec go (remaining : List String) (opts : Options) (file : String) : Options × String :=
    match remaining with
    | [] => (opts, file)
    | "-b" :: rest | "--sh" :: rest | "--bourne-shell" :: rest =>
      go rest { opts with sh := true } file
    | "-c" :: rest | "--csh" :: rest | "--c-shell" :: rest =>
      go rest { opts with csh := true } file
    | "-p" :: rest | "--print-database" :: rest =>
      go rest { opts with printDatabase := true } file
    | "--print-ls-colors" :: rest =>
      go rest { opts with printLsColors := true } file
    | s :: rest =>
      if s.startsWith "-" && s != "-" then (opts, file)
      else go rest opts s
  go args {} ""

/-- Format header comment block for -p output. -/
def headerLines : List String := [
  "# Lentils dircolors database",
  "# This file controls the LS_COLORS environment variable.",
  "#",
  "# The keywords COLOR, OPTIONS, and EIGHTBIT are recognized but ignored.",
  "# ===================================================================",
  "# Terminal filters",
  "# ===================================================================",
  "# TERM and COLORTERM entries restrict following config to terminals",
  "# whose environment variable matches the given glob pattern."
]

/-- Attribute/color code explanation comments (used in -p). -/
def attrCommentLines : List String := [
  "# ===================================================================",
  "# Basic file types",
  "# ===================================================================",
  "# Below are the color strings for basic file type categories.",
  "# Color codes use semicolon-separated attributes:",
  "#   00=none  01=bold  04=underscore  05=blink  07=reverse  08=concealed",
  "# Text colors: 30-37 (black/red/green/yellow/blue/magenta/cyan/white)",
  "# Background:  40-47 (same color range)",
  "# 256-color and 24-bit RGB also supported via escape sequences.",
  "#NORMAL 00",
  "#FILE 00"
]

/-- Extension section header. -/
def extSectionLines : List String := [
  "# ===================================================================",
  "# File extension colors",
  "# ===================================================================",
  "# Suffix-based color rules.  Format: .extension COLOR",
  "# Matching is case-insensitive."
]

/-- Produce the full -p output as a list of lines. -/
def formatDatabase (db : Database) : List String :=
  let section1 := headerLines ++ db.terms ++ [""] ++ attrCommentLines
  let typeLines := db.types.map (λ e => e.key ++ " " ++ e.code ++ " # " ++
    match e.key with
    | "RESET" => "reset to \"normal\" color"
    | "DIR" => "directory"
    | "LINK" => "symbolic link"
    | "MULTIHARDLINK" => "regular file with more than one link"
    | "FIFO" => "pipe"
    | "SOCK" => "socket"
    | "DOOR" => "door"
    | "BLK" => "block device driver"
    | "CHR" => "character device driver"
    | "ORPHAN" => "symlink to nonexistent file, or non-stat'able file"
    | "MISSING" => "... and the files they point to"
    | "SETUID" => "file that is setuid (u+s)"
    | "SETGID" => "file that is setgid (g+s)"
    | "CAPABILITY" => "file with capability"
    | "STICKY_OTHER_WRITABLE" => "dir that is sticky and other-writable (+t,o+w)"
    | "OTHER_WRITABLE" => "dir that is other-writable (o+w) and not sticky"
    | "STICKY" => "dir with the sticky bit set (+t) and not other-writable"
    | "EXEC" => "files with execute permission"
    | _ => "")
  let typeBlock := typeLines ++ [""] ++ extSectionLines
  let extLines := db.exts.map (λ e => e.key ++ " " ++ e.code)
  section1 ++ typeBlock ++ extLines ++ [""]
/-- Map from long type names to GNU short lowercase aliases. -/
def shortAliases : List (String × String) :=
  [("RESET", "rs"), ("DIR", "di"), ("LINK", "ln"),
   ("MULTIHARDLINK", "mh"), ("FIFO", "pi"), ("SOCK", "so"),
   ("DOOR", "do"), ("BLK", "bd"), ("CHR", "cd"),
   ("ORPHAN", "or"), ("MISSING", "mi"), ("SETUID", "su"),
   ("SETGID", "sg"), ("CAPABILITY", "ca"),
   ("STICKY_OTHER_WRITABLE", "tw"), ("OTHER_WRITABLE", "ow"),
   ("STICKY", "st"), ("EXEC", "ex")]

/-- Look up the short alias for a type key, or return the key itself. -/
def shortKey (key : String) : String :=
  match shortAliases.find? (λ (long, _) => long = key) with
  | some (_, short) => short
  | none => key

/-- Format a single extension entry for LS_COLORS, adding * prefix if needed. -/
def formatExtEntry (e : Entry) : String :=
  (if e.key.startsWith "*" then "" else "*") ++ e.key ++ "=" ++ e.code

/-- Convert database entries to LS_COLORS string (using short aliases). -/
def entriesToLS_COLORS (db : Database) : String :=
  let typeEntries := db.types.map (λ e => shortKey e.key ++ "=" ++ e.code)
  let extEntries := db.exts.map formatExtEntry
  String.intercalate ":" (typeEntries ++ extEntries) ++ ":"

/-- Format shell output for sh or csh. -/
def formatShellOutput (opts : Options) (value : String) : String :=
  if opts.csh then
    "setenv LS_COLORS '" ++ value ++ "'\n"
  else
    "LS_COLORS='" ++ value ++ "';\nexport LS_COLORS\n"

/-- Parse a database text (file content) into a Database. -/
def parseDatabase (content : String) : Database :=
  let lines := content.splitOn "\n"
  let rec go (remaining : List String) (terms : List String) (types : List Entry)
             (exts : List Entry) : Database :=
    match remaining with
    | [] => { terms := terms.reverse, types := types.reverse, exts := exts.reverse }
    | line :: rest =>
      let trimmed := line.trimAscii.toString
      if trimmed.isEmpty || trimmed.startsWith "#" then
        go rest terms types exts
      else if trimmed.startsWith "TERM " then
        go rest (trimmed :: terms) types exts
      else if trimmed.startsWith "COLORTERM " then
        go rest (trimmed :: terms) types exts
      else if trimmed.startsWith "COLOR " || trimmed.startsWith "EIGHTBIT " ||
              trimmed.startsWith "OPTIONS" then
        go rest terms types exts
      else
        match trimmed.splitOn " " with
        | key :: restWords =>
          let codePart := String.intercalate " " restWords
          let code' := (codePart.splitOn "#").head?.getD "" |>.trimAscii.toString
          if code'.isEmpty then
            go rest terms types exts
          else if key.startsWith "." || key.startsWith "*" then
            go rest terms types (⟨key, code'⟩ :: exts)
          else
            go rest terms (⟨key, code'⟩ :: types) exts
        | _ => go rest terms types exts
    termination_by remaining.length
  go lines [] [] []

theorem parse_default_sh : (parseArgs []).1.sh = false := by native_decide
theorem parse_csh : (parseArgs ["-c"]).1.csh = true := by native_decide
theorem parse_print : (parseArgs ["-p"]).1.printDatabase = true := by native_decide

/-- shortKey: RESET maps to rs. -/
theorem shortKey_RESET : shortKey "RESET" = "rs" := by native_decide

/-- shortKey: DIR maps to di. -/
theorem shortKey_DIR : shortKey "DIR" = "di" := by native_decide

/-- shortKey: EXEC maps to ex. -/
theorem shortKey_EXEC : shortKey "EXEC" = "ex" := by native_decide

/-- shortKey: unknown key returns itself. -/
theorem shortKey_unknown : shortKey "UNKNOWN" = "UNKNOWN" := by native_decide

/-- formatExtEntry: extension with dot gets * prefix. -/
theorem formatExtEntry_dot :
  formatExtEntry ⟨".tar", "01;31"⟩ = "*.tar=01;31" := by native_decide

/-- formatExtEntry: key with * already keeps it. -/
theorem formatExtEntry_star :
  formatExtEntry ⟨"*~", "00;90"⟩ = "*~=00;90" := by native_decide

/-- formatShellOutput: sh format. -/
theorem formatShellOutput_sh :
  formatShellOutput {} "rs=0:di=1" = "LS_COLORS='rs=0:di=1';\nexport LS_COLORS\n" := by native_decide

/-- formatShellOutput: csh format. -/
theorem formatShellOutput_csh :
  formatShellOutput { csh := true } "rs=0" = "setenv LS_COLORS 'rs=0'\n" := by native_decide

/-- entriesToLS_COLORS: empty database produces trailing colon. -/
theorem entriesToLS_COLORS_empty :
  entriesToLS_COLORS { terms := [], types := [], exts := [] } = ":" := by native_decide

/-- entriesToLS_COLORS: single type entry. -/
theorem entriesToLS_COLORS_single_type :
  entriesToLS_COLORS { terms := [], types := [⟨"DIR", "01;34"⟩], exts := [] } = "di=01;34:" := by native_decide

/-- entriesToLS_COLORS: single extension entry. -/
theorem entriesToLS_COLORS_single_ext :
  entriesToLS_COLORS { terms := [], types := [], exts := [⟨".tar", "01;31"⟩] } = "*.tar=01;31:" := by native_decide

end Lentils.Dircolors.Logic