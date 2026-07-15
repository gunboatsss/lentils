import Lentils.Config.Generated

/-
Config — Lentils configuration system.
0BSD

Reads a .config file (generated from lentils.Kconfig) and provides
a simple lookup for whether each applet is enabled.
-/

namespace Lentils.Config

/-- Parse a .config line: "CONFIG_FOO=y" or "# CONFIG_FOO is not set". -/
def parseLine (line : String) : Option (String × Bool) :=
  -- Convert to List Char for reliable operations
  let chars := line.toList
  let s := String.ofList chars
  if s.startsWith "CONFIG_" then
    let rest := (s.drop 7).toString
    match rest.splitOn "=" with
    | [name, "y"] => some (name, true)
    | [name, "m"] => some (name, true)
    | _ => none
  else if s.startsWith "# CONFIG_" then
    let rest := (s.drop 9).toString
    let name := (rest.splitOn " ").head?.getD ""
    some (name, false)
  else
    none

/-- Parse a full .config file content into a list of (name, enabled). -/
def parseConfig (content : String) : List (String × Bool) :=
  let lines := content.splitOn "\n"
  lines.filterMap parseLine

/-- Look up whether an applet is enabled. Default: true. -/
def isEnabled (name : String) (config : List (String × Bool)) : Bool :=
  match config.find? (λ (n, _) => n = name) with
  | some (_, v) => v
  | none => true

/-- Get list of enabled applet names. -/
def enabledApplets (config : List (String × Bool)) : List String :=
  config.filter (λ (_, v) => v) |>.map (λ (n, _) => n)

-- ─── Compile-time applet config ─────────────────────────────────────────────

/-- Check if an applet is enabled at compile time (by lowercase name). -/
def isGenEnabled (name : String) : Bool :=
  match name with
  | "arch" => Generated.ARCH_enabled
  | "b2sum" => Generated.B2SUM_enabled
  | "base32" => Generated.BASE32_enabled
  | "base64" => Generated.BASE64_enabled
  | "basename" => Generated.BASENAME_enabled
  | "basenc" => Generated.BASENC_enabled
  | "cal" => Generated.CAL_enabled
  | "cat" => Generated.CAT_enabled
  | "chgrp" => Generated.CHGRP_enabled
  | "chmod" => Generated.CHMOD_enabled
  | "chown" => Generated.CHOWN_enabled
  | "cksum" => Generated.CKSUM_enabled
  | "comm" => Generated.COMM_enabled
  | "cp" => Generated.CP_enabled
  | "csplit" => Generated.CSPLIT_enabled
  | "cut" => Generated.CUT_enabled
  | "date" => Generated.DATE_enabled
  | "dd" => Generated.DD_enabled
  | "df" => Generated.DF_enabled
  | "dir" => Generated.DIR_enabled
  | "dircolors" => Generated.DIRCOLORS_enabled
  | "dirname" => Generated.DIRNAME_enabled
  | "du" => Generated.DU_enabled
  | "echo" => Generated.ECHO_enabled
  | "env" => Generated.ENV_enabled
  | "expand" => Generated.EXPAND_enabled
  | "expr" => Generated.EXPR_enabled
  | "factor" => Generated.FACTOR_enabled
  | "false" => Generated.FALSE_enabled
  | "fmt" => Generated.FMT_enabled
  | "fold" => Generated.FOLD_enabled
  | "grep" => Generated.GREP_enabled
  | "groups" => Generated.GROUPS_enabled
  | "head" => Generated.HEAD_enabled
  | "hostid" => Generated.HOSTID_enabled
  | "id" => Generated.ID_enabled
  | "install" => Generated.INSTALL_enabled
  | "join" => Generated.JOIN_enabled
  | "kill" => Generated.KILL_enabled
  | "link" => Generated.LINK_enabled
  | "ln" => Generated.LN_enabled
  | "logname" => Generated.LOGNAME_enabled
  | "ls" => Generated.LS_enabled
  | "md5sum" => Generated.MD5SUM_enabled
  | "mesg" => Generated.MESG_enabled
  | "mkdir" => Generated.MKDIR_enabled
  | "mkfifo" => Generated.MKFIFO_enabled
  | "mknod" => Generated.MKNOD_enabled
  | "mktemp" => Generated.MKTEMP_enabled
  | "more" => Generated.MORE_enabled
  | "mv" => Generated.MV_enabled
  | "nice" => Generated.NICE_enabled
  | "nl" => Generated.NL_enabled
  | "nohup" => Generated.NOHUP_enabled
  | "nproc" => Generated.NPROC_enabled
  | "numfmt" => Generated.NUMFMT_enabled
  | "od" => Generated.OD_enabled
  | "paste" => Generated.PASTE_enabled
  | "pathchk" => Generated.PATHCHK_enabled
  | "pinky" => Generated.PINKY_enabled
  | "pr" => Generated.PR_enabled
  | "printenv" => Generated.PRINTENV_enabled
  | "printf" => Generated.PRINTF_enabled
  | "ptx" => Generated.PTX_enabled
  | "pwd" => Generated.PWD_enabled
  | "readlink" => Generated.READLINK_enabled
  | "realpath" => Generated.REALPATH_enabled
  | "rm" => Generated.RM_enabled
  | "rmdir" => Generated.RMDIR_enabled
  | "seq" => Generated.SEQ_enabled
  | "sha1sum" => Generated.SHA1SUM_enabled
  | "sha224sum" => Generated.SHA224SUM_enabled
  | "sha256sum" => Generated.SHA256SUM_enabled
  | "sha384sum" => Generated.SHA384SUM_enabled
  | "sha512sum" => Generated.SHA512SUM_enabled
  | "shred" => Generated.SHRED_enabled
  | "shuf" => Generated.SHUF_enabled
  | "sleep" => Generated.SLEEP_enabled
  | "sort" => Generated.SORT_enabled
  | "split" => Generated.SPLIT_enabled
  | "stat" => Generated.STAT_enabled
  | "sum" => Generated.SUM_enabled
  | "sync" => Generated.SYNC_enabled
  | "tac" => Generated.TAC_enabled
  | "tail" => Generated.TAIL_enabled
  | "tee" => Generated.TEE_enabled
  | "test" => Generated.TEST_enabled
  | "timeout" => Generated.TIMEOUT_enabled
  | "touch" => Generated.TOUCH_enabled
  | "tr" => Generated.TR_enabled
  | "true" => Generated.TRUE_enabled
  | "truncate" => Generated.TRUNCATE_enabled
  | "tsort" => Generated.TSORT_enabled
  | "tty" => Generated.TTY_enabled
  | "uname" => Generated.UNAME_enabled
  | "unexpand" => Generated.UNEXPAND_enabled
  | "uniq" => Generated.UNIQ_enabled
  | "unlink" => Generated.UNLINK_enabled
  | "uptime" => Generated.UPTIME_enabled
  | "users" => Generated.USERS_enabled
  | "vdir" => Generated.VDIR_enabled
  | "wc" => Generated.WC_enabled
  | "who" => Generated.WHO_enabled
  | "whoami" => Generated.WHOAMI_enabled
  | "yes" => Generated.YES_enabled
  | _ => true

-- ─── Proofs ──────────────────────────────────────────────────────────────────

theorem isEnabled_true : isEnabled "CAT" [("CAT", true), ("SHRED", false)] = true := by native_decide

theorem isEnabled_false : isEnabled "SHRED" [("CAT", true), ("SHRED", false)] = false := by native_decide

theorem isEnabled_default : isEnabled "MISSING" [("CAT", true)] = true := by native_decide

theorem enabledApplets_simple : enabledApplets [("CAT", true), ("SHRED", false), ("LS", true)] = ["CAT", "LS"] := by native_decide

end Lentils.Config