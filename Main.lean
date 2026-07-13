/-
Main — Single-binary dispatcher for lentils.
0BSD

Entry point for the lentils multi-call binary.
Dispatches based on argv[0] to the appropriate utility.
Since Lean 4 main doesn't receive argv[0], use wrapper scripts
(see scripts/install-symlinks.sh) or invoke as `lentils <applet> ...`.
-/

import Lentils

/-- An applet registered in the multicall binary. -/
structure Applet where
  name : String
  run : (progName : String) → List String → IO UInt32
  descr : String

/-- Wrap a normal run to ignore the prog name. -/
def runNoProg (f : List String → IO UInt32) : String → List String → IO UInt32 :=
  λ _ args => f args

/-- All registered applets. -/
def applets : List Applet :=
  [
    { name := "arch",     run := runNoProg Lentils.Arch.run,     descr := "print machine architecture" },
    { name := "basename", run := runNoProg Lentils.Basename.run, descr := "strip directory and suffix from filenames" },
    { name := "cat",      run := runNoProg Lentils.Cat.run,      descr := "concatenate files to stdout" },
    { name := "chmod",    run := runNoProg Lentils.Chmod.run,    descr := "change file mode bits" },
    { name := "comm",     run := runNoProg Lentils.Comm.run,     descr := "compare two sorted files" },
    { name := "cp",       run := runNoProg Lentils.Cp.run,       descr := "copy files and directories" },
    { name := "cut",      run := runNoProg Lentils.Cut.run,      descr := "extract sections from each line of files" },
    { name := "dirname",  run := runNoProg Lentils.Dirname.run,  descr := "strip last component from file name" },
    { name := "echo",     run := runNoProg Lentils.Echo.run,     descr := "write arguments to stdout" },
    { name := "env",      run := runNoProg Lentils.Env.run,      descr := "run a command with modified environment" },
    { name := "expand",   run := runNoProg Lentils.Expand.run,   descr := "convert tabs to spaces" },
    { name := "false",    run := runNoProg Lentils.False.run,    descr := "exit with status 1" },
    { name := "fold",     run := runNoProg Lentils.Fold.run,     descr := "wrap lines at a specified width" },
    { name := "grep",     run := runNoProg Lentils.Grep.run,     descr := "print lines matching a pattern" },
    { name := "groups",   run := runNoProg Lentils.Groups.run,   descr := "print group memberships" },
    { name := "head",     run := runNoProg Lentils.Head.run,     descr := "output the first part of files" },
    { name := "hostid",   run := runNoProg Lentils.Hostid.run,   descr := "print numeric host identifier" },
    { name := "id",       run := runNoProg Lentils.Id.run,       descr := "print user and group identity" },
    { name := "join",     run := runNoProg Lentils.Join.run,     descr := "join lines on a common field" },
    { name := "ln",       run := runNoProg Lentils.Ln.run,       descr := "make links between files" },
    { name := "logname",  run := runNoProg Lentils.Logname.run, descr := "print user's login name" },
    { name := "ls",      run := runNoProg Lentils.Ls.run,      descr := "list directory contents" },
    { name := "mkdir",    run := runNoProg Lentils.Mkdir.run,    descr := "make directories" },
    { name := "mv",       run := runNoProg Lentils.Mv.run,       descr := "move (rename) files" },
    { name := "nproc",    run := runNoProg Lentils.Nproc.run,    descr := "print number of processing units" },
    { name := "nice",     run := runNoProg Lentils.Nice.run,     descr := "run a program with modified scheduling priority" },
    { name := "nohup",    run := runNoProg Lentils.Nohup.run,    descr := "run a command immune to hangups" },
    { name := "od",       run := runNoProg Lentils.Od.run,       descr := "dump files in octal format" },
    { name := "paste",    run := runNoProg Lentils.Paste.run,    descr := "merge lines of files" },
    { name := "printenv", run := runNoProg Lentils.Printenv.run, descr := "print environment variables" },
    { name := "printf",   run := runNoProg Lentils.Printf.run,   descr := "write formatted output" },
    { name := "pwd",      run := runNoProg Lentils.Pwd.run,      descr := "print working directory" },
    { name := "readlink", run := runNoProg Lentils.Readlink.run, descr := "print target of a symbolic link" },
    { name := "realpath", run := runNoProg Lentils.Realpath.run, descr := "print canonical absolute path" },
    { name := "rm",       run := runNoProg Lentils.Rm.run,       descr := "remove files or directories" },
    { name := "rmdir",    run := runNoProg Lentils.Rmdir.run,    descr := "remove empty directories" },
    { name := "seq",      run := runNoProg Lentils.Seq.run,      descr := "print sequence of numbers" },
    { name := "shuf",     run := runNoProg Lentils.Shuf.run,     descr := "shuffle lines of input" },
    { name := "sleep",    run := runNoProg Lentils.Sleep.run,    descr := "suspend execution for an interval" },
    { name := "sort",     run := runNoProg Lentils.Sort.run,     descr := "sort lines of text files" },
    { name := "tail",     run := runNoProg Lentils.Tail.run,     descr := "output the last part of files" },
    { name := "tee",      run := runNoProg Lentils.Tee.run,      descr := "read stdin and write to stdout and files" },
    { name := "test",     run := runNoProg Lentils.Test.run,     descr := "check file types and compare values" },
    { name := "[",        run := runNoProg Lentils.Test.run,     descr := "check file types and compare values" },
    { name := "tr",       run := runNoProg Lentils.Tr.run,       descr := "translate or delete characters" },
    { name := "true",     run := runNoProg Lentils.True.run,     descr := "exit with status 0" },
    { name := "touch",    run := runNoProg Lentils.Touch.run,    descr := "change file timestamps" },
    { name := "timeout",  run := runNoProg Lentils.Timeout.run,  descr := "run a command with a time limit" },
    { name := "tsort",    run := runNoProg Lentils.Tsort.run,    descr := "topological sort" },
    { name := "tty",      run := runNoProg Lentils.Tty.run,      descr := "print terminal file name" },
    { name := "uname",    run := runNoProg Lentils.Uname.run,    descr := "print system information" },
    { name := "unexpand", run := runNoProg Lentils.Unexpand.run, descr := "convert spaces to tabs" },
    { name := "uniq",     run := runNoProg Lentils.Uniq.run,     descr := "report or omit repeated lines" },
    { name := "uptime",   run := runNoProg Lentils.Uptime.run,   descr := "print system uptime" },
    { name := "users",    run := runNoProg Lentils.Users.run,    descr := "print logged-in user names" },
    { name := "wc",       run := runNoProg Lentils.Wc.run,       descr := "word, line, and byte count" },
    { name := "whoami",   run := runNoProg Lentils.Whoami.run,   descr := "print effective user name" },
    { name := "yes",      run := runNoProg Lentils.Yes.run,      descr := "repeat a string until killed" },
    { name := "fmt",     run := runNoProg Lentils.Fmt.run,     descr := "reformat paragraph text" },
    { name := "sum",     run := runNoProg Lentils.Sum.run,     descr := "BSD checksum and block counts" },
    { name := "cksum",   run := runNoProg Lentils.Cksum.run,   descr := "POSIX CRC checksum" },
    { name := "factor",  run := runNoProg Lentils.Factor.run,  descr := "factor integers into primes" },
    { name := "numfmt",  run := runNoProg Lentils.Numfmt.run,  descr := "convert numbers to/from human-readable form" },
    { name := "pr",      run := runNoProg Lentils.Pr.run,      descr := "paginate text into pages" },
    { name := "cal",     run := runNoProg Lentils.Cal.run,     descr := "display a calendar" },
    { name := "date",    run := runNoProg Lentils.Date.run,    descr := "print or set the system date and time" },
    { name := "stat",    run := Lentils.Stat.run,    descr := "display file or file system status" },
    { name := "df",      run := Lentils.Df.run,      descr := "report file system disk space usage" },
    { name := "du",      run := Lentils.Du.run,      descr := "estimate file space usage" },
    { name := "truncate",run := Lentils.Truncate.run, descr := "shrink or extend a file to a specified size" },
    { name := "install", run := Lentils.Install.run, descr := "copy files and set attributes" },
  ]

/-- Print per-utility help for the given applet.
Auto-generated from the Applet list. -/
def printHelp (prog : String) : IO Unit :=
  match applets.find? (λ a => a.name = prog) with
  | some a => do
      IO.println s!"Usage: lentils {a.name}"
      IO.println ""
      IO.println a.descr
  | none =>
      IO.eprintln s!"{prog}: no help available"

/--
Find an applet by name.
-/
def findApplet (name : String) : Option Applet :=
  applets.find? (λ a => a.name = name)

/--
Dispatch to the appropriate utility based on the program name.
Supports explicit invocation (`lentils cat file`).
Symlink invocation requires wrapper scripts.
-/
partial def dispatch (prog : String) (args : List String) : IO UInt32 :=
  match prog with
  | "lentils" =>
      match args with
      | [] => do
          IO.println "Usage: lentils <applet> [args...]"
          IO.println "Available applets:"
          for a in applets do
            IO.println s!"  {a.name}  — {a.descr}"
          return 0
      | "--help" :: _ => dispatch "lentils" []
      | applet :: "--help" :: _ => printHelp applet *> return 0
      | applet :: rest => dispatch applet rest
  | "help" =>
      match args with
      | [] => dispatch "lentils" []
      | applet :: _ => printHelp applet *> return 0
  | "--help" =>
      match args with
      | [] => dispatch "lentils" []
      | applet :: _ => printHelp applet *> return 0
  | _ =>
      match findApplet prog with
      | some a =>
        if args == ["--help"] then
          printHelp a.name *> return 0
        else
          a.run prog args
      | none => do
          IO.eprintln s!"{prog}: unknown applet"
          return 127

/--
Main entry point. Extracts the program name from argv[0]
(if present — Lean 4 main receives args without argv[0],
so use `lentils <applet>` or wrapper scripts).
-/
def main (args : List String) : IO UInt32 :=
  let prog := (System.FilePath.mk (args.headD "lentils")).fileName.getD "lentils"
  dispatch prog args.tail
