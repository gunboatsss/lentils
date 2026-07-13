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
  run : List String → IO UInt32
  descr : String

/-- All registered applets. -/
def applets : List Applet :=
  [
    { name := "arch",     run := Lentils.Arch.run,     descr := "print machine architecture" },
    { name := "basename", run := Lentils.Basename.run, descr := "strip directory and suffix from filenames" },
    { name := "cat",      run := Lentils.Cat.run,      descr := "concatenate files to stdout" },
    { name := "comm",     run := Lentils.Comm.run,     descr := "compare two sorted files" },
    { name := "cut",      run := Lentils.Cut.run,      descr := "extract sections from each line of files" },
    { name := "dirname",  run := Lentils.Dirname.run,  descr := "strip last component from file name" },
    { name := "echo",     run := Lentils.Echo.run,     descr := "write arguments to stdout" },
    { name := "env",      run := Lentils.Env.run,      descr := "run a command with modified environment" },
    { name := "expand",   run := Lentils.Expand.run,   descr := "convert tabs to spaces" },
    { name := "false",    run := Lentils.False.run,    descr := "exit with status 1" },
    { name := "fold",     run := Lentils.Fold.run,     descr := "wrap lines at a specified width" },
    { name := "grep",     run := Lentils.Grep.run,     descr := "print lines matching a pattern" },
    { name := "groups",   run := Lentils.Groups.run,   descr := "print group memberships" },
    { name := "head",     run := Lentils.Head.run,     descr := "output the first part of files" },
    { name := "hostid",   run := Lentils.Hostid.run,   descr := "print numeric host identifier" },
    { name := "id",       run := Lentils.Id.run,       descr := "print user and group identity" },
    { name := "join",     run := Lentils.Join.run,     descr := "join lines on a common field" },
    { name := "logname",  run := Lentils.Logname.run,  descr := "print user's login name" },
    { name := "nl",       run := Lentils.Nl.run,       descr := "number lines of input" },
    { name := "nproc",    run := Lentils.Nproc.run,    descr := "print number of processing units" },
    { name := "od",       run := Lentils.Od.run,       descr := "dump files in octal format" },
    { name := "paste",    run := Lentils.Paste.run,    descr := "merge lines of files" },
    { name := "printenv", run := Lentils.Printenv.run, descr := "print environment variables" },
    { name := "printf",   run := Lentils.Printf.run,   descr := "write formatted output" },
    { name := "pwd",      run := Lentils.Pwd.run,      descr := "print working directory" },
    { name := "readlink", run := Lentils.Readlink.run, descr := "print target of a symbolic link" },
    { name := "realpath", run := Lentils.Realpath.run, descr := "print canonical absolute path" },
    { name := "seq",      run := Lentils.Seq.run,      descr := "print sequence of numbers" },
    { name := "shuf",     run := Lentils.Shuf.run,     descr := "shuffle lines of input" },
    { name := "sleep",    run := Lentils.Sleep.run,    descr := "suspend execution for an interval" },
    { name := "sort",     run := Lentils.Sort.run,     descr := "sort lines of text files" },
    { name := "tail",     run := Lentils.Tail.run,     descr := "output the last part of files" },
    { name := "tee",      run := Lentils.Tee.run,      descr := "read stdin and write to stdout and files" },
    { name := "test",     run := Lentils.Test.run,     descr := "check file types and compare values" },
    { name := "[",        run := Lentils.Test.run,     descr := "check file types and compare values" },
    { name := "tr",       run := Lentils.Tr.run,       descr := "translate or delete characters" },
    { name := "true",     run := Lentils.True.run,     descr := "exit with status 0" },
    { name := "tsort",    run := Lentils.Tsort.run,    descr := "topological sort" },
    { name := "tty",      run := Lentils.Tty.run,      descr := "print terminal file name" },
    { name := "uname",    run := Lentils.Uname.run,    descr := "print system information" },
    { name := "unexpand", run := Lentils.Unexpand.run, descr := "convert spaces to tabs" },
    { name := "uniq",     run := Lentils.Uniq.run,     descr := "report or omit repeated lines" },
    { name := "uptime",   run := Lentils.Uptime.run,   descr := "print system uptime" },
    { name := "users",    run := Lentils.Users.run,    descr := "print logged-in user names" },
    { name := "wc",       run := Lentils.Wc.run,       descr := "word, line, and byte count" },
    { name := "whoami",   run := Lentils.Whoami.run,   descr := "print effective user name" },
    { name := "yes",      run := Lentils.Yes.run,      descr := "repeat a string until killed" },
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
          a.run args
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
