/-
Main — Single-binary dispatcher for lentils.
0BSD

Entry point for the lentils multi-call binary.
Dispatches based on argv[0] to the appropriate utility.
Since Lean 4 main doesn't receive argv[0], use wrapper scripts
(see scripts/install-symlinks.sh) or invoke as `lentils <applet> ...`.
-/

import Lentils
import Lentils.Config

/-- An applet registered in the multicall binary. -/
structure Applet where
  name : String
  run : (progName : String) → List String → IO UInt32
  descr : String

/-- Wrap a normal run to ignore the prog name. -/
def runNoProg (f : List String → IO UInt32) : String → List String → IO UInt32 :=
  λ _ args => f args

/-- Conditionally include an applet based on config. -/
def appletIf (b : Bool) (a : Applet) : List Applet :=
  if b then [a] else []

/-- All registered applets (config-controlled). -/
def applets : List Applet :=
  List.flatten [
    appletIf (Lentils.Config.isGenEnabled "arch")

      { name := "arch",     run := runNoProg Lentils.Arch.run,     descr := "print machine architecture" },
    appletIf (Lentils.Config.isGenEnabled "basename")

      { name := "basename", run := runNoProg Lentils.Basename.run, descr := "strip directory and suffix from filenames" },
    appletIf (Lentils.Config.isGenEnabled "cat")

      { name := "cat",      run := runNoProg Lentils.Cat.run,      descr := "concatenate files to stdout" },
    appletIf (Lentils.Config.isGenEnabled "chgrp")

      { name := "chgrp",    run := runNoProg Lentils.Chgrp.run,    descr := "change group ownership" },
    appletIf (Lentils.Config.isGenEnabled "chmod")

      { name := "chmod",    run := runNoProg Lentils.Chmod.run,    descr := "change file mode bits" },
    appletIf (Lentils.Config.isGenEnabled "chown")

      { name := "chown",    run := runNoProg Lentils.Chown.run,    descr := "change file owner and group" },
    appletIf (Lentils.Config.isGenEnabled "comm")

      { name := "comm",     run := runNoProg Lentils.Comm.run,     descr := "compare two sorted files" },
    appletIf (Lentils.Config.isGenEnabled "csplit")

      { name := "csplit",   run := runNoProg Lentils.Csplit.run,   descr := "split file by context lines" },
    appletIf (Lentils.Config.isGenEnabled "cp")

      { name := "cp",       run := runNoProg Lentils.Cp.run,       descr := "copy files and directories" },
    appletIf (Lentils.Config.isGenEnabled "cut")

      { name := "cut",      run := runNoProg Lentils.Cut.run,      descr := "extract sections from each line of files" },
    appletIf (Lentils.Config.isGenEnabled "dirname")

      { name := "dirname",  run := runNoProg Lentils.Dirname.run,  descr := "strip last component from file name" },
    appletIf (Lentils.Config.isGenEnabled "dircolors")

      { name := "dircolors",run := runNoProg Lentils.Dircolors.run, descr := "color setup for ls" },
    appletIf (Lentils.Config.isGenEnabled "echo")

      { name := "echo",     run := runNoProg Lentils.Echo.run,     descr := "write arguments to stdout" },
    appletIf (Lentils.Config.isGenEnabled "env")

      { name := "env",      run := runNoProg Lentils.Env.run,      descr := "run a command with modified environment" },
    appletIf (Lentils.Config.isGenEnabled "expand")

      { name := "expand",   run := runNoProg Lentils.Expand.run,   descr := "convert tabs to spaces" },
    appletIf (Lentils.Config.isGenEnabled "false")

      { name := "false",    run := runNoProg Lentils.False.run,    descr := "exit with status 1" },
    appletIf (Lentils.Config.isGenEnabled "fold")

      { name := "fold",     run := runNoProg Lentils.Fold.run,     descr := "wrap lines at a specified width" },
    appletIf (Lentils.Config.isGenEnabled "grep")

      { name := "grep",     run := runNoProg Lentils.Grep.run,     descr := "print lines matching a pattern" },
    appletIf (Lentils.Config.isGenEnabled "groups")

      { name := "groups",   run := runNoProg Lentils.Groups.run,   descr := "print group memberships" },
    appletIf (Lentils.Config.isGenEnabled "head")

      { name := "head",     run := runNoProg Lentils.Head.run,     descr := "output the first part of files" },
    appletIf (Lentils.Config.isGenEnabled "hostid")

      { name := "hostid",   run := runNoProg Lentils.Hostid.run,   descr := "print numeric host identifier" },
    appletIf (Lentils.Config.isGenEnabled "id")

      { name := "id",       run := runNoProg Lentils.Id.run,       descr := "print user and group identity" },
    appletIf (Lentils.Config.isGenEnabled "join")

      { name := "join",     run := runNoProg Lentils.Join.run,     descr := "join lines on a common field" },
    appletIf (Lentils.Config.isGenEnabled "kill")

      { name := "kill",     run := runNoProg Lentils.Kill.run,     descr := "send a signal to a process" },
    appletIf (Lentils.Config.isGenEnabled "link")

      { name := "link",     run := runNoProg Lentils.Link.run,     descr := "call link() syscall" },
    appletIf (Lentils.Config.isGenEnabled "ln")

      { name := "ln",       run := runNoProg Lentils.Ln.run,       descr := "make links between files" },
    appletIf (Lentils.Config.isGenEnabled "logname")

      { name := "logname",  run := runNoProg Lentils.Logname.run, descr := "print user's login name" },
    appletIf (Lentils.Config.isGenEnabled "dir")

      { name := "dir",     run := runNoProg Lentils.Dir.run,     descr := "list directory contents (columnar)" },
    appletIf (Lentils.Config.isGenEnabled "ls")

      { name := "ls",       run := runNoProg Lentils.Ls.run,       descr := "list directory contents" },
    appletIf (Lentils.Config.isGenEnabled "vdir")

      { name := "vdir",    run := runNoProg Lentils.Vdir.run,    descr := "list directory contents (long)" },
    appletIf (Lentils.Config.isGenEnabled "mknod")

      { name := "mknod",    run := runNoProg Lentils.Mknod.run,    descr := "make block/character special files" },
    appletIf (Lentils.Config.isGenEnabled "mkfifo")

      { name := "mkfifo",   run := runNoProg Lentils.Mkfifo.run,   descr := "make named pipes (FIFOs)" },
    appletIf (Lentils.Config.isGenEnabled "mkdir")

      { name := "mkdir",    run := runNoProg Lentils.Mkdir.run,    descr := "make directories" },
    appletIf (Lentils.Config.isGenEnabled "mv")

      { name := "mv",       run := runNoProg Lentils.Mv.run,       descr := "move (rename) files" },
    appletIf (Lentils.Config.isGenEnabled "nproc")

      { name := "nproc",    run := runNoProg Lentils.Nproc.run,    descr := "print number of processing units" },
    appletIf (Lentils.Config.isGenEnabled "nice")

      { name := "nice",     run := runNoProg Lentils.Nice.run,     descr := "run a program with modified scheduling priority" },
    appletIf (Lentils.Config.isGenEnabled "nl")

      { name := "nl",       run := runNoProg Lentils.Nl.run,       descr := "number lines of input" },
    appletIf (Lentils.Config.isGenEnabled "nohup")

      { name := "nohup",    run := runNoProg Lentils.Nohup.run,    descr := "run a command immune to hangups" },
    appletIf (Lentils.Config.isGenEnabled "od")

      { name := "od",       run := runNoProg Lentils.Od.run,       descr := "dump files in octal format" },
    appletIf (Lentils.Config.isGenEnabled "paste")

      { name := "paste",    run := runNoProg Lentils.Paste.run,    descr := "merge lines of files" },
    appletIf (Lentils.Config.isGenEnabled "printenv")

      { name := "printenv", run := runNoProg Lentils.Printenv.run, descr := "print environment variables" },
    appletIf (Lentils.Config.isGenEnabled "printf")

      { name := "printf",   run := runNoProg Lentils.Printf.run,   descr := "write formatted output" },
    appletIf (Lentils.Config.isGenEnabled "pwd")

      { name := "pwd",      run := runNoProg Lentils.Pwd.run,      descr := "print working directory" },
    appletIf (Lentils.Config.isGenEnabled "readlink")

      { name := "readlink", run := runNoProg Lentils.Readlink.run, descr := "print target of a symbolic link" },
    appletIf (Lentils.Config.isGenEnabled "realpath")

      { name := "realpath", run := runNoProg Lentils.Realpath.run, descr := "print canonical absolute path" },
    appletIf (Lentils.Config.isGenEnabled "rm")

      { name := "rm",       run := runNoProg Lentils.Rm.run,       descr := "remove files or directories" },
    appletIf (Lentils.Config.isGenEnabled "rmdir")

      { name := "rmdir",    run := runNoProg Lentils.Rmdir.run,    descr := "remove empty directories" },
    appletIf (Lentils.Config.isGenEnabled "seq")

      { name := "seq",      run := runNoProg Lentils.Seq.run,      descr := "print sequence of numbers" },
    appletIf (Lentils.Config.isGenEnabled "shuf")

      { name := "shuf",     run := runNoProg Lentils.Shuf.run,     descr := "shuffle lines of input" },
    appletIf (Lentils.Config.isGenEnabled "sleep")

      { name := "sleep",    run := runNoProg Lentils.Sleep.run,    descr := "suspend execution for an interval" },
    appletIf (Lentils.Config.isGenEnabled "sort")

      { name := "sort",     run := runNoProg Lentils.Sort.run,     descr := "sort lines of text files" },
    appletIf (Lentils.Config.isGenEnabled "tac")

      { name := "tac",      run := runNoProg Lentils.Tac.run,      descr := "concatenate and write files in reverse" },
    appletIf (Lentils.Config.isGenEnabled "tail")

      { name := "tail",     run := runNoProg Lentils.Tail.run,     descr := "output the last part of files" },
    appletIf (Lentils.Config.isGenEnabled "tee")

      { name := "tee",      run := runNoProg Lentils.Tee.run,      descr := "read stdin and write to stdout and files" },
    appletIf (Lentils.Config.isGenEnabled "test")

      { name := "test",     run := runNoProg Lentils.Test.run,     descr := "check file types and compare values" },
    appletIf (Lentils.Config.isGenEnabled "test")
    { name := "[",        run := runNoProg Lentils.Test.run,     descr := "check file types and compare values" },
    appletIf (Lentils.Config.isGenEnabled "tr")

      { name := "tr",       run := runNoProg Lentils.Tr.run,       descr := "translate or delete characters" },
    appletIf (Lentils.Config.isGenEnabled "true")

      { name := "true",     run := runNoProg Lentils.True.run,     descr := "exit with status 0" },
    appletIf (Lentils.Config.isGenEnabled "touch")

      { name := "touch",    run := runNoProg Lentils.Touch.run,    descr := "change file timestamps" },
    appletIf (Lentils.Config.isGenEnabled "timeout")

      { name := "timeout",  run := runNoProg Lentils.Timeout.run,  descr := "run a command with a time limit" },
    appletIf (Lentils.Config.isGenEnabled "tsort")

      { name := "tsort",    run := runNoProg Lentils.Tsort.run,    descr := "topological sort" },
    appletIf (Lentils.Config.isGenEnabled "tty")

      { name := "tty",      run := runNoProg Lentils.Tty.run,      descr := "print terminal file name" },
    appletIf (Lentils.Config.isGenEnabled "uname")

      { name := "uname",    run := runNoProg Lentils.Uname.run,    descr := "print system information" },
    appletIf (Lentils.Config.isGenEnabled "unexpand")

      { name := "unexpand", run := runNoProg Lentils.Unexpand.run, descr := "convert spaces to tabs" },
    appletIf (Lentils.Config.isGenEnabled "unlink")

      { name := "unlink",  run := runNoProg Lentils.Unlink.run,  descr := "call unlink() syscall" },
    appletIf (Lentils.Config.isGenEnabled "uniq")

      { name := "uniq",     run := runNoProg Lentils.Uniq.run,     descr := "report or omit repeated lines" },
    appletIf (Lentils.Config.isGenEnabled "uptime")

      { name := "uptime",   run := runNoProg Lentils.Uptime.run,   descr := "print system uptime" },
    appletIf (Lentils.Config.isGenEnabled "users")

      { name := "users",    run := runNoProg Lentils.Users.run,    descr := "print logged-in user names" },
    appletIf (Lentils.Config.isGenEnabled "wc")

      { name := "wc",       run := runNoProg Lentils.Wc.run,       descr := "word, line, and byte count" },
    appletIf (Lentils.Config.isGenEnabled "who")

      { name := "who",      run := runNoProg Lentils.Who.run,      descr := "list logged-in users" },
    appletIf (Lentils.Config.isGenEnabled "whoami")

      { name := "whoami",   run := runNoProg Lentils.Whoami.run,   descr := "print effective user name" },
    appletIf (Lentils.Config.isGenEnabled "yes")

      { name := "yes",      run := runNoProg Lentils.Yes.run,      descr := "repeat a string until killed" },
    appletIf (Lentils.Config.isGenEnabled "fmt")

      { name := "fmt",     run := runNoProg Lentils.Fmt.run,     descr := "reformat paragraph text" },
    appletIf (Lentils.Config.isGenEnabled "sum")

      { name := "sum",     run := runNoProg Lentils.Sum.run,     descr := "BSD checksum and block counts" },
    appletIf (Lentils.Config.isGenEnabled "sync")

      { name := "sync",    run := runNoProg Lentils.Sync.run,    descr := "synchronize cached writes to disk" },
    appletIf (Lentils.Config.isGenEnabled "cksum")

      { name := "cksum",   run := runNoProg Lentils.Cksum.run,   descr := "POSIX CRC checksum" },
    appletIf (Lentils.Config.isGenEnabled "factor")

      { name := "factor",  run := runNoProg Lentils.Factor.run,  descr := "factor integers into primes" },
    appletIf (Lentils.Config.isGenEnabled "numfmt")

      { name := "numfmt",  run := runNoProg Lentils.Numfmt.run,  descr := "convert numbers to/from human-readable form" },
    appletIf (Lentils.Config.isGenEnabled "pr")

      { name := "pr",      run := runNoProg Lentils.Pr.run,      descr := "paginate text into pages" },
    appletIf (Lentils.Config.isGenEnabled "cal")

      { name := "cal",     run := runNoProg Lentils.Cal.run,     descr := "display a calendar" },
    appletIf (Lentils.Config.isGenEnabled "date")

      { name := "date",    run := runNoProg Lentils.Date.run,    descr := "print or set the system date and time" },
    appletIf (Lentils.Config.isGenEnabled "stat")

      { name := "stat",    run := Lentils.Stat.run,    descr := "display file or file system status" },
    appletIf (Lentils.Config.isGenEnabled "df")

      { name := "df",      run := Lentils.Df.run,      descr := "report file system disk space usage" },
    appletIf (Lentils.Config.isGenEnabled "du")

      { name := "du",      run := Lentils.Du.run,      descr := "estimate file space usage" },
    appletIf (Lentils.Config.isGenEnabled "truncate")

      { name := "truncate",run := Lentils.Truncate.run, descr := "shrink or extend a file to a specified size" },
    appletIf (Lentils.Config.isGenEnabled "install")

      { name := "install", run := Lentils.Install.run, descr := "copy files and set attributes" },
    appletIf (Lentils.Config.isGenEnabled "expr")

      { name := "expr",    run := runNoProg Lentils.Expr.run,    descr := "evaluate integer/string expressions" },
    appletIf (Lentils.Config.isGenEnabled "base32")

      { name := "base32",  run := runNoProg Lentils.Base32.run,  descr := "encode/decode base32 data" },
    appletIf (Lentils.Config.isGenEnabled "basenc")

      { name := "basenc",  run := runNoProg Lentils.Basenc.run,  descr := "encode/decode using any base" },
    appletIf (Lentils.Config.isGenEnabled "base64")

      { name := "base64",  run := runNoProg Lentils.Base64.run,  descr := "encode/decode base64 data" },
    appletIf (Lentils.Config.isGenEnabled "split")

      { name := "split",   run := runNoProg Lentils.Split.run,   descr := "split input into files by size/line count" },
    appletIf (Lentils.Config.isGenEnabled "dd")

      { name := "dd",      run := runNoProg Lentils.Dd.run,      descr := "convert and copy with block size options" },
    appletIf (Lentils.Config.isGenEnabled "pathchk")

      { name := "pathchk", run := runNoProg Lentils.Pathchk.run, descr := "check pathname validity" },
    appletIf (Lentils.Config.isGenEnabled "pinky")

      { name := "pinky",   run := runNoProg Lentils.Pinky.run,   descr := "lightweight finger" },
    appletIf (Lentils.Config.isGenEnabled "ptx")

      { name := "ptx",     run := runNoProg Lentils.Ptx.run,     descr := "produce permuted index" },
    appletIf (Lentils.Config.isGenEnabled "md5sum")

      { name := "md5sum",   run := runNoProg Lentils.Md5sum.run,   descr := "compute MD5 hash" },
    appletIf (Lentils.Config.isGenEnabled "sha1sum")

      { name := "sha1sum",  run := runNoProg Lentils.Sha1sum.run,  descr := "compute SHA-1 hash" },
    appletIf (Lentils.Config.isGenEnabled "shred")

      { name := "shred",    run := runNoProg Lentils.Shred.run,    descr := "securely delete files" },
    appletIf (Lentils.Config.isGenEnabled "sha224sum")

      { name := "sha224sum",run := runNoProg Lentils.Sha224sum.run, descr := "compute SHA-224 hash" },
    appletIf (Lentils.Config.isGenEnabled "sha256sum")

      { name := "sha256sum",run := runNoProg Lentils.Sha256sum.run, descr := "compute SHA-256 hash" },
    appletIf (Lentils.Config.isGenEnabled "sha384sum")

      { name := "sha384sum",run := runNoProg Lentils.Sha384sum.run, descr := "compute SHA-384 hash" },
    appletIf (Lentils.Config.isGenEnabled "sha512sum")

      { name := "sha512sum",run := runNoProg Lentils.Sha512sum.run, descr := "compute SHA-512 hash" },
    appletIf (Lentils.Config.isGenEnabled "b2sum")

      { name := "b2sum",    run := runNoProg Lentils.B2sum.run,    descr := "compute BLAKE2b hash" },
    appletIf (Lentils.Config.isGenEnabled "mktemp")

      { name := "mktemp",   run := runNoProg Lentils.Mktemp.run,   descr := "create temporary files/dirs" },
    appletIf (Lentils.Config.isGenEnabled "more")

      { name := "more",     run := runNoProg Lentils.More.run,     descr := "page through text files" },
    appletIf (Lentils.Config.isGenEnabled "mesg")

      { name := "mesg",     run := runNoProg Lentils.Mesg.run,     descr := "check/set terminal write access" },
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
