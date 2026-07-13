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

/-- Print per-utility help for the given applet. -/
def printHelp (prog : String) : IO Unit :=
  match prog with
  | "arch" => do
      IO.println "Usage: lentils arch"
      IO.println ""
      IO.println "Print the machine architecture."
  | "hostid" => do
      IO.println "Usage: lentils hostid"
      IO.println ""
      IO.println "Print the numeric host identifier."
  | "join" => do
      IO.println "Usage: lentils join [file1 file2]"
      IO.println ""
      IO.println "Join lines of two files on a common field."
  | "logname" => do
      IO.println "Usage: lentils logname"
      IO.println ""
      IO.println "Print the user's login name."
  | "nl" => do
      IO.println "Usage: lentils nl"
      IO.println ""
      IO.println "Number lines of input."
  | "nproc" => do
      IO.println "Usage: lentils nproc"
      IO.println ""
      IO.println "Print the number of processing units."
  | "od" => do
      IO.println "Usage: lentils od"
      IO.println ""
      IO.println "Dump files in octal format."
  | "paste" => do
      IO.println "Usage: lentils paste [-d delim] [-s] [file...]"
      IO.println ""
      IO.println "Merge lines of files."
  | "uname" => do
      IO.println "Usage: lentils uname [OPTION]..."
      IO.println ""
      IO.println "Print system information."
      IO.println ""
      IO.println "Options:"
      IO.println "  -a    print all information"
      IO.println "  -s    print kernel name"
      IO.println "  -n    print network node hostname"
      IO.println "  -r    print kernel release"
      IO.println "  -v    print kernel version"
      IO.println "  -m    print machine hardware name"
  | "seq" => do
      IO.println "Usage: lentils seq [FIRST [INCREMENT]] LAST"
      IO.println ""
      IO.println "Print a sequence of numbers."
  | "shuf" => do
      IO.println "Usage: lentils shuf"
      IO.println ""
      IO.println "Shuffle lines of input."
  | "tty" => do
      IO.println "Usage: lentils tty"
      IO.println ""
      IO.println "Print the terminal file name."
  | "uptime" => do
      IO.println "Usage: lentils uptime"
      IO.println ""
      IO.println "Print the system uptime."
  | "whoami" => do
      IO.println "Usage: lentils whoami"
      IO.println ""
      IO.println "Print the effective user name."
  | "cat" => do
      IO.println "Usage: lentils cat [file...]"
      IO.println ""
      IO.println "Concatenate files to stdout."
  | "comm" => do
      IO.println "Usage: lentils comm [-123] file1 file2"
      IO.println ""
      IO.println "Compare two sorted files line by line."
  | "echo" => do
      IO.println "Usage: lentils echo [string...]"
      IO.println ""
      IO.println "Write arguments to stdout, separated by spaces."
  | "env" => do
      IO.println "Usage: lentils env [-i] [name=value...] [command [args...]]"
      IO.println ""
      IO.println "Run a command with modified environment, or list environment."
  | "expand" => do
      IO.println "Usage: lentils expand [-t tabsize]"
      IO.println ""
      IO.println "Convert tabs to spaces."
  | "fold" => do
      IO.println "Usage: lentils fold [-w width] [-s]"
      IO.println ""
      IO.println "Wrap lines at a specified width."
  | "true" => do
      IO.println "Usage: lentils true"
      IO.println ""
      IO.println "Exit with status 0."
  | "false" => do
      IO.println "Usage: lentils false"
      IO.println ""
      IO.println "Exit with status 1."
  | "pwd" => do
      IO.println "Usage: lentils pwd"
      IO.println ""
      IO.println "Print the current working directory."
  | "yes" => do
      IO.println "Usage: lentils yes [string]"
      IO.println ""
      IO.println "Repeat a string until killed. Default: 'y'."
  | "sleep" => do
      IO.println "Usage: lentils sleep seconds"
      IO.println ""
      IO.println "Suspend execution for the specified number of seconds."
  | "basename" => do
      IO.println "Usage: lentils basename path [suffix]"
      IO.println ""
      IO.println "Strip directory and optional suffix from a pathname."
  | "dirname" => do
      IO.println "Usage: lentils dirname path"
      IO.println ""
      IO.println "Strip the last component from a pathname."
  | "head" => do
      IO.println "Usage: lentils head [-n count] [file...]"
      IO.println ""
      IO.println "Output the first part of files. Default: 10 lines."
  | "tail" => do
      IO.println "Usage: lentils tail [-n count] [file...]"
      IO.println ""
      IO.println "Output the last part of files. Default: 10 lines."
  | "wc" => do
      IO.println "Usage: lentils wc [-l] [-w] [-c] [file...]"
      IO.println ""
      IO.println "Count lines, words, and bytes in files."
      IO.println ""
      IO.println "Options:"
      IO.println "  -l    count lines only"
      IO.println "  -w    count words only"
      IO.println "  -c    count bytes only"
  | "uniq" => do
      IO.println "Usage: lentils uniq [-u|-d] [file]"
      IO.println ""
      IO.println "Filter adjacent matching lines."
      IO.println ""
      IO.println "Options:"
      IO.println "  -u    output only unique lines"
      IO.println "  -d    output only repeated lines"
  | "tee" => do
      IO.println "Usage: lentils tee [-a] [file...]"
      IO.println ""
      IO.println "Copy stdin to stdout and to each file."
      IO.println ""
      IO.println "Options:"
      IO.println "  -a    append to files instead of overwriting"
  | "printf" => do
      IO.println "Usage: lentils printf format [arg...]"
      IO.println ""
      IO.println "Write formatted output. Supports %s, %d, %%, \\n, \\t, \\\\."
  | "readlink" => do
      IO.println "Usage: lentils readlink path"
      IO.println ""
      IO.println "Print the target of a symbolic link."
  | "realpath" => do
      IO.println "Usage: lentils realpath path"
      IO.println ""
      IO.println "Print the canonical absolute path."
  | "cut" => do
      IO.println "Usage: lentils cut -b list [-n] [file...]"
      IO.println "       lentils cut -c list [file...]"
      IO.println "       lentils cut -f list [-d delim] [-s] [file...]"
      IO.println ""
      IO.println "Extract sections from each line of input."
      IO.println ""
      IO.println "Options:"
      IO.println "  -b list   select bytes (not yet implemented)"
      IO.println "  -c list   select characters"
      IO.println "  -f list   select fields"
      IO.println "  -d delim  field delimiter (default: tab)"
      IO.println "  -s        suppress lines without delimiters"
      IO.println "  -n        do not split multibyte chars (with -b)"
  | "tr" => do
      IO.println "Usage: lentils tr [-c|-C] [-s] string1 string2"
      IO.println "       lentils tr -s [-c|-C] string1"
      IO.println "       lentils tr -d [-c|-C] string1"
      IO.println "       lentils tr -ds [-c|-C] string1 string2"
      IO.println ""
      IO.println "Translate, squeeze, or delete characters."
  | "tsort" => do
      IO.println "Usage: lentils tsort"
      IO.println ""
      IO.println "Topological sort."
  | "unexpand" => do
      IO.println "Usage: lentils unexpand"
      IO.println ""
      IO.println "Convert spaces to tabs."
  | "sort" => do
      IO.println "Usage: lentils sort [-r] [-n] [file...]"
      IO.println ""
      IO.println "Sort lines of text."
      IO.println ""
      IO.println "Options:"
      IO.println "  -r        reverse sort order"
      IO.println "  -n        compare according to string numerical value"
  | "test" => do
      IO.println "Usage: lentils test expression"
      IO.println "       lentils [ expression ]"
      IO.println ""
      IO.println "Evaluate conditional expressions."
      IO.println ""
      IO.println "String operators:"
      IO.println "  -n str    true if string is non-empty"
      IO.println "  -z str    true if string is empty"
      IO.println "  str1 = str2   true if strings equal"
      IO.println "  str1 != str2  true if strings not equal"
      IO.println ""
      IO.println "Integer operators:"
      IO.println "  n1 -eq n2  n1 -ne n2  n1 -lt n2  n1 -le n2"
      IO.println "  n1 -gt n2  n1 -ge n2"
      IO.println ""
      IO.println "Logical operators: !, -a, -o"
      IO.println ""
      IO.println "File operators:"
      IO.println "  -f file   true if file is a regular file"
      IO.println "  -d file   true if file is a directory"
      IO.println "  -e file   true if file exists"
      IO.println "  -s file   true if file exists and size > 0"
      IO.println "  -r file   true if file is readable"
      IO.println "  -w file   true if file is writable"
      IO.println "  -x file   true if file is executable"
  | "groups" => do
      IO.println "Usage: lentils groups [user]"
      IO.println ""
      IO.println "Print group memberships for each user."
  | "id" => do
      IO.println "Usage: lentils id [user]"
      IO.println ""
      IO.println "Print user and group identity."
  | "printenv" => do
      IO.println "Usage: lentils printenv [name...]"
      IO.println ""
      IO.println "Print environment variables."
  | "users" => do
      IO.println "Usage: lentils users"
      IO.println ""
      IO.println "Print logged-in user names."
  | "grep" => do
      IO.println "Usage: lentils grep [-v] [-e pattern] pattern [file...]"
      IO.println ""
      IO.println "Search for patterns in files or stdin."
      IO.println ""
      IO.println "Options:"
      IO.println "  -v        invert match (select non-matching lines)"
      IO.println "  -e pat    use pat as the pattern"
      IO.println ""
      IO.println "Regex syntax: . * ^ $ [abc] [^abc] \\"
      IO.println ""
      IO.println "Note: -i, -c, -n, -l, -q, -E, -F are not yet implemented."
  | _ =>
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
