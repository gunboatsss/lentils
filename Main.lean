/-
Main — Single-binary dispatcher for lentils.
0BSD

Entry point for the lentils multi-call binary.
Dispatches based on argv[0] to the appropriate utility.
Since Lean 4 main doesn't receive argv[0], use wrapper scripts
(see scripts/install-symlinks.sh) or invoke as `lentils <applet> ...`.
-/

import Lentils

/-- Print per-utility help for the given applet. -/
def printHelp (prog : String) : IO Unit :=
  match prog with
  | "arch" => do
      IO.println "Usage: lentils arch"
      IO.println ""
      IO.println "Print the machine architecture."
  | "logname" => do
      IO.println "Usage: lentils logname"
      IO.println ""
      IO.println "Print the user's login name."
  | "nproc" => do
      IO.println "Usage: lentils nproc"
      IO.println ""
      IO.println "Print the number of processing units."
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
  | "whoami" => do
      IO.println "Usage: lentils whoami"
      IO.println ""
      IO.println "Print the effective user name."
  | "cat" => do
      IO.println "Usage: lentils cat [file...]"
      IO.println ""
      IO.println "Concatenate files to stdout."
  | "echo" => do
      IO.println "Usage: lentils echo [string...]"
      IO.println ""
      IO.println "Write arguments to stdout, separated by spaces."
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
      IO.println ""
      IO.println "Options:"
      IO.println "  -d        delete characters in string1"
      IO.println "  -s        squeeze repeated characters"
      IO.println "  -c, -C    complement string1"
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
Dispatch to the appropriate utility based on the program name.
Supports explicit invocation (`lentils cat file`).
Symlink invocation requires wrapper scripts.
-/
partial def dispatch (prog : String) (args : List String) : IO UInt32 :=
  -- Check for --help in any utility's args
  let hasHelp := args == ["--help"]
  match prog with
  | "true"     => if hasHelp then printHelp "true" *> return 0 else Lentils.True.run args
  | "false"    => if hasHelp then printHelp "false" *> return 0 else Lentils.False.run args
  | "cat"      => if hasHelp then printHelp "cat" *> return 0 else Lentils.Cat.run args
  | "echo"     => if hasHelp then printHelp "echo" *> return 0 else Lentils.Echo.run args
  | "pwd"      => if hasHelp then printHelp "pwd" *> return 0 else Lentils.Pwd.run args
  | "head"     => if hasHelp then printHelp "head" *> return 0 else Lentils.Head.run args
  | "tail"     => if hasHelp then printHelp "tail" *> return 0 else Lentils.Tail.run args
  | "wc"       => if hasHelp then printHelp "wc" *> return 0 else Lentils.Wc.run args
  | "uniq"     => if hasHelp then printHelp "uniq" *> return 0 else Lentils.Uniq.run args
  | "cut"      => if hasHelp then printHelp "cut" *> return 0 else Lentils.Cut.run args
  | "tr"       => if hasHelp then printHelp "tr" *> return 0 else Lentils.Tr.run args
  | "sort"     => if hasHelp then printHelp "sort" *> return 0 else Lentils.Sort.run args
  | "test"     => if hasHelp then printHelp "test" *> return 0 else Lentils.Test.run args
  | "["        => if hasHelp then printHelp "test" *> return 0 else Lentils.Test.run args
  | "grep"     => if hasHelp then printHelp "grep" *> return 0 else Lentils.Grep.run args
  | "arch"     => if hasHelp then printHelp "arch" *> return 0 else Lentils.Arch.run args
  | "logname"  => if hasHelp then printHelp "logname" *> return 0 else Lentils.Logname.run args
  | "nproc"    => if hasHelp then printHelp "nproc" *> return 0 else Lentils.Nproc.run args
  | "uname"    => if hasHelp then printHelp "uname" *> return 0 else Lentils.Uname.run args
  | "whoami"   => if hasHelp then printHelp "whoami" *> return 0 else Lentils.Whoami.run args
  | "basename" => if hasHelp then printHelp "basename" *> return 0 else Lentils.Basename.run args
  | "dirname"  => if hasHelp then printHelp "dirname" *> return 0 else Lentils.Dirname.run args
  | "yes"      => if hasHelp then printHelp "yes" *> return 0 else Lentils.Yes.run args
  | "sleep"    => if hasHelp then printHelp "sleep" *> return 0 else Lentils.Sleep.run args
  | "tee"      => if hasHelp then printHelp "tee" *> return 0 else Lentils.Tee.run args
  | "printf"   => if hasHelp then printHelp "printf" *> return 0 else Lentils.Printf.run args
  | "lentils" =>
      match args with
      | [] => do
          IO.println "Usage: lentils <applet> [args...]"
          IO.println "Available applets:"
          IO.println "  arch      — print machine architecture"
          IO.println "  basename  — strip directory and suffix from filenames"
          IO.println "  logname   — print user's login name"
          IO.println "  nproc     — print number of processing units"
          IO.println "  uname     — print system information"
          IO.println "  whoami    — print effective user name"
          IO.println "  cat       — concatenate files to stdout"
          IO.println "  cut       — extract sections from each line of files"
          IO.println "  dirname   — strip last component from file name"
          IO.println "  echo      — write arguments to stdout"
          IO.println "  false     — exit with status 1"
          IO.println "  grep      — print lines matching a pattern"
          IO.println "  head      — output the first part of files"
          IO.println "  printf    — write formatted output"
          IO.println "  pwd       — print working directory"
          IO.println "  sleep     — suspend execution for an interval"
          IO.println "  sort      — sort lines of text files"
          IO.println "  tail      — output the last part of files"
          IO.println "  tee       — read stdin and write to stdout and files"
          IO.println "  test      — check file types and compare values"
          IO.println "  tr        — translate or delete characters"
          IO.println "  true      — exit with status 0"
          IO.println "  uniq      — report or omit repeated lines"
          IO.println "  wc        — word, line, and byte count"
          IO.println "  yes       — repeat a string until killed"
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
  | _ => do
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
