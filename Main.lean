/-
Main — Single-binary dispatcher for lentils.
0BSD

Entry point for the lentils multi-call binary.
Dispatches based on argv[0] to the appropriate utility.
Since Lean 4 main doesn't receive argv[0], use wrapper scripts
(see scripts/install-symlinks.sh) or invoke as `lentils <applet> ...`.
-/

import Lentils

/--
Dispatch to the appropriate utility based on the program name.
Supports explicit invocation (`lentils cat file`).
Symlink invocation requires wrapper scripts.
-/
def dispatch (prog : String) (args : List String) : IO UInt32 :=
  match prog with
  | "true"     => Lentils.True.run args
  | "false"    => Lentils.False.run args
  | "cat"      => Lentils.Cat.run args
  | "echo"     => Lentils.Echo.run args
  | "pwd"      => Lentils.Pwd.run args
  | "head"     => Lentils.Head.run args
  | "tail"     => Lentils.Tail.run args
  | "wc"       => Lentils.Wc.run args
  | "uniq"     => Lentils.Uniq.run args
  | "basename" => Lentils.Basename.run args
  | "dirname"  => Lentils.Dirname.run args
  | "yes"      => Lentils.Yes.run args
  | "sleep"    => Lentils.Sleep.run args
  | "tee"      => Lentils.Tee.run args
  | "printf"   => Lentils.Printf.run args
  | "lentils" =>
      match args with
      | [] => do
          IO.println "Usage: lentils <applet> [args...]"
          IO.println "Available applets:"
          IO.println "  basename  — strip directory and suffix from filenames"
          IO.println "  cat       — concatenate files to stdout"
          IO.println "  dirname   — strip last component from file name"
          IO.println "  echo      — write arguments to stdout"
          IO.println "  false     — exit with status 1"
          IO.println "  head      — output the first part of files"
          IO.println "  printf    — write formatted output"
          IO.println "  pwd       — print working directory"
          IO.println "  sleep     — suspend execution for an interval"
          IO.println "  tail      — output the last part of files"
          IO.println "  tee       — read stdin and write to stdout and files"
          IO.println "  true      — exit with status 0"
          IO.println "  uniq      — report or omit repeated lines"
          IO.println "  wc        — word, line, and byte count"
          IO.println "  yes       — repeat a string until killed"
          return 0
      | applet :: rest => dispatch applet rest
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
