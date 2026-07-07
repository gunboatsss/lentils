import Lake
open System Lake DSL

package «lentils» where
  version := v!"0.1.0"

lean_lib Lentils

-- Build a static C archive (libcoreutils_support.a) from c/coreutils.c.
-- Using the system C compiler with Lean include path (not leanc, which
-- intentionally lacks system headers).
extern_lib coreutilsSupport (pkg) := do
  let src := pkg.dir / "c" / "coreutils.c"
  let o   := pkg.buildDir / "c" / "coreutils.o"
  let lib := pkg.buildDir / "c" / "libcoreutils_support.a"
  Job.async do
    let some sysroot ← Lake.findLeanSysroot?
      | error "could not find Lean sysroot"
    let lean ← LeanInstall.get sysroot
    compileO o src (lean.cFlags ++ #["-I", lean.includeDir.toString]) "cc"
    compileStaticLib lib #[o]
    return lib

@[default_target] lean_exe lentils where
  root := `Main
