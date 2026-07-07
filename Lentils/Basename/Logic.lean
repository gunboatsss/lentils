/-
Basename.Logic — Pure string processing for `basename`. 0BSD -/
namespace Lentils.Basename.Logic

def basename (path : String) (suffix : String := "") : String :=
  let parts := path.splitOn "/"
  let rec findLast (xs : List String) (default : String) : String :=
    match xs with | [] => default | y :: ys => findLast ys (if y.isEmpty then default else y)
  let last := findLast parts (if path.isEmpty then "" else "/")
  if suffix.isEmpty then last
  else if last.endsWith suffix then
    (last.take (last.length - suffix.length)).toString
  else last

theorem basename_plain_file : basename "file.txt" = "file.txt" := by native_decide
theorem basename_full_path : basename "/usr/bin/file.txt" = "file.txt" := by native_decide
theorem basename_suffix : basename "/usr/bin/file.txt" ".txt" = "file" := by native_decide
theorem basename_root : basename "/" = "/" := by native_decide
theorem basename_trailing_slash : basename "/usr/bin/" = "bin" := by native_decide

end Lentils.Basename.Logic
