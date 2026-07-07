/-
Dirname.Logic — Pure string processing for `dirname`. 0BSD -/
namespace Lentils.Dirname.Logic
def dirname (path : String) : String :=
  if path.isEmpty then "."
  else if path.all (· == '/') then "/"
  else
    let parts := path.splitOn "/"
    let nonEmpty : List String := parts.filter (λ s => s ≠ "")
    match nonEmpty.reverse with
    | [] => "."
    | [_] => if path.startsWith "/" then "/" else "."
    | _ :: rest =>
      let dirParts := rest.reverse
      if path.startsWith "/" then "/" ++ String.intercalate "/" dirParts
      else String.intercalate "/" dirParts
theorem dirname_plain_file : dirname "file.txt" = "." := by native_decide
theorem dirname_full_path : dirname "/usr/bin/file.txt" = "/usr/bin" := by native_decide
theorem dirname_root : dirname "/" = "/" := by native_decide
theorem dirname_trailing_slash : dirname "/usr/bin/" = "/usr" := by native_decide
end Lentils.Dirname.Logic
