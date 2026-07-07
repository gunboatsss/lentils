/-
Printf.Logic — Pure format string processing. 0BSD -/
namespace Lentils.Printf.Logic

partial def format (fmt : String) (args : List String) : String :=
  if fmt.isEmpty then ""
  else if fmt.startsWith "%%" then "%" ++ format (fmt.drop 2).toString args
  else if fmt.startsWith "%s" then
    match args with | [] => format (fmt.drop 2).toString [] | arg :: rest => arg ++ format (fmt.drop 2).toString rest
  else if fmt.startsWith "%d" || fmt.startsWith "%i" then
    match args with | [] => format (fmt.drop 2).toString [] | arg :: rest => arg ++ format (fmt.drop 2).toString rest
  else if fmt.startsWith "%" then format (fmt.drop 2).toString args
  else (fmt.take 1).toString ++ format (fmt.drop 1).toString args

theorem format_empty : format "" [] = "" := by native_decide
example : format "hello" [] = "hello" := by native_decide
example : format "%s" ["world"] = "world" := by native_decide

end Lentils.Printf.Logic
