/-
Printf.Logic — Pure format string processing. 0BSD -/
namespace Lentils.Printf.Logic

/--
Format a string with positional arguments.
Processes the format string as a character list for structural recursion.
-/
def format (fmt : String) (args : List String) : String :=
  let chars := fmt.toList
  let rec go (cs : List Char) (as : List String) : String :=
    match cs with
    | [] => ""
    | '%' :: '%' :: rest => "%" ++ go rest as
    | '%' :: 's' :: rest =>
      match as with
      | [] => go rest []
      | arg :: restArgs => arg ++ go rest restArgs
    | '%' :: 'd' :: rest =>
      match as with
      | [] => go rest []
      | arg :: restArgs => arg ++ go rest restArgs
    | '%' :: 'i' :: rest =>
      match as with
      | [] => go rest []
      | arg :: restArgs => arg ++ go rest restArgs
    | '%' :: _ :: rest => go rest as
    | c :: rest => String.ofList [c] ++ go rest as
    termination_by cs.length
  go chars args

theorem format_empty : format "" [] = "" := by native_decide
example : format "hello" [] = "hello" := by native_decide
example : format "%s" ["world"] = "world" := by native_decide

end Lentils.Printf.Logic
