/-
Tsort.Logic — Pure topological sort logic for `tsort`. 0BSD -/
namespace Lentils.Tsort.Logic

/--
Check if a value is in a list.
-/
def contains (xs : List String) (x : String) : Bool :=
  xs.any (λ y => y = x)

/--
Simple topological sort using Kahn's algorithm.
Input lines are pairs separated by space: "a b" means a before b.
Each line may contain one node (no edges).
This is marked partial because Lean can't prove termination automatically
for the filter-based algorithm (the list strictly shrinks each iteration).
-/
partial def tsort (pairs : List (String × String)) : List String :=
  -- Collect all unique nodes (manual dedup)
  let allNodes :=
    let fst := pairs.map (λ (a, _) => a)
    let snd := pairs.map (λ (_, b) => b)
    let combined := fst ++ snd
    let rec uniq (xs : List String) (seen : List String) : List String :=
      match xs with
      | [] => seen.reverse
      | x :: rest =>
        if contains seen x then uniq rest seen
        else uniq rest (x :: seen)
    uniq combined []
  -- Kahn's algorithm
  let rec go (remaining : List (String × String)) (ordered : List String) : List String :=
    match remaining with
    | [] => ordered ++ (allNodes.filter (λ n => !contains ordered n))
    | _ =>
      let incoming := remaining.map (λ (_, b) => b)
      let ready := allNodes.filter (λ n =>
        !contains ordered n && !contains incoming n)
      match ready with
      | [] => ordered
      | n :: _ =>
        let remaining' := remaining.filter (λ (a, _) => a ≠ n)
        go remaining' (ordered ++ [n])
  go pairs []

end Lentils.Tsort.Logic
