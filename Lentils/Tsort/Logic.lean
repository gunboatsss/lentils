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
Terminates by counting down from the number of unique nodes.
-/
def tsort (pairs : List (String × String)) : List String :=
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
  -- Kahn's algorithm with countdown for termination
  let rec go (remaining : List (String × String)) (count : Nat) (ordered : List String) : List String :=
    if count = 0 then ordered
    else
      match remaining with
      | [] => ordered ++ (allNodes.filter (λ n => !contains ordered n))
      | _ =>
        let incoming := remaining.map (λ (_, b) => b)
        let ready := allNodes.filter (λ n =>
          !contains ordered n && !contains incoming n)
        match ready with
        | [] => ordered  -- cycle detected
        | n :: _ =>
          let remaining' := remaining.filter (λ (a, _) => a ≠ n)
          go remaining' (count - 1) (ordered ++ [n])
    termination_by count
  go pairs allNodes.length []

end Lentils.Tsort.Logic
