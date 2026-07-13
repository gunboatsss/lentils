/-
Shuf.Logic — Pure shuffling logic for `shuf`. 0BSD -/
namespace Lentils.Shuf.Logic

/--
Pick a random element from a list and return it along with the rest.
Uses `rand` which should return a Nat in [0, n-1].
-/
partial def pickRandom (items : List String) (rand : Nat → IO Nat) : IO (Prod String (List String)) := do
  match items with
  | [] => pure ("", [])
  | [x] => pure (x, [])
  | _ =>
    let n := items.length
    let idx ← rand (n - 1)
    let selected := match items.drop idx with | x :: _ => x | [] => ""
    let rest := items.take idx ++ items.drop (idx + 1)
    pure (selected, rest)

/--
Helper: recursive shuffle with accumulator.
-/
partial def goShuffle (remaining : List String) (acc : List String) (rand : Nat → IO Nat) : IO (List String) :=
  match remaining with
  | [] => pure acc.reverse
  | _ => do
    let pair ← pickRandom remaining rand
    let picked := pair.1
    let rest := pair.2
    goShuffle rest (picked :: acc) rand

/--
Shuffle all items using Fisher-Yates style random selection.
-/
def shuffle (items : List String) (rand : Nat → IO Nat) : IO (List String) :=
  goShuffle items [] rand

end Lentils.Shuf.Logic
