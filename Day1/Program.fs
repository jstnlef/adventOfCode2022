open System.IO

type FoodCalories = int list
type AllElves = FoodCalories list

let elfWithMostCaloricFood elves =
  elves
  |> List.map List.sum
  |> List.max


// type State = { all: int list list; current: int list}
let elvesWithFood =
  let s =
    File.ReadLines "Day1/input.txt"
    |> Seq.fold (fun state line ->
      let (all, current) = state
      if line = "" then
        (current :: all, [])
      else
        (all, (int line) :: current)
    ) ([], [])

  let (all, _) = s
  all

// let elvesWithFood =
//   [| [|1000
//        2000
//        3000|]
//      [| 4000 |]
//      [|7000
//        8000
//        9000|] |]

printfn "Elf with most caloric food: %d" (elfWithMostCaloricFood elvesWithFood)
