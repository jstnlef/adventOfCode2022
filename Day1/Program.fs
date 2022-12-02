open System.IO

let elfWithMostCaloricFood elves =
  elves
  |> Seq.map List.sum
  |> Seq.max

let top3withMostCaloricFood elves =
  elves
  |> Seq.map List.sum
  |> Seq.sortDescending
  |> Seq.take 3
  |> Seq.sum

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

printfn "Elf with most caloric food: %d" (elfWithMostCaloricFood elvesWithFood)
printfn "Top 3 elves with most caloric food: %d" (top3withMostCaloricFood elvesWithFood)
