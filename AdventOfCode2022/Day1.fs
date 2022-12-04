module Day1

open System.IO

let elfWithMostCaloricFood (elves: int list list) =
  elves
  |> Seq.map List.sum
  |> Seq.max

let top3withMostCaloricFood (elves: int list list) =
  elves
  |> Seq.map List.sum
  |> Seq.sortDescending
  |> Seq.take 3
  |> Seq.sum

let parseElvesWithFood fileName =
  let s =
    File.ReadLines fileName
    |> Seq.fold (fun state line ->
      let (all, current) = state
      if line = "" then
        (current :: all, [])
      else
        (all, (int line) :: current)
    ) ([], [])

  let (all, _) = s
  all
