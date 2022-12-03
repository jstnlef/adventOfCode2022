open System
open System.IO

let fileName = "Day3/input.txt"

let priorityForChar c =
  if (Char.IsUpper(c)) then
    int c - int 'A' + 27
  else
    int c - int 'a' + 1

let priorityForRucksack rucksack =
  let first, second =
    rucksack
    |> Array.splitAt (rucksack.Length / 2)

  Set.intersect (Set.ofArray first) (Set.ofArray second)
  |> Seq.head
  |> priorityForChar

let sumOfRucksackPriorities rucksacks =
  rucksacks
  |> Seq.map priorityForRucksack
  |> Seq.sum

let priorityFor3Elves rucksacks =
  rucksacks
  |> Seq.map Set.ofArray
  |> Seq.reduce (fun first next -> Set.intersect first next)
  |> Seq.head
  |> priorityForChar

let sumOf3ElfBadgePriorities rucksacks =
  rucksacks
  |> Seq.chunkBySize 3
  |> Seq.map priorityFor3Elves
  |> Seq.sum

let rucksacks =
  File.ReadLines fileName
  |> Seq.map (fun line -> Array.ofSeq line)

printfn "Sum of the shared rucksack priorities: %d" (sumOfRucksackPriorities rucksacks)
printfn "Sum of the 3 elf group badge priorities: %d" (sumOf3ElfBadgePriorities rucksacks)
