module Day3

open System
open System.IO

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

let parseRucksacks fileName =
  File.ReadLines fileName
  |> Seq.map (fun line -> Array.ofSeq line)
