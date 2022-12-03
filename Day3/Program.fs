open System
open System.IO

type Rucksack = char Set * char Set

let split size source =
  seq {
    let r = ResizeArray()
    for x in source do
      r.Add(x)
      if r.Count = size then
        yield r.ToArray()
        r.Clear()
    if r.Count > 0 then
      yield r.ToArray()
  }

let fileName = "Day3/input.txt"

let priorityForChar c =
  if (Char.IsUpper(c)) then
    int c - int 'A' + 27
  else
    int c - int 'a' + 1

let priorityForRucksack (rucksack: Rucksack) =
  let (first, second) = rucksack
  let shared = ((Set.intersect first second) |> Set.toArray)[0]
  priorityForChar shared

let sumOfRucksackPriorities (rucksacks: Rucksack seq) =
  rucksacks
  |> Seq.map priorityForRucksack
  |> Seq.sum

let priorityFor3Elves (rucksacks: Rucksack array) =
  let mergeSack rucksack =
    let (a, b) = rucksack
    Set.union a b

  let shared =
    rucksacks
    |> Seq.map mergeSack
    |> Seq.reduce (fun first next ->
      Set.intersect first next
    )
    |> Seq.toArray

  priorityForChar shared[0]

let sumOf3ElfBadgePriorities (rucksacks: Rucksack seq) =
  rucksacks
  |> Seq.chunkBySize 3
  |> Seq.map priorityFor3Elves
  |> Seq.sum

let rucksacks =
  File.ReadLines fileName
  |> Seq.map (fun line ->
    let s = Seq.toArray (split (line.Length / 2) line)
    (Set.ofArray s[0], Set.ofArray s[1])
  )

printfn "Sum of the shared rucksack priorities: %d" (sumOfRucksackPriorities rucksacks)
printfn "Sum of the 3 elf group badge priorities: %d" (sumOf3ElfBadgePriorities rucksacks)
