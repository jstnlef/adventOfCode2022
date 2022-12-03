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

let priorityForRucksack (rucksack: Rucksack) =
  let (first, second) = rucksack
  let shared = ((Set.intersect first second) |> Set.toArray)[0]
  if (Char.IsUpper(shared)) then
    int shared - int 'A' + 27
  else
    int shared - int 'a' + 1

let sumOfPriorities rucksacks =
  rucksacks
  |> Seq.map priorityForRucksack
  |> Seq.sum

let rucksacks =
  File.ReadLines fileName
  |> Seq.map (fun line ->
    let s = Seq.toArray (split (line.Length / 2) line)
    (Set.ofArray s[0], Set.ofArray s[1])
  )

printfn "Sum of the rucksack priorities: %d" (sumOfPriorities rucksacks)
