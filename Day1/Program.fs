open System.IO

let part1_single_delta depths =
  let mutable count = 0
  let mutable last = 10000
  for depth in depths do
    if depth > last then
      count <- count + 1
    last <- depth
  count

let part2_sliding_delta (depths: int array) =
  let mutable count = 0
  let mutable last = 1000000
  for i in seq { 0 .. depths.Length - 3 } do
    let x = depths[i]
    let y = depths[i+1]
    let z = depths[i+2]
    let sum = x + y + z
    if sum > last then
      count <- count + 1
    last <- sum
  count

let depths =
  File.ReadLines "Day1/input.txt"
  |> Seq.map int
  |> Seq.toArray

printfn "Part 1 Depth increased: %d" (part1_single_delta depths)
printfn "Part 2 Depth increased: %d" (part2_sliding_delta depths)
