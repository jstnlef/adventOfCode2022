module Day4

open System
open System.IO

type Pair = int Set * int Set

let IsPairFullyContained (pair: Pair) =
  let a, b = pair
  let maxSize =
    [a; b]
    |> List.map Set.count
    |> List.max
  let union = Set.union a b
  union.Count = maxSize

let findFullyContainedAssignments (assignments: Pair seq) =
  assignments
  |> Seq.filter IsPairFullyContained
  |> Seq.length

let isPairOverlapping (pair: Pair) =
  let a, b = pair
  let intersection = Set.intersect a  b
  intersection.Count > 0

let findOverlappingAssignments (assignments: Pair seq) =
  assignments
  |> Seq.filter isPairOverlapping
  |> Seq.length

let parseAssignmentPairs fileName =
  let parseLine (line: string): Pair =
    let split = line.Split ","
    let sets =
      split
      |> Array.map (fun s ->
        let range = s.Split "-" |> Array.map int
        seq {range[0] .. range[1]}
      )
      |> Array.map Set.ofSeq
    sets[0], sets[1]

  File.ReadLines fileName
  |> Seq.map parseLine
