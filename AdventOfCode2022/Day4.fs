module Day4

open System.IO

type Assignment = int Set
type Pair = Assignment * Assignment

module Pair =
  let assignmentsAreFullyContained (pair: Pair) =
    let a, b = pair
    let maxSize =
      [a; b]
      |> List.map Set.count
      |> List.max
    let union = Set.union a b
    union.Count = maxSize

  let assignmentsAreOverlapping (pair: Pair) =
    let a, b = pair
    let intersection = Set.intersect a  b
    intersection.Count > 0

let countAssignments filterFunc assignments  =
  assignments
  |> Seq.filter filterFunc
  |> Seq.length

let parseAssignmentPairs fileName =
  let parseLine (line: string) =
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
