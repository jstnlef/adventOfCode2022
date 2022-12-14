module Day13

open System.IO

type Packet =
  | Integer of int
  | List of Packet array

type PacketPair = Packet * Packet

module PacketPair =
  let isInCorrectOrder (left, right) : bool =
    let compareIntegers (left: int) (right: int) : int = left.CompareTo(right)

    let rec compare left right : int =
      let compareLists (left: Packet array) (right: Packet array) : int =
        let maxLength = [ left.Length; right.Length ] |> List.max
        let mutable i = 0
        let mutable state = 0

        while (state = 0 && i < maxLength) do
          if i >= left.Length && i < right.Length then state <- -1
          elif i < left.Length && i >= right.Length then state <- 1
          else state <- compare left[i] right[i]

          i <- i + 1

        state

      match (left, right) with
      | Integer l, Integer r -> compareIntegers l r
      | List l, List r -> compareLists l r
      | Integer l, List r -> compareLists [| Integer(l) |] r
      | List l, Integer r -> compareLists l [| Integer(r) |]

    compare left right = -1

type DistressSignal = PacketPair array

module DistressSignal =
  let findCorrectPairs (distressSignal: DistressSignal) : int seq =
    distressSignal
    |> Seq.filter PacketPair.isInCorrectOrder
    |> Seq.mapi (fun i _ -> i + 1)

  let parse filename : DistressSignal =
    let parseLine line = Integer 0

    File.ReadAllText filename
    |> (fun s -> s.Split("\n\n"))
    |> Array.map (fun s ->
      let split = s.Split("\n")
      parseLine split[0], parseLine split[1])
