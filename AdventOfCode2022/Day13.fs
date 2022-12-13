module Day13

open System.IO

type Packet =
  | Integer of int
  | List of Packet array

type PacketPair = Packet * Packet

module PacketPair =
  let rec isInCorrectOrder (left, right) : bool =
    let compareIntegers left right : bool = left <= right

    let compareLists (left: Packet array) (right: Packet array) : bool =
      let mutable i = 0
      let mutable state = true

      while (state && i < left.Length && i <= right.Length) do
        if i >= right.Length then
          state <- false
        else
          state <- isInCorrectOrder (left[i], right[i])

        i <- i + 1

      state

    match (left, right) with
    | Integer l, Integer r -> compareIntegers l r
    | List l, List r -> compareLists l r
    | Integer l, List r -> compareLists [| Integer(l) |] r
    | List l, Integer r -> compareLists l [| Integer(r) |]

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
