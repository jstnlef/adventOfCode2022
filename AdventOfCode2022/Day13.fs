module Day13

open System
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

/// Grammar
/// Integer = Digit
/// IntList = Integer "," InnerList
/// Inner = Integer | List | IntList
/// List = "[" InnerList "]"
/// Packet = List
module Parsing =
  let rec private parseInteger (chars: char list) : Packet * char list =
    let peek = chars.Head

    if peek = '[' || peek = ']' || peek = ',' then
      failwith "not an int!"

    let n =
      chars
      |> List.mapi (fun i c -> i, c)
      |> List.takeWhile (fun (_, c) -> c <> ',' && c <> ']')

    let sliceIndex = n[n.Length - 1] |> fst
    let v = String.Concat(n |> Seq.map snd) |> int

    Integer(v), chars[sliceIndex + 1 ..]

  let private parseLeftBracket chars =
    match chars with
    | '[' :: rest -> rest
    | _ -> failwith "not a left bracket!"

  let private parseRightBracket chars =
    match chars with
    | ']' :: rest -> rest
    | _ -> failwith "not a left bracket!"

  let rec private parseList (chars: char list) : Packet * char list =
    let rec parseInner (chars: char list) : Packet array * char list =
      let peek = chars[0]

      if peek = ']' then
        [||], chars
      else
        try
          let i, rest = parseInteger chars
          [| i |], rest
        with Failure _ ->
          let i, rest = parseList chars
          [| i |], rest

    let postLeft = parseLeftBracket chars
    let inner, postInner = parseInner postLeft
    let rest = parseRightBracket postInner
    List(inner), rest

  let parsePacket (line: string) =
    (List.ofArray (line.ToCharArray())) |> parseList |> fst

module DistressSignal =
  let findCorrectPairs (distressSignal: DistressSignal) : int seq =
    distressSignal
    |> Seq.filter PacketPair.isInCorrectOrder
    |> Seq.mapi (fun i _ -> i + 1)

  let rec parse filename : DistressSignal =
    File.ReadAllText filename
    |> (fun s -> s.Split("\n\n"))
    |> Array.map (fun s ->
      let split = s.Split("\n")
      Parsing.parsePacket split[0], Parsing.parsePacket split[1])
