module Day13

open System
open System.IO

type Packet =
  | Integer of int
  | List of Packet array

type PacketPair = Packet * Packet

module PacketPair =
  let rec compare left right : int =
    let compareIntegers (left: int) (right: int) : int = left.CompareTo(right)

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

  let isInCorrectOrder (left, right) : bool = compare left right = -1

type DistressSignal = PacketPair array

/// Grammar
/// Value = Integer | List
/// List = "[" Value { "," Value } "]"
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
    | _ -> failwith "not a right bracket!"

  let private parseComma chars =
    match chars with
    | ',' :: rest -> rest
    | _ -> failwith "not a comma!"

  let rec private parseList (chars: char list) : Packet * char list =
    // Value = Integer | List
    let rec parseValue (chars: char list) : Packet * char list =
      let peek = chars[0]
      if peek = '[' then parseList chars else parseInteger chars

    let mutable rest = parseLeftBracket chars
    let mutable inner = []

    while rest[0] <> ']' do
      let packet, postValueChars = parseValue rest
      inner <- inner @ [ packet ]
      rest <- postValueChars

      if rest[0] = ',' then
        rest <- parseComma rest

    rest <- parseRightBracket rest

    List(List.toArray inner), rest

  let parsePacket (line: string) =
    (List.ofArray (line.ToCharArray())) |> parseList |> fst

module DistressSignal =
  let findCorrectPairs (distressSignal: DistressSignal) : int seq =
    distressSignal
    |> Seq.map PacketPair.isInCorrectOrder
    |> Seq.mapi (fun i correct -> if correct then i + 1 else -1)
    |> Seq.filter (fun i -> i <> -1)

  let findDecoderKey (distressSignal: DistressSignal) : int =
    let two = List([| List([| Integer(2) |]) |])
    let six = List([| List([| Integer(6) |]) |])

    let signalWithDividers =
      distressSignal |> Array.insertAt distressSignal.Length (two, six)

    let packets = signalWithDividers |> Array.collect (fun (l, r) -> [| l; r |])
    Array.sortInPlaceWith PacketPair.compare packets

    packets
    |> Seq.indexed
    |> Seq.filter (fun (_, p) -> p = two || p = six)
    |> Seq.map (fun p -> (p |> fst) + 1)
    |> Seq.reduce (*)

  let rec parse filename : DistressSignal =
    File.ReadAllText filename
    |> (fun s -> s.Split("\n\n"))
    |> Array.map (fun s ->
      let split = s.Split("\n")
      Parsing.parsePacket split[0], Parsing.parsePacket split[1])
