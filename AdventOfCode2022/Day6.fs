module Day6

open System
open System.IO

type DataStream = string

let isDistinct chars =
  let charSet = Set.ofArray (chars |> Array.map snd )
  chars.Length = charSet.Count

let findStartOfPacketMarker (datastream: DataStream): int =
  let uniqueChars =
    datastream
    |> Seq.mapi (fun i c -> i, c)
    |> Seq.windowed 4
    |> Seq.find isDistinct

  let i, _ = uniqueChars[uniqueChars.Length - 1]
  i + 1

let parseDataStream fileName: DataStream =
  File.ReadAllText fileName
