module Day6

open System.IO

type DataStream = string

module DataStream =
  let private findStartOfMarker (markerSize: int) (datastream: DataStream): int =
    let isDistinct chars =
      let charSet = chars |> Array.map snd |> Set.ofArray
      chars.Length = charSet.Count

    let marker =
      datastream
      |> Seq.mapi (fun i c -> i, c)
      |> Seq.windowed markerSize
      |> Seq.find isDistinct

    let index = marker[marker.Length - 1] |> fst
    index + 1

  let findStartOfPacketMarker = findStartOfMarker 4

  let findStartOfMessageMarker = findStartOfMarker 14

  let parse fileName: DataStream = File.ReadAllText fileName
