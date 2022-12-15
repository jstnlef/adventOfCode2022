module Day15

type SensorReportEntry =
  { sensor: int * int; beacon: int * int }

type SensorReport = SensorReportEntry seq

module SensorReport =
  open System.IO
  open System.Text.RegularExpressions

  let entryRegex =
    Regex(
      "Sensor at x=(?<sensorX>-?\d+), y=(?<sensorY>-?\d+): closest beacon is at x=(?<beaconX>-?\d+), y=(?<beaconY>-?\d+)"
    )

  let parse filename : SensorReport =
    let parseLine line =
      let m = entryRegex.Match(line)

      { sensor = int m.Groups["sensorX"].Value, int m.Groups["sensorY"].Value
        beacon = int m.Groups["beaconX"].Value, int m.Groups["beaconY"].Value }

    File.ReadLines filename |> Seq.map parseLine

type Location =
  | Sensor
  | Beacon
  | NoBeacon
  | Unknown

type Map = Location array array

let manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

module Tunnels =
  let generateMap row report : Map =
    // Run a flood fill on the tunnels and then select the entries which are `NoBeacon`

    Array.empty

  let positionsWithoutBeacon (row: int) (report: SensorReport) : int =
    let mapForRow = generateMap row report

    -1

// let render tunnels : string =
//   let renderLocation location =
//     match location with
//     | Sensor -> "S"
//     | Beacon -> "B"
//     | NoBeacon -> "#"
//     | Unknown -> "."
//
//   tunnels.map
//   |> Array.map (fun row -> String.concat "" (row |> Array.map renderLocation))
//   |> String.concat "\n"
