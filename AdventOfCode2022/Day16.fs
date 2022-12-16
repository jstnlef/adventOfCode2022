module Day16

open System.Text.RegularExpressions

type Pipe =
  { id: string
    flowRate: int
    valves: string array }

type Pipes = Pipe seq

module Pipes =
  open System.IO

  let findMostPressureReleased timeframe pipes = -1

  let regex =
    Regex("Valve (?<id>\w\w) has flow rate=(?<flowRate>\d+); tunnels? leads? to valves? (?<valves>.*)")

  let parse filename =
    let parseLine line =
      let m = regex.Match(line)

      { id = m.Groups["id"].Value
        flowRate = int m.Groups["flowRate"].Value
        valves = m.Groups[ "valves" ].Value.Split(",") }

    File.ReadLines filename |> Seq.map parseLine
