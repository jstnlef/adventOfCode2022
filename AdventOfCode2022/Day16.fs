module Day16

open System.Collections.Generic
open System.Text.RegularExpressions

type Pipe =
  { id: int
    name: string
    flowRate: int
    valves: string array }

type Pipes =
  { pipes: Pipe array
    distances: int array array }

let memoize f =
  let dict = Dictionary<_, _>()

  fun c ->
    let exists, value = dict.TryGetValue c

    match exists with
    | true -> value
    | _ ->
      let value = f c
      dict.Add(c, value)
      value

module Pipes =
  open System.IO

  let findMostPressureReleased minutes (pipes: Pipes) : int =
    let nonZeroFlowPipes = pipes.pipes |> Array.filter (fun p -> p.flowRate > 0)

    let rec findMaxPressureForPipe pipeA (openValves: int Set) minutesLeft totalReleased : int =
      if minutesLeft <= 0 then
        totalReleased
      else
        seq {
          let mutable discovered = Set.empty

          for pipeB in nonZeroFlowPipes do
            if pipeA.id <> pipeB.id then
              if discovered |> Set.contains pipeB |> not then
                discovered <- Set.add pipeB discovered
                let delta = pipes.distances[pipeA.id][pipeB.id]
                let minutesLeftAfter = minutesLeft - delta
                let newFlowRate = (openValves |> Set.toSeq |> Seq.sum) * delta
                let openValves = Set.add pipeB.flowRate openValves

                yield findMaxPressureForPipe pipeB openValves (minutesLeftAfter - 1) (totalReleased + newFlowRate)
        }
        |> Seq.max

    findMaxPressureForPipe pipes.pipes[0] Set.empty minutes 0

  // Floyd–Warshall algorithm to generate shortest steps from any 2 valves
  let private calculateShortestDistances (pipes: Pipe array) : int array array =
    let numPipes = pipes.Length
    let paths = Array.init numPipes (fun _ -> Array.create numPipes 10000)

    for pipe in pipes do
      let i = pipe.id
      paths[i][i] <- 0

      for valve in pipe.valves do
        pipes
        |> Array.tryFindIndex (fun pipe -> pipe.name = valve)
        |> Option.iter (fun j -> paths[i][j] <- 1)

    for k in { 0 .. paths.Length - 1 } do
      for i in { 0 .. paths.Length - 1 } do
        for j in { 0 .. paths.Length - 1 } do
          if paths[i][j] > (paths[i][k] + paths[k][j]) then
            paths[i][j] <- paths[i][k] + paths[k][j]

    paths

  let regex =
    Regex("Valve (?<id>\w\w) has flow rate=(?<flowRate>\d+); tunnels? leads? to valves? (?<valves>.*)")

  let parse filename : Pipes =
    let parseLine i line =
      let m = regex.Match(line)

      { id = i
        name = m.Groups["id"].Value
        flowRate = int m.Groups["flowRate"].Value
        valves = m.Groups[ "valves" ].Value.Split(", ") }

    let pipes = File.ReadLines filename |> Seq.mapi parseLine |> Seq.toArray

    { pipes = pipes
      distances = calculateShortestDistances pipes }
