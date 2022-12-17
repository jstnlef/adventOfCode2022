module Day16

open System.Text.RegularExpressions

type Pipe =
  { id: int
    name: string
    flowRate: int
    valves: string array }

type Pipes =
  { pipes: Pipe array
    distances: int array array
    nonZeroFlowPipes: Pipe array }

// let memoize f =
//   let dict = Dictionary<_, _>()
//
//   fun c ->
//     let exists, value = dict.TryGetValue c
//
//     match exists with
//     | true -> value
//     | _ ->
//       let value = f c
//       dict.Add(c, value)
//       value

module rec Pipes =
  open System.IO

  let calcPressureReleased delta openValves =
    if delta < 0 then
      failwith "Shouldn't calc pressure with negative delta"

    openValves
    |> Set.toSeq
    |> Seq.map (fun flow -> flow.flowRate * delta)
    |> Seq.sum

  let rec findMaxPressureForPipe pipeA minutesLeft totalReleased openValves discovered pipes : int =
    seq {
      if minutesLeft < 0 then
        failwith "This should not be called with less than 0 minutes left"
      else
        // Check each of the pipes I could travel to with non-zero flow.
        for pipeB in pipes.nonZeroFlowPipes do
          if pipeA.name <> pipeB.name && not (Set.contains pipeB.name discovered) then
            let discovered = Set.add pipeB.name discovered
            let delta = pipes.distances[pipeA.id][pipeB.id] + 1
            let minutesLeft = minutesLeft - delta

            if minutesLeft > 0 then
              let totalReleased = totalReleased + (calcPressureReleased delta openValves)
              yield findMaxPressureForPipe pipeB minutesLeft totalReleased (Set.add pipeB openValves) discovered pipes

        // The value if I just waited here until 0
        yield totalReleased + (calcPressureReleased (minutesLeft + 1) openValves)
    }
    |> Seq.max

  // let memFindMaxPressureForPipe = memoize findMaxPressureForPipe

  let findMostPressureYouCanRelease minutes (pipes: Pipes) : int =
    findMaxPressureForPipe pipes.pipes[0] minutes 0 Set.empty Set.empty pipes

  let findMostPressureYouAndAnElephantCanRelease minutes (pipes: Pipes) : int =
    findMaxPressureForPipe pipes.pipes[0] minutes 0 Set.empty Set.empty pipes

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
      distances = calculateShortestDistances pipes
      nonZeroFlowPipes = pipes |> Array.filter (fun p -> p.flowRate > 0) }
