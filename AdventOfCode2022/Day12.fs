module Day12

open System.Collections.Generic
open System.IO

type Position =
  | Height of int
  | Start
  | BestSignal

type PositionIndex = int * int
type HeightMap = Position array array

module HeightMap =
  let private findNeighbors (i, j) (heightMap: HeightMap) : PositionIndex list =
    let currentPosition = heightMap[i][j]

    let findNeighbor i j : (int * int) option =
      if i < 0 || j < 0 || i >= heightMap.Length || j >= heightMap[0].Length then
        None
      else
        let newPosition = heightMap[i][j]

        match currentPosition, newPosition with
        | Height currentHeight, Height newHeight -> if newHeight <= currentHeight + 1 then Some(i, j) else None
        | Start, Height newHeight -> if newHeight <= 1 then Some(i, j) else None
        | Height currentHeight, BestSignal -> if currentHeight >= 25 then Some(i, j) else None
        | _ -> None


    let up = findNeighbor (i - 1) j
    let down = findNeighbor (i + 1) j
    let left = findNeighbor i (j - 1)
    let right = findNeighbor i (j + 1)
    [ up; down; left; right ] |> List.collect Option.toList

  let private findStartPositionIndex (heightMap: HeightMap) : PositionIndex =
    let size = heightMap.Length

    seq {
      for i in 0 .. size - 1 do
        for j in 0 .. size - 1 do
          yield (i, j)
    }
    |> Seq.find (fun (i, j) -> heightMap[i][j] = Start)

  let private findAllAs (heightMap: HeightMap) : PositionIndex seq =
    let size = heightMap.Length

    seq {
      for i in 0 .. size - 1 do
        for j in 0 .. size - 1 do
          yield (i, j)
    }
    |> Seq.filter (fun (i, j) ->
      let position = heightMap[i][j]

      match position with
      | Start -> true
      | Height n when n = 0 -> true
      | _ -> false)

  let pathToGoal start (heightMap: HeightMap) : PositionIndex seq =
    let frontier = Queue<PositionIndex>()
    frontier.Enqueue(start)

    let cameFrom = Dictionary<PositionIndex, PositionIndex option>()
    cameFrom[start] <- None
    let mutable endPos = None

    seq {
      while frontier.Count > 0 do
        let current = frontier.Dequeue()

        if heightMap[fst current][snd current] = BestSignal then
          endPos <- Some current

        for next in (findNeighbors current heightMap) do
          if not (cameFrom.ContainsKey(next)) then
            frontier.Enqueue(next)
            cameFrom[next] <- Some current

      while endPos.IsSome do
        yield endPos.Value
        endPos <- cameFrom[endPos.Value]
    }
    |> Seq.rev

  let shortestStepsToGoal heightMap : int =
    let start = findStartPositionIndex heightMap
    heightMap |> pathToGoal start |> Seq.length |> (fun n -> n - 1)

  let shortestStepsFromAnyA heightMap : int =
    findAllAs heightMap
    |> Seq.map (fun p -> heightMap |> pathToGoal p |> Seq.length |> (fun n -> n - 1))
    |> Seq.filter (fun pathSize -> pathSize >= 0)
    |> Seq.min

  let parse filename : HeightMap =
    let parseChar c =
      match c with
      | 'S' -> Start
      | 'E' -> BestSignal
      | c -> Height(int c - int 'a')

    let parseLine (line: string) =
      line |> Seq.map parseChar |> Seq.toArray

    File.ReadLines filename |> Seq.map parseLine |> Seq.toArray
