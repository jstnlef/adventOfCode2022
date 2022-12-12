module Day12

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
      if i < 0 || j < 0 || i >= heightMap.Length || j >= heightMap.Length then
        None
      else
        let newPosition = heightMap[i][j]

        match currentPosition, newPosition with
        | Height currentHeight, Height newHeight -> if newHeight <= currentHeight + 1 then Some(i, j) else None
        | Start, _ -> Some(i, j)
        | _, BestSignal -> Some(i, j)
        | _ -> None


    let up = findNeighbor (i - 1) j
    let down = findNeighbor (i + 1) j
    let left = findNeighbor i (j - 1)
    let right = findNeighbor i (j + 1)
    [ up; down; left; right ] |> List.collect Option.toList

  let private findStartIndexes (heightMap: HeightMap) : PositionIndex =
    let size = heightMap.Length

    seq {
      for i in 0 .. size - 1 do
        for j in 0 .. size - 1 do
          yield (i, j)
    }
    |> Seq.find (fun (i, j) -> heightMap[i][j] = Start)

  let pathToGoal (heightMap: HeightMap) : PositionIndex seq =
    let rec search
      (considered: PositionIndex Set)
      (neighbors: PositionIndex list)
      (path: PositionIndex list)
      : PositionIndex list =
      if List.isEmpty neighbors then
        path
      else
        let posIndex = List.head neighbors
        let position = posIndex |> (fun (i, j) -> heightMap[i][j])

        if position = BestSignal then
          posIndex :: path
        elif considered |> Set.contains posIndex then
          search considered (List.tail neighbors) path
        else
          let newConsidered = Set.add posIndex considered

          let newNeighbors =
            (List.tail neighbors @ (findNeighbors posIndex heightMap)) |> List.distinct

          search newConsidered newNeighbors (posIndex :: path)

    (search Set.empty [ findStartIndexes heightMap ] List.empty)
    |> List.rev
    |> List.toSeq

  let shortestStepsToGoal heightmap : int = heightmap |> pathToGoal |> Seq.length

  let parse filename : HeightMap =
    let parseChar c =
      match c with
      | 'S' -> Start
      | 'E' -> BestSignal
      | c -> Height(int c - int 'a')

    let parseLine (line: string) =
      line |> Seq.map parseChar |> Seq.toArray

    File.ReadLines filename |> Seq.map parseLine |> Seq.toArray
