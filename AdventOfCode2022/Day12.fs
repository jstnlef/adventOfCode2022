module Day12

open System.IO

type Position =
  | Height of int
  | Start
  | BestSignal

type HeightMap = Position array array

module HeightMap =
  let shortestStepsToGoal heightmap : int =
    // TODO: Should be pretty easy to to implement A* or breadth first search to get this done. Just no time now.
    0

  let parse filename : HeightMap =
    let parseChar c =
      match c with
      | 'S' -> Start
      | 'E' -> BestSignal
      | c -> Height(int c - int 'a')

    let parseLine (line: string) =
      line |> Seq.map parseChar |> Seq.toArray

    File.ReadLines filename |> Seq.map parseLine |> Seq.toArray
