module Day8

type TreeHeightMap = int array array

open System.IO

module TreeHeightMap =
  let isOnEdge (i, j) (heightMap: TreeHeightMap): bool =
    let size = heightMap.Length - 1
    i = 0 || i = size || j = 0 || j = size

  let noTallerNeighbors (i, j) (heightMap: TreeHeightMap): bool =
    let size = heightMap.Length - 1
    let current = heightMap[i][j]
    let lessThanCurrent tree = tree < current
    let up = seq { i - 1 .. -1 .. 0 } |> Seq.forall (fun n -> lessThanCurrent (heightMap[n][j]))
    let down = seq { i + 1 .. size } |> Seq.forall (fun n -> lessThanCurrent (heightMap[n][j]))
    let right = seq { j + 1 .. size } |> Seq.forall (fun n -> lessThanCurrent (heightMap[i][n]))
    let left = seq { j - 1 .. -1 .. 0 } |> Seq.forall (fun n -> lessThanCurrent (heightMap[i][n]))
    up || down || right || left

  let isVisible (i, j) (heightMap: TreeHeightMap): bool =
    isOnEdge (i, j) heightMap || noTallerNeighbors (i, j) heightMap

  let findVisibleTrees (heightMap: TreeHeightMap): int =
    let mutable visibleTrees = 0
    let size = heightMap.Length
    for i in seq { 0 .. size - 1 } do
      for j in seq { 0 .. size - 1 } do
        if isVisible (i, j) heightMap then
          visibleTrees <- visibleTrees + 1
    visibleTrees

  let parse filename: TreeHeightMap =
    File.ReadLines filename
    |> Seq.map (fun s -> s |> Seq.map string |> Seq.map int |> Seq.toArray)
    |> Array.ofSeq
