module Day8

open System.IO

type TreeHeightMap = int array array

module TreeHeightMap =
  let isVisible (heightMap: TreeHeightMap) (i, j) : bool =
    let size = heightMap.Length - 1
    let current = heightMap[i][j]
    let lessThanCurrent tree = tree < current

    let up =
      seq { i - 1 .. -1 .. 0 }
      |> Seq.forall (fun n -> lessThanCurrent (heightMap[n][j]))

    let down =
      seq { i + 1 .. size }
      |> Seq.forall (fun n -> lessThanCurrent (heightMap[n][j]))

    let right =
      seq { j + 1 .. size }
      |> Seq.forall (fun n -> lessThanCurrent (heightMap[i][n]))

    let left =
      seq { j - 1 .. -1 .. 0 }
      |> Seq.forall (fun n -> lessThanCurrent (heightMap[i][n]))

    up || down || right || left

  let iterIndexes size : (int * int) seq =
    seq {
      for i in seq { 0 .. size - 1 } do
        for j in seq { 0 .. size - 1 } do
          yield (i, j)
    }

  let findVisibleTrees (heightMap: TreeHeightMap) : int =
    let size = heightMap.Length

    iterIndexes size
    |> Seq.filter (isVisible heightMap)
    |> Seq.length

  let countTrees (heightMap: TreeHeightMap) (currentHeight: int) (indexes: (int * int) seq) : int =
    let mutable allDone = false

    indexes
    |> Seq.takeWhile (fun (i, j) ->
      if allDone then
        false
      else
        let nextHeight = heightMap[i][j]

        if nextHeight >= currentHeight then
          allDone <- true

        true)
    |> Seq.length

  let calculateScenicScore (heightMap: TreeHeightMap) (i, j) : int =
    let size = heightMap.Length - 1
    let current = heightMap[i][j]

    let up =
      countTrees
        heightMap
        current
        (seq { i - 1 .. -1 .. 0 }
         |> Seq.map (fun n -> (n, j)))

    let down =
      countTrees heightMap current (seq { i + 1 .. size } |> Seq.map (fun n -> (n, j)))

    let right =
      countTrees heightMap current (seq { j + 1 .. size } |> Seq.map (fun n -> (i, n)))

    let left =
      countTrees
        heightMap
        current
        (seq { j - 1 .. -1 .. 0 }
         |> Seq.map (fun n -> (i, n)))

    up * down * right * left

  let calculateHighestScenicScore (heightMap: TreeHeightMap) : int =
    let size = heightMap.Length

    iterIndexes size
    |> Seq.map (calculateScenicScore heightMap)
    |> Seq.max

  let parse filename : TreeHeightMap =
    File.ReadLines filename
    |> Seq.map (fun s -> s |> Seq.map string |> Seq.map int |> Seq.toArray)
    |> Array.ofSeq
