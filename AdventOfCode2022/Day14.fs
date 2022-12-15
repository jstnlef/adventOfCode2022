module Day14

open System

type Location =
  | Rock
  | Air
  | Sand
  | Origin

type Cave =
  { filled: bool
    sand: int
    minX: int
    maxX: int
    minY: int
    maxY: int
    map: Location array array }

type RockLine = (int * int) seq
type CaveScan = RockLine seq

module CaveScan =
  open System.IO

  let generateFullRockLine (x1, y1) (x2, y2) =
    let (deltaX: int, deltaY: int) = x2 - x1, y2 - y1
    let signX, signY = Math.Sign(deltaX), Math.Sign(deltaY)

    seq {
      for i in 0 .. (abs deltaX + abs deltaY) do
        yield (x1 + (i * signX)), (y1 + (i * signY))
    }

  let parse filename =
    let parsePosition (position: string) =
      position.Split(",") |> (fun s -> int s[0], int s[1])

    let parseLine (line: string) =
      line.Split(" -> ") |> Seq.map parsePosition

    File.ReadLines filename |> Seq.map parseLine

module Cave =
  let originX, originY = 500, 0

  let getLocation (x, y) cave = cave.map[y][x - cave.minX]

  let setLocation (x, y) location cave = cave.map[y][x - cave.minX] <- location

  let fromScan scan : Cave =
    let xs = scan |> Seq.collect (fun rockLine -> rockLine |> Seq.map fst)
    let ys = scan |> Seq.collect (fun rockLine -> rockLine |> Seq.map snd)
    let minX = xs |> Seq.min
    let maxX = xs |> Seq.max
    let maxY = ys |> Seq.max

    let map = Array.init (maxY + 1 - 0) (fun _ -> Array.create (maxX + 1 - minX) Air)

    let placeRocks scan =
      for line in scan do
        for window in (Seq.windowed 2 line) do
          for rockX, rockY in (CaveScan.generateFullRockLine window[0] window[1]) do
            map[rockY][rockX - minX] <- Rock

    map[originY][originX - minX] <- Origin
    placeRocks scan

    { filled = false
      sand = 0
      minX = minX
      maxX = maxX
      minY = 0
      maxY = maxY
      map = map }

  let private isSolid loc =
    match loc with
    | Rock -> true
    | Air -> false
    | Sand -> true
    | Origin -> false

  let fromScanWithFloor scan : Cave =
    let xs = scan |> Seq.collect (fun rockLine -> rockLine |> Seq.map fst)
    let ys = scan |> Seq.collect (fun rockLine -> rockLine |> Seq.map snd)
    let minX = xs |> Seq.min
    let maxX = xs |> Seq.max
    let maxY = ys |> Seq.max

    scan
    |> Seq.insertAt (Seq.length scan) ([ (minX - 150, maxY + 2); (maxX + 150, maxY + 2) ] |> List.toSeq)
    |> fromScan

  let simulateSandDrop cave : Cave =
    let mutable sandX, sandY = originX, originY
    let mutable resting = false

    try
      while not resting do
        let down = (sandX, sandY + 1)
        let downLeft = (sandX - 1, sandY + 1)
        let downRight = (sandX + 1, sandY + 1)

        if not (isSolid (getLocation down cave)) then
          sandY <- sandY + 1
        elif not (isSolid (getLocation downLeft cave)) then
          sandX <- sandX - 1
          sandY <- sandY + 1
        elif not (isSolid (getLocation downRight cave)) then
          sandX <- sandX + 1
          sandY <- sandY + 1
        else
          resting <- true

      cave |> setLocation (sandX, sandY) Sand

      { cave with sand = cave.sand + 1 }

    with :? IndexOutOfRangeException ->
      { cave with filled = true }

  let simulateUntilFull cave : Cave =
    seq {
      let mutable cave = cave

      let mutable origin = Origin

      while not cave.filled && origin = Origin do
        cave <- simulateSandDrop cave
        origin <- getLocation (originX, originY) cave
        yield cave
    }
    |> Seq.last

  let render cave : string =
    let renderLocation location =
      match location with
      | Air -> "."
      | Rock -> "#"
      | Sand -> "o"
      | Origin -> "+"

    cave.map
    |> Array.map (fun row -> String.concat "" (row |> Array.map renderLocation))
    |> String.concat "\n"
