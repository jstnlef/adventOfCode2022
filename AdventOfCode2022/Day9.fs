module Day9

open System.IO
open Microsoft.FSharp.Collections

type MoveAction =
  | Up of int
  | Right of int
  | Left of int
  | Down of int

type Position = int * int

type SimState =
  { knots: Position array
    positionsTailVisited: Position Set }

module RopeSim =
  let moveKnot head tail : Position =
    let headX, headY = head
    let tailX, tailY = tail

    let deltaX = headX - tailX
    let deltaY = headY - tailY

    if abs deltaX <= 1 && abs deltaY <= 1 then
      tail
    elif abs deltaX > abs deltaY && deltaX > 0 then
      (headX - 1, headY)
    elif abs deltaX > abs deltaY && deltaX < 0 then
      (headX + 1, headY)
    elif abs deltaY > abs deltaX && deltaY > 0 then
      (headX, headY - 1)
    elif abs deltaY > abs deltaX && deltaY < 0 then
      (headX, headY + 1)
    else
      failwith "eh?"

  let performStep deltaX deltaY state _ =
    let headX, headY = state.knots[0]

    let newHead =
      (headX + deltaX, headY + deltaY)

    state.knots[ 0 ] <- newHead

    for i in seq { 0 .. state.knots.Length - 2 } do
      let head, tail =
        state.knots[i], state.knots[i + 1]

      state.knots[ i + 1 ] <- moveKnot head tail

    { state with positionsTailVisited = Set.add state.knots[state.knots.Length - 1] state.positionsTailVisited }

  let performMovement state action =
    let deltaX, deltaY, steps =
      match action with
      | Up n -> (0, 1, n)
      | Down n -> (0, -1, n)
      | Right n -> (1, 0, n)
      | Left n -> (-1, 0, n)

    seq { steps - 1 .. -1 .. 0 }
    |> Seq.fold (performStep deltaX deltaY) state

  let simulate knots (actions: MoveAction seq) : SimState =
    let initial =
      { knots =
          seq { 0 .. knots - 1 }
          |> Seq.map (fun _ -> (0, 0))
          |> Seq.toArray
        positionsTailVisited = Set.empty }

    Seq.fold performMovement initial actions

  let parse filename : MoveAction seq =
    let parseLine (line: string) : MoveAction =
      let split = line.Split(" ")
      let n = int split[1]

      match split[0] with
      | "U" -> Up(n)
      | "D" -> Down(n)
      | "R" -> Right(n)
      | _ -> Left(n)

    File.ReadLines filename |> Seq.map parseLine
