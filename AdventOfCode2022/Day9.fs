module Day9

open System
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
  let private moveTailKnotRelativeToHead (headX, headY) (tailX, tailY) : Position =
    let deltaX = headX - tailX
    let deltaY = headY - tailY

    if abs deltaX <= 1 && abs deltaY <= 1 then
      (tailX, tailY)
    else
      (tailX + Math.Sign(deltaX), tailY + Math.Sign(deltaY))

  let private performStep (deltaX, deltaY) state _ =
    let headX, headY = state.knots[0]

    state.knots[ 0 ] <- (headX + deltaX, headY + deltaY)

    for i in seq { 0 .. state.knots.Length - 2 } do
      let head, tail =
        state.knots[i], state.knots[i + 1]

      state.knots[ i + 1 ] <- moveTailKnotRelativeToHead head tail

    { state with positionsTailVisited = Set.add state.knots[state.knots.Length - 1] state.positionsTailVisited }

  let private performMovement state action =
    let delta, steps =
      match action with
      | Up n -> (0, 1), n
      | Down n -> (0, -1), n
      | Right n -> (1, 0), n
      | Left n -> (-1, 0), n

    seq { steps - 1 .. -1 .. 0 }
    |> Seq.fold (performStep delta) state

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
