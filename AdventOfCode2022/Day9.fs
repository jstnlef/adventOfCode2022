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
  { head: Position
    tail: Position
    positionsTailVisited: Position Set }

module RopeSim =
  let moveTail head tail : Position =
    let headX, headY = head
    let tailX, tailY = tail

    let deltaX = headX - tailX
    let deltaY = headY - tailY

    if abs deltaX <= 1 && abs deltaY <= 1 then
      tail
    elif abs deltaX > abs deltaY && deltaX > 0 then
      headX - 1, headY
    elif abs deltaX > abs deltaY && deltaX < 0 then
      headX + 1, headY
    elif abs deltaY > abs deltaX && deltaY > 0 then
      headX, headY - 1
    else
      headX, headY + 1

  let performStep deltaX deltaY state _ =
    let headX, headY = state.head

    let newHead =
      (headX + deltaX, headY + deltaY)

    let newTail = moveTail newHead state.tail

    { state with
        head = newHead
        tail = newTail
        positionsTailVisited = Set.add newTail state.positionsTailVisited }

  let performMovement state action =
    let deltaX, deltaY, steps =
      match action with
      | Up n -> (0, 1, n)
      | Down n -> (0, -1, n)
      | Right n -> (1, 0, n)
      | Left n -> (-1, 0, n)

    seq { steps - 1 .. -1 .. 0 }
    |> Seq.fold (performStep deltaX deltaY) state

  let simulate (actions: MoveAction seq) : SimState =
    let initial =
      { head = (0, 0)
        tail = (0, 0)
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
