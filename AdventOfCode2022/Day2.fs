module Day2

open System.IO

type Selection = Rock = 1 | Paper = 2 | Scissors = 3
type Throw = Selection * Selection
type MistakenGuide = Throw seq

let calculateScoreForThrow (throw: Throw) =
  let outcome =
    match throw with
    | (Selection.Rock, Selection.Paper) -> 6
    | (Selection.Paper, Selection.Scissors) -> 6
    | (Selection.Scissors, Selection.Rock) -> 6
    | (Selection.Rock, Selection.Scissors) -> 0
    | (Selection.Paper, Selection.Rock) -> 0
    | (Selection.Scissors, Selection.Paper) -> 0
    | _ -> 3

  let (_, mine) = throw
  outcome + (int)mine

module MistakenGuide =
  let calculateScore (guide: Throw seq) =
    guide
    |> Seq.map calculateScoreForThrow
    |> Seq.sum

type Outcome = Win | Lose | Draw
type Round = Selection * Outcome
type ProperGuide = Round seq

module ProperGuide =
  let calculateScoreForRound round =
    let throw =
      match round with
      | (Selection.Rock, Win) -> (Selection.Rock, Selection.Paper)
      | (Selection.Rock, Draw) -> (Selection.Rock, Selection.Rock)
      | (Selection.Rock, Lose) -> (Selection.Rock, Selection.Scissors)
      | (Selection.Paper, Win) -> (Selection.Paper, Selection.Scissors)
      | (Selection.Paper, Draw) -> (Selection.Paper, Selection.Paper)
      | (Selection.Paper, Lose) -> (Selection.Paper, Selection.Rock)
      | (Selection.Scissors, Win) -> (Selection.Scissors, Selection.Rock)
      | (Selection.Scissors, Draw) -> (Selection.Scissors, Selection.Scissors)
      | (Selection.Scissors, Lose) -> (Selection.Scissors, Selection.Paper)
      | _ -> failwith "Shouldn't get here"
    calculateScoreForThrow throw

  let calculateScore (guide: Round seq) =
    guide
    |> Seq.map calculateScoreForRound
    |> Seq.sum

let parseMistakenGuide fileName: MistakenGuide =
  File.ReadLines fileName
  |> Seq.map (fun s ->
    let split = s.Split " "
    let parsedOpponent =
      match split[0] with
      | "A" -> Selection.Rock
      | "B" -> Selection.Paper
      | "C" -> Selection.Scissors
      | _ -> failwith "Unknown selection"

    let parsedMine =
      match split[1] with
      | "X" -> Selection.Rock
      | "Y" -> Selection.Paper
      | "Z" -> Selection.Scissors
      | _ -> failwith "Unknown selection"

    (parsedOpponent, parsedMine)
  )

let parseProperGuide fileName: ProperGuide =
  File.ReadLines fileName
  |> Seq.map (fun s ->
    let split = s.Split " "
    let opponentSelection =
      match split[0] with
      | "A" -> Selection.Rock
      | "B" -> Selection.Paper
      | "C" -> Selection.Scissors
      | _ -> failwith "Unknown selection"

    let expectedOutCome =
      match split[1] with
      | "X" -> Lose
      | "Y" -> Draw
      | "Z" -> Win
      | _ -> failwith "Unknown selection"

    (opponentSelection, expectedOutCome)
  )
