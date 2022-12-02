open System.IO

type Selection = Rock | Paper | Scissors
type Throw = Selection * Selection

let calculateScoreForSelection selection =
  match selection with
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3

let calculateScoreForThrow (throw: Throw) =
  let (opponent, mine) = throw
  let outcome =
    match (opponent, mine) with
    | (Rock, Scissors) -> 0
    | (Rock, Paper) -> 6
    | (Paper, Rock) -> 0
    | (Paper, Scissors) -> 6
    | (Scissors, Paper) -> 0
    | (Scissors, Rock) -> 6
    | _ -> 3

  outcome + calculateScoreForSelection mine

let calculateScore (guide: Throw seq) =
  guide
  |> Seq.map calculateScoreForThrow
  |> Seq.sum

let guide =
  File.ReadLines "Day2/input.txt"
  |> Seq.map (fun s ->
    let split = s.Split " "
    let parsedOpponent =
      match split[0] with
      | "A" -> Rock
      | "B" -> Paper
      | "C" -> Scissors
      | _ -> failwith "Unknown selection"

    let parsedMine =
      match split[1] with
      | "X" -> Rock
      | "Y" -> Paper
      | "Z" -> Scissors
      | _ -> failwith "Unknown selection"

    (parsedOpponent, parsedMine)
  )

printfn "Total score for guide: %d" (calculateScore guide)
