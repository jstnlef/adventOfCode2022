module Day5

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type Supplies = Stack<char> array

type Step =
  {
    move: int
    fromStack: int
    toStack: int
  }

type Procedure = Step seq

module Step =
  let perform (supplies: Supplies) (step: Step): Supplies =
    let fromStack = supplies[step.fromStack - 1]
    let toStack = supplies[step.toStack - 1]
    for _ in seq { 0 .. step.move - 1 } do
      let popped = fromStack.Pop()
      toStack.Push(popped)
    supplies

module Supplies =
  let rearrange (procedure: Procedure) (supplies: Supplies): Supplies =
    Seq.fold Step.perform supplies procedure

  let topCratesPerStack (supplies: Supplies): string =
    supplies
    |> Array.map (fun s -> s.Peek())
    |> String.Concat

let parseProcedure (input: string): Procedure =
  let regex = Regex("move (\d+) from (\d+) to (\d+)")
  regex.Matches(input)
  |> Seq.map (fun m ->
    {
      move=int m.Groups[1].Value
      fromStack=int m.Groups[2].Value
      toStack=int m.Groups[3].Value
    })

let parseStacks input =
  [|Stack(['Z'; 'N']); Stack(['M'; 'C'; 'D']); Stack(['P'])|]

let parseSuppliesAndProcedures fileName: Procedure * Supplies =
  let input = File.ReadAllText fileName
  let split = input.Split "\n\n"
  parseProcedure split[1], parseStacks split[0]
