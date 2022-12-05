module Day5

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions

type Supplies = Stack<string> array

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

let parseStacks (input: string): Supplies =
  let lines = input.Split "\n"
  let columns = lines[lines.Length - 1]
  let columnsRegex = Regex("(\d)")
  let columnMatches = columnsRegex.Matches columns

  let columnLookup =
    columnMatches
    |> Seq.mapi (fun i m -> m.Index, i)
    |> Map.ofSeq

  let stacks =
    columnMatches
    |> Seq.map (fun _ -> Stack<string>())
    |> Seq.toArray

  let addToStack (m: Match) =
    let group = m.Groups[1]
    let stackIndex = columnLookup[group.Index]
    stacks[stackIndex].Push(group.Value)

  let regex = Regex("\s*\[([A-Z])\]+")
  let collections =
    lines[0..lines.Length - 2]
    |> Seq.rev
    |> Seq.map regex.Matches

  for collection in collections do
    for m in collection do
      addToStack m

  stacks

let parseSuppliesAndProcedures fileName: Procedure * Supplies =
  let input = File.ReadAllText fileName
  let split = input.Split "\n\n"
  parseProcedure split[1], parseStacks split[0]
