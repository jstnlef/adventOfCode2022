module Day5

open System
open System.Collections.Generic
open System.IO

type Supplies = Stack<char> array

type Step =
  {
    move: int
    fromStack: int
    toStack: int
  }

type Procedure = Step seq

module Step =
  let perform supplies step =
    supplies

module Supplies =
  let rearrange (procedure: Procedure) (supplies: Supplies): Supplies =
    Seq.fold Step.perform supplies procedure

  let top3Crates (supplies: Supplies): string =
    supplies
    |> Array.map (fun s -> s.Peek())
    |> String.Concat

let parseSuppliesAndProcedures fileName: Procedure * Supplies =
  [], [|Stack(['C']); Stack(['M']); Stack(['Z'])|]
