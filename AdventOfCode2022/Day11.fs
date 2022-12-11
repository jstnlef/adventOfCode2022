module Day11

open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharp.Core

type MonkeyId = int
type Item = uint64

type Monkey =
  { id: MonkeyId
    items: Item list
    operation: Item -> Item
    divisibleTest: uint64
    ifTrueMonkey: MonkeyId
    ifFalseMonkey: MonkeyId
    itemsInspected: uint64 }

module Monkey =
  let withItemThrown monkey =
    { monkey with items = List.tail monkey.items }

  let withItemCaught item monkey =
    { monkey with items = monkey.items @ [ item ] }

  let incrementItemInspected monkey =
    { monkey with itemsInspected = monkey.itemsInspected + 1UL }

type Quagmire = { monkeys: Monkey array }

module Quagmire =
  let private monkeyThrowsItem thrower item catcherId quagmire =
    let catcher = quagmire.monkeys[catcherId]

    let postThrowMonkeys =
      quagmire.monkeys
      |> Array.updateAt thrower.id (thrower |> Monkey.withItemThrown)
      |> Array.updateAt catcherId (catcher |> Monkey.withItemCaught item)

    { quagmire with monkeys = postThrowMonkeys }

  let private performShenanigan worryCalc monkeyId quagmire item : Quagmire =
    let monkey = quagmire.monkeys[monkeyId] |> Monkey.incrementItemInspected
    let itemWithNewWorry = worryCalc item monkey
    let throwItemWithNewWorryToMonkey = monkeyThrowsItem monkey itemWithNewWorry

    quagmire
    |> if (itemWithNewWorry % monkey.divisibleTest) = 0UL then
         throwItemWithNewWorryToMonkey monkey.ifTrueMonkey
       else
         throwItemWithNewWorryToMonkey monkey.ifFalseMonkey

  let private doMonkeyBusiness worryCalc quagmire monkeyId : Quagmire =
    quagmire.monkeys[monkeyId].items
    |> List.fold (performShenanigan worryCalc monkeyId) quagmire

  let runRound worryCalc quagmire : Quagmire =
    let doMonkeyBusinessWithWorry = doMonkeyBusiness worryCalc

    seq { 0 .. quagmire.monkeys.Length - 1 }
    |> Seq.fold doMonkeyBusinessWithWorry quagmire

  let executeShenanigans rounds worryCalc quagmire : Quagmire =
    let runRoundWithWorry q _ = runRound worryCalc q
    seq { 0 .. rounds - 1 } |> Seq.fold runRoundWithWorry quagmire

  let totalMonkeyBusiness quagmire : uint64 =
    quagmire.monkeys
    |> Seq.map (fun m -> m.itemsInspected)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

  let parse filename : Quagmire =
    let operation op n old = op old n
    let specOp op old = op old old

    let monkeyRegex =
      Regex(
        "^Monkey (?<id>\d+):\n  Starting items: (?<items>.+)+\n  Operation: new = old (?<op>\*|\+) (?<n>\d+|old)\n  Test: divisible by (?<divisible>\d+)\n    If true: throw to monkey (?<ifTrue>\d+)\n    If false: throw to monkey (?<ifFalse>\d+)"
      )

    let parseMonkey monkeyText : Monkey =
      let m = monkeyRegex.Match monkeyText

      let parseOp opStr operandStr =
        let op =
          match opStr with
          | "*" -> (*)
          | "+" -> (+)
          | _ -> failwith "unknown op"

        match operandStr with
        | "old" -> specOp op
        | n -> operation op (uint64 n)

      let items = m.Groups[ "items" ].Value.Split(", ") |> Seq.map uint64 |> Seq.toList

      { id = (int m.Groups["id"].Value)
        items = items
        divisibleTest = uint64 m.Groups["divisible"].Value
        operation = parseOp m.Groups["op"].Value m.Groups["n"].Value
        ifTrueMonkey = int m.Groups["ifTrue"].Value
        ifFalseMonkey = int m.Groups["ifFalse"].Value
        itemsInspected = 0UL }

    let monkeys =
      File.ReadAllText filename
      |> (fun s -> s.Split "\n\n")
      |> Seq.map parseMonkey
      |> Seq.toArray

    { monkeys = monkeys }
