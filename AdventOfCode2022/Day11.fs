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
  let didThrow monkey =
    { monkey with items = List.tail monkey.items }

  let throwTo item monkey =
    { monkey with items = monkey.items @ [ item ] }

  let inspectedItem monkey =
    { monkey with itemsInspected = monkey.itemsInspected + 1UL }

type Quagmire = { monkeys: Monkey array }

module Quagmire =
  let monkeyThrowsItem thrower catcherId item quagmire =
    let catcher = quagmire.monkeys[catcherId]

    let postThrowMonkeys =
      quagmire.monkeys
      |> Array.updateAt thrower.id (thrower |> Monkey.didThrow)
      |> Array.updateAt catcherId (catcher |> Monkey.throwTo item)

    { quagmire with monkeys = postThrowMonkeys }

  let handleItem worryCalc monkeyId quagmire item : Quagmire =
    let monkey = quagmire.monkeys[monkeyId] |> Monkey.inspectedItem

    let newWorry = worryCalc item monkey

    if (newWorry % monkey.divisibleTest) = 0UL then
      monkeyThrowsItem monkey monkey.ifTrueMonkey newWorry quagmire
    else
      monkeyThrowsItem monkey monkey.ifFalseMonkey newWorry quagmire

  let doMonkeyBusiness worryCalc monkeyId quagmire : Quagmire =
    quagmire.monkeys[monkeyId].items
    |> List.fold (handleItem worryCalc monkeyId) quagmire

  let runRound worryCalc quagmire : Quagmire =
    seq { 0 .. quagmire.monkeys.Length - 1 }
    |> Seq.fold (fun q id -> doMonkeyBusiness worryCalc id q) quagmire

  let executeShenanigans rounds worryCalc quagmire : Quagmire =
    seq { 0 .. rounds - 1 } |> Seq.fold (fun q _ -> runRound worryCalc q) quagmire

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
