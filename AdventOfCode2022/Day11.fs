module Day11

open System.IO
open System.Text.RegularExpressions

type Item = int

type Monkey =
  { id: int
    items: Item list
    operation: int -> int
    test: int -> bool
    ifTrueMonkey: int
    ifFalseMonkey: int
    inspected: int }

module Monkey =
  let didThrow monkey =
    { monkey with items = List.tail monkey.items }

  let throwTo item monkey =
    { monkey with items = monkey.items @ [ item ] }

  let inspectedItem monkey =
    { monkey with inspected = monkey.inspected + 1 }

type Quagmire = { monkeys: Monkey array }

module Quagmire =
  let doMonkeyBusiness monkeyId quagmire : Quagmire =
    let handleItem quagmire item : Quagmire =
      let monkey = quagmire.monkeys[monkeyId]
      let newWorry = (monkey.operation item) / 3
      let throwingMonkey = monkey |> Monkey.inspectedItem |> Monkey.didThrow

      if monkey.test newWorry then
        let thrownToMonkey =
          quagmire.monkeys[monkey.ifTrueMonkey] |> Monkey.throwTo newWorry

        let updated =
          quagmire.monkeys
          |> Array.updateAt monkey.id throwingMonkey
          |> Array.updateAt monkey.ifTrueMonkey thrownToMonkey

        { quagmire with monkeys = updated }
      else
        let thrownToMonkey =
          quagmire.monkeys[monkey.ifFalseMonkey] |> Monkey.throwTo newWorry

        let updated =
          quagmire.monkeys
          |> Array.updateAt monkey.id throwingMonkey
          |> Array.updateAt monkey.ifFalseMonkey thrownToMonkey

        { quagmire with monkeys = updated }

    quagmire.monkeys[monkeyId].items |> List.fold handleItem quagmire

  let runRound quagmire : Quagmire =
    seq { 0 .. quagmire.monkeys.Length - 1 }
    |> Seq.fold (fun q id -> doMonkeyBusiness id q) quagmire

  let executeShenanigans rounds quagmire : Quagmire =
    seq { 0 .. rounds - 1 } |> Seq.fold (fun q _ -> runRound q) quagmire

  let totalMonkeyBusiness quagmire : int =
    quagmire.monkeys
    |> Seq.map (fun m -> m.inspected)
    |> Seq.sortDescending
    |> Seq.take 2
    |> Seq.reduce (*)

  let parse filename : Quagmire =
    let isDivisible d n = (n % d) = 0
    let operation op n old : int = op old n
    let specOp op old : int = op old old

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
        | n -> operation op (int n)

      let items = m.Groups[ "items" ].Value.Split(", ") |> Seq.map int |> Seq.toList

      { id = (int m.Groups["id"].Value)
        items = items
        test = isDivisible (int m.Groups["divisible"].Value)
        operation = parseOp m.Groups["op"].Value m.Groups["n"].Value
        ifTrueMonkey = int m.Groups["ifTrue"].Value
        ifFalseMonkey = int m.Groups["ifFalse"].Value
        inspected = 0 }

    let monkeys =
      File.ReadAllText filename
      |> (fun s -> s.Split "\n\n")
      |> Seq.map parseMonkey
      |> Seq.toArray

    { monkeys = monkeys }
