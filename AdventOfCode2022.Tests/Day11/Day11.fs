module Tests.Day11

open Xunit
open Day11

[<Theory>]
[<InlineData("Day11/sample.txt", 10605)>]
[<InlineData("Day11/input.txt", 111210)>]
let ``Level of monkey business after 20 rounds of stuff-slinging simian shenanigans``
  (
    fileName: string,
    expected: uint64
  ) =
  let worry item monkey = monkey.operation item / 3UL

  let result =
    Quagmire.parse fileName
    |> Quagmire.executeShenanigans 20 worry
    |> Quagmire.totalMonkeyBusiness

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day11/sample.txt", 2713310158UL)>]
[<InlineData("Day11/input.txt", 15447387620UL)>]
let ``Level of monkey business after 10000 rounds`` (fileName: string, expected: uint64) =
  let quagmire = Quagmire.parse fileName

  let worry item monkey =
    let allTests =
      quagmire.monkeys |> Seq.map (fun m -> m.divisibleTest) |> Seq.reduce (*)

    (monkey.operation item) % allTests

  let result =
    quagmire
    |> Quagmire.executeShenanigans 10000 worry
    |> Quagmire.totalMonkeyBusiness

  Assert.Equal(expected, result)
