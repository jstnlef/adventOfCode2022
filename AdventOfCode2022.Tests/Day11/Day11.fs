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
  let result =
    Quagmire.parse fileName
    |> Quagmire.executeShenanigans 20 true
    |> Quagmire.totalMonkeyBusiness

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day11/sample.txt", 2713310158L)>]
[<InlineData("Day11/input.txt", 0)>]
let ``Level of monkey business after 10000 rounds`` (fileName: string, expected: uint64) =
  let result =
    Quagmire.parse fileName
    |> Quagmire.executeShenanigans 10000 false
    |> Quagmire.totalMonkeyBusiness

  Assert.Equal(expected, result)
