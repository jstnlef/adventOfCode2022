module Tests.Day11

open Xunit
open Day11

[<Theory>]
[<InlineData("Day11/sample.txt", 10605)>]
[<InlineData("Day11/input.txt", 111210)>]
let ``Level of monkey business after 20 rounds of stuff-slinging simian shenanigans``
  (
    fileName: string,
    expected: int
  ) =
  let result =
    Quagmire.parse fileName
    |> Quagmire.executeShenanigans 20
    |> Quagmire.totalMonkeyBusiness

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day11/sample.txt", 0)>]
[<InlineData("Day11/input.txt", 0)>]
let ``Part 2`` (fileName: string, expected: int) =
  let result = - 1
  Assert.Equal(expected, result)
