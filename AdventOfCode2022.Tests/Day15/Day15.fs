module Tests.Day15

open Day15
open Xunit
open Xunit.Abstractions

type Day15(output: ITestOutputHelper) =
  [<Theory>]
  [<InlineData("Day15/sample.txt", 10, 26)>]
  [<InlineData("Day15/input.txt", 2000000, -1)>]
  member _.``Find the number of positions which cannot contain a beacon``(fileName: string, row: int, expected: int) =
    let result = SensorReport.parse fileName |> Tunnels.positionsWithoutBeacon row
    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Day15/sample.txt", -1)>]
  [<InlineData("Day15/input.txt", -1)>]
  member _.``Part 2``(fileName: string, expected: int) =
    let result = -1
    Assert.Equal(expected, result)
