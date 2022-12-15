module Tests.Day14

open Day14
open Xunit
open Xunit.Abstractions

type Day14(output: ITestOutputHelper) =
  [<Theory>]
  [<InlineData("Day14/sample.txt", 24)>]
  [<InlineData("Day14/input.txt", -1)>]
  member _.``Find amount of sand before it flows into the abyss below``(fileName: string, expected: int) =
    let cave = CaveScan.parse fileName |> Cave.fromScan
    output.WriteLine("Initial Cave:")
    output.WriteLine(Cave.render cave + "\n")
    let filledCave = Cave.simulateUntilFull cave
    output.WriteLine("Full Cave:")
    output.WriteLine(Cave.render filledCave + "\n")
    Assert.Equal(expected, filledCave.sand)

  [<Theory>]
  [<InlineData("Day14/sample.txt", -1)>]
  [<InlineData("Day14/input.txt", -1)>]
  member _.``Part 2``(fileName: string, expected: int) =
    let cave = CaveScan.parse fileName |> Cave.fromScan
    let result = -1

    Assert.Equal(expected, result)
