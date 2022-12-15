module Tests.Day14

open Day14
open Xunit
open Xunit.Abstractions

type Day14(output: ITestOutputHelper) =
  [<Theory>]
  [<InlineData("Day14/sample.txt", 24)>]
  [<InlineData("Day14/input.txt", 888)>]
  member _.``Find amount of sand before it flows into the abyss below``(fileName: string, expected: int) =
    let cave = CaveScan.parse fileName |> Cave.fromScan
    let filledCave = Cave.simulateUntilFull cave
    output.WriteLine("Simulated Cave:")
    output.WriteLine(Cave.render filledCave + "\n")
    Assert.Equal(expected, filledCave.sand)

  [<Theory>]
  [<InlineData("Day14/sample.txt", 93)>]
  [<InlineData("Day14/input.txt", 26461)>]
  member _.``Find amount of sand before it covers the origin``(fileName: string, expected: int) =
    let cave = CaveScan.parse fileName |> Cave.fromScanWithFloor
    let filledCave = Cave.simulateUntilFull cave
    output.WriteLine("Simulated Cave:")
    output.WriteLine(Cave.render filledCave + "\n")
    Assert.Equal(expected, filledCave.sand)
