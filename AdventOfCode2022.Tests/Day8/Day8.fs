module Tests.Day8

open Xunit
open Day8

[<Theory>]
[<InlineData("Day8/sample.txt", -1)>]
[<InlineData("Day8/input.txt", -1)>]
let ``Part 1`` (fileName: string, expected: int) =
  // let result =
  //   Shell.parseHistory fileName
  //   |> FileSystem.fromShellHistory
  //   |> FileSystem.getDirectories
  //   |> Seq.map FileSystem.size
  //   |> Seq.filter (fun size -> size < 100000)
  //   |> Seq.sum
  let result = -1
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day8/sample.txt", -1)>]
[<InlineData("Day8/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  // let result =
  //   parseSuppliesAndProcedures fileName
  //   ||> Supplies.rearrange Step.performWithCrateMover9000
  //   |> Supplies.topCratesPerStack
  let result = -1
  Assert.Equal(expected, result)
