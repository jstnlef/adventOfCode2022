module Tests.Day5

open Xunit
open Day5

[<Theory>]
[<InlineData("Day5/testInput.txt", "CMZ")>]
[<InlineData("Day5/input.txt", "TLNGFGMFN")>]
let ``Top crates after rearrangement with CrateMover 9000`` (fileName: string, expected: string) =
  let result =
    parseSuppliesAndProcedures fileName
    ||> Supplies.rearrange Step.performWithCrateMover9000
    |> Supplies.topCratesPerStack
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day5/testInput.txt", "MCD")>]
[<InlineData("Day5/input.txt", "FGLQJCMBD")>]
let ``Top crates after rearrangement with CrateMover 9001`` (fileName: string, expected: string) =
  let result =
    parseSuppliesAndProcedures fileName
    ||> Supplies.rearrange Step.performWithCrateMover9001
    |> Supplies.topCratesPerStack
  Assert.Equal(expected, result)
