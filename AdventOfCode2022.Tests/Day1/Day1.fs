module Tests.Day1

open Xunit
open Day1

[<Theory>]
[<InlineData("Day1/testInput.txt", 24000)>]
[<InlineData("Day1/input.txt", 66487)>]
let ``Elf with most caloric food`` (fileName: string, expected: int) =
  let result = parseElvesWithFood fileName |> elfWithMostCaloricFood
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day1/testInput.txt", 45000)>]
[<InlineData("Day1/input.txt", 197301)>]
let ``Top 3 elves with most caloric food`` (fileName: string, expected: int) =
  let result = parseElvesWithFood fileName |> top3withMostCaloricFood
  Assert.Equal(expected, result)
