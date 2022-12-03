module Tests.Day3

open Xunit
open Day3

[<Theory>]
[<InlineData("Day3/testInput.txt", 157)>]
[<InlineData("Day3/input.txt", 8018)>]
let ``Sum of the shared rucksack priorities`` (fileName: string, expected: int) =
  let result = parseRucksacks fileName |> sumOfRucksackPriorities
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day3/testInput.txt", 70)>]
[<InlineData("Day3/input.txt", 2518)>]
let ``Sum of the 3 elf group badge priorities`` (fileName: string, expected: int) =
  let result = parseRucksacks fileName |> sumOf3ElfBadgePriorities
  Assert.Equal(expected, result)
