module Tests.Day2

open Xunit
open Day2

[<Theory>]
[<InlineData("Day2/testInput.txt", 15)>]
[<InlineData("Day2/input.txt", 14264)>]
let ``Total score for mistaken guide interpretation`` (fileName: string, expected: int) =
  let result = parseMistakenGuide fileName |> MistakenGuide.calculateScore
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day2/testInput.txt", 12)>]
[<InlineData("Day2/input.txt", 12382)>]
let ``Total score for proper guide interpretation`` (fileName: string, expected: int) =
  let result = parseProperGuide fileName |> ProperGuide.calculateScore
  Assert.Equal(expected, result)
