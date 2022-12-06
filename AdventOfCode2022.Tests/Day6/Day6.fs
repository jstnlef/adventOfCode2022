module Tests.Day6

open Xunit
open Day6

[<Theory>]
[<InlineData("Day6/sample1.txt", 7)>]
[<InlineData("Day6/sample2.txt", 5)>]
[<InlineData("Day6/sample3.txt", 6)>]
[<InlineData("Day6/sample4.txt", 10)>]
[<InlineData("Day6/sample5.txt", 11)>]
[<InlineData("Day6/input.txt", 1987)>]
let ``Find start of packet marker in data stream`` (fileName: string, expected: int) =
  let result =
    parseDataStream fileName
    |> findStartOfPacketMarker
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day6/testInput.txt", "")>]
[<InlineData("Day6/input.txt", "")>]
let ``2`` (fileName: string, expected: string) =
  let result = ""
  Assert.Equal(expected, result)
