module Tests.Day12

open Day12
open Xunit

[<Theory>]
[<InlineData("Day12/sample.txt", 31)>]
[<InlineData("Day12/input.txt", -1)>]
let ``Fewest steps required to move from current position to best signal`` (fileName: string, expected: int) =
  let result = HeightMap.parse fileName |> HeightMap.shortestStepsToGoal

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day12/sample.txt", -1)>]
[<InlineData("Day12/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  let result = -1

  Assert.Equal(expected, result)

[<Fact>]
let ``Test the base cases for BFS`` () =
  let path =
    [| [| Start; Height 1 |]; [| Height 1; BestSignal |] |]
    |> HeightMap.pathToGoal
    |> Seq.toList

  Assert.True([ (0, 0); (1, 0); (1, 1) ] = path)
