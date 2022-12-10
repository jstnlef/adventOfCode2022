module Tests.Day10

open Xunit
open Day10

[<Theory>]
[<InlineData("Day10/sample.txt", 13140)>]
[<InlineData("Day10/input.txt", 12560)>]
let ``Calculate the sum of the 6 signal strengths`` (fileName: string, expected: int) =
  let program = Program.parse fileName

  let result =
    seq {
      let mutable device = Device.init

      let interestingCycles =
        seq {
          20
          60
          100
          140
          180
          220
        }

      for cycle in interestingCycles do
        device <- Device.runUntil cycle device program
        yield device
    }
    |> Seq.map Device.signalStrength
    |> Seq.sum

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day10/sample.txt", 1)>]
[<InlineData("Day10/input.txt", 2303)>]
let ``Part 2`` (fileName: string, expected: int) =
  // let result =
  //   RopeSim.parse fileName
  //   |> RopeSim.simulate 10
  //   |> (fun state -> state.positionsTailVisited.Count)

  let result = -1
  Assert.Equal(expected, result)
