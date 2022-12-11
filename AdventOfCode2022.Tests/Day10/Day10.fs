module Tests.Day10

open Xunit
open Xunit.Abstractions
open Day10

type Day10(output: ITestOutputHelper) =

  [<Theory>]
  [<InlineData("Day10/sample.txt", 13140)>]
  [<InlineData("Day10/input.txt", 12560)>]
  member _.``Calculate the sum of the 6 signal strengths``(fileName: string, expected: int) =
    let program = Program.parse fileName
    let mutable device = Device.init ()

    let result =
      seq {
        20
        60
        100
        140
        180
        220
      }
      |> Seq.mapFold
           (fun d cycle ->
             let newDevice = Device.runUntil cycle program d
             Device.signalStrength newDevice, newDevice)
           device
      |> fst
      |> Seq.sum

    Assert.Equal(expected, result)

  [<Theory>]
  [<InlineData("Day10/sample.txt",
               "##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######.....")>]
  [<InlineData("Day10/input.txt",
               "###..#....###...##..####.###...##..#....
#..#.#....#..#.#..#.#....#..#.#..#.#....
#..#.#....#..#.#..#.###..###..#....#....
###..#....###..####.#....#..#.#....#....
#....#....#....#..#.#....#..#.#..#.#....
#....####.#....#..#.#....###...##..####.")>]
  member _.``Sprite Rendering``(fileName: string, expected: string) =
    let program = Program.parse fileName
    let result = Device.runUntil 240 program (Device.init ()) |> Device.renderPixels

    output.WriteLine("Pixel output:")
    output.WriteLine(result)

    Assert.Equal(expected, result)
