module Tests.Day13

open Day13
open Xunit

[<Theory>]
[<InlineData("Day13/sample.txt", 13)>]
[<InlineData("Day13/input.txt", -1)>]
let ``Sum of the indices of the correct pairs`` (fileName: string, expected: int) =
  let result =
    DistressSignal.parse fileName |> DistressSignal.findCorrectPairs |> Seq.sum

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day13/sample.txt", -1)>]
[<InlineData("Day13/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  let result = -1

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData(0, true)>]
[<InlineData(1, true)>]
[<InlineData(2, false)>]
[<InlineData(3, true)>]
[<InlineData(4, false)>]
[<InlineData(5, true)>]
[<InlineData(6, false)>]
[<InlineData(7, false)>]
let ``Test in correct order`` (index: int, expected: bool) =
  let tests =
    [| List([| Integer(1); Integer(1); Integer(3); Integer(1); Integer(1) |]),
       List([| Integer(1); Integer(1); Integer(5); Integer(1); Integer(1) |])

       List([| List([| Integer(1) |]); List([| Integer(2); Integer(3); Integer(4) |]) |]),
       List([| List([| Integer(1) |]); Integer(4) |])

       List([| Integer(9) |]), List([| List([| Integer(8); Integer(7); Integer(6) |]) |])

       List([| List([| Integer(4); Integer(4) |]); Integer(4); Integer(4) |]),
       List([| List([| Integer(4); Integer(4) |]); Integer(4); Integer(4); Integer(4) |])

       List([| Integer(7); Integer(7); Integer(7); Integer(7) |]), List([| Integer(7); Integer(7); Integer(7) |])

       List([||]), List([| Integer(3) |])

       List([| List([| List [||] |]) |]), List([| List([||]) |])

       List(
         [| Integer(1)
            List(
              [| Integer(2)
                 List(
                   [| Integer(3)
                      List([| Integer(4); List([| Integer(5); Integer(6); Integer(7) |]) |]) |]
                 ) |]
            )
            Integer(8)
            Integer(9) |]
       ),
       List(
         [| Integer(1)
            List(
              [| Integer(2)
                 List(
                   [| Integer(3)
                      List([| Integer(4); List([| Integer(5); Integer(6); Integer(0) |]) |]) |]
                 ) |]
            )
            Integer(8)
            Integer(9) |]
       ) |]

  let isCorrect = tests[index] |> PacketPair.isInCorrectOrder

  Assert.Equal(expected, isCorrect)
