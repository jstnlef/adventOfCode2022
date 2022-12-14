module Tests.Day13

open Day13
open Xunit

[<Theory>]
[<InlineData("Day13/sample.txt", 13)>]
[<InlineData("Day13/input.txt", 5717)>]
let ``Sum of the indices of the correct pairs`` (fileName: string, expected: int) =
  let result =
    DistressSignal.parse fileName |> DistressSignal.findCorrectPairs |> Seq.sum

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day13/sample.txt", 140)>]
[<InlineData("Day13/input.txt", 25935)>]
let ``Find the decoder key for the distress signal`` (fileName: string, expected: int) =
  let result = DistressSignal.parse fileName |> DistressSignal.findDecoderKey
  Assert.Equal(expected, result)

[<Fact>]
let ``Parse line of input`` () =
  Assert.Equal(List([| Integer(1) |]), Parsing.parsePacket "[1]")
  Assert.Equal(List([| Integer(10) |]), Parsing.parsePacket "[10]")
  Assert.Equal(List([||]), Parsing.parsePacket "[]")
  Assert.Equal(List([| List([||]) |]), Parsing.parsePacket "[[]]")

  Assert.Equal(
    List([| Integer(1); List([| Integer(2); Integer(3); Integer(4) |]) |]),
    Parsing.parsePacket "[1,[2,3,4]]"
  )

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
