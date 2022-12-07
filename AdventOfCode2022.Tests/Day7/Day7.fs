module Tests.Day7

open Xunit
open Day7

[<Theory>]
[<InlineData("Day7/sample.txt", 95437)>]
[<InlineData("Day7/input.txt", -1)>]
let ``Sum of directories with total size less than 100000`` (fileName: string, expected: int) =
  let result =
    Shell.parseHistory fileName
    |> Files.fromShellHistory
    |> Files.getDirectories
    |> Seq.map Files.calculateSize
    |> Seq.filter (fun size -> size < 100000)
    |> Seq.sum
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day7/sample.txt", -1)>]
[<InlineData("Day7/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  // let result =
  //   parseSuppliesAndProcedures fileName
  //   ||> Supplies.rearrange Step.performWithCrateMover9000
  //   |> Supplies.topCratesPerStack
  let result = -1
  Assert.Equal(expected, result)


[<Fact>]
let ``calculateSize on file returns size`` () =
  let file = Files.File(name = "test", size = 1099)
  let size = Files.calculateSize file
  Assert.Equal(1099, size)

[<Fact>]
let ``calculateSize on directory recursively calculates size`` () =
  let file1 = Files.File(name = "f1", size = 1)
  let file2 = Files.File(name = "f2", size = 2)
  let file3 = Files.File(name = "f3", size = 3)
  let dir = Files.Directory(name = "d1", files = [file1; file2; file3])
  let root = Files.Directory(name = "/", files = [dir; file3])

  let size = Files.calculateSize root
  Assert.Equal(9, size)

[<Fact>]
let ``calculateSize on entire filesystem`` () =
  let fileSystem =
    Directory(name = "/", files = [
      Directory(name = "a", files = [
        Directory(name = "e", files = [
          File(name= "i", size=584)
        ])
        File(name= "f", size=29116)
        File(name= "g", size=2557)
        File(name= "h.lst", size=62596)
      ])
      File(name= "b.txt", size=14848514)
      File(name= "c.dat", size=8504156)
      Directory(name = "d", files = [
        File(name= "j", size=4060174)
        File(name= "d.log", size=8033020)
        File(name= "d.ext", size=5626152)
        File(name= "k", size=7214296)
      ])
    ])

  let size = Files.calculateSize fileSystem
  Assert.Equal(48381165, size)
