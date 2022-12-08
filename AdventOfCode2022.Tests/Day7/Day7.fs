module Tests.Day7

open Xunit
open Day7

[<Theory>]
[<InlineData("Day7/sample.txt", 95437)>]
[<InlineData("Day7/input.txt", 1642503)>]
let ``Sum of directories with total size less than 100000`` (fileName: string, expected: int) =
  let result =
    Shell.parseHistory fileName
    |> FileSystem.fromShellHistory
    |> FileSystem.getDirectorySizes
    |> Seq.filter (fun size -> size <= 100000)
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
let ``parsing input creates ShellHistory`` () =
  let expected: ShellHistory =
    [ Command(ChangeDirectory("/"))
      Command(ListDirectories)
      ShellEntry.Directory("a")
      ShellEntry.File("b.txt", 14848514)
      ShellEntry.File("c.dat", 8504156)
      ShellEntry.Directory("d")
      Command(ChangeDirectory("a"))
      Command(ListDirectories)
      ShellEntry.Directory("e")
      ShellEntry.File("f", 29116)
      ShellEntry.File("g", 2557)
      ShellEntry.File("h.lst", 62596)
      Command(ChangeDirectory("e"))
      Command(ListDirectories)
      ShellEntry.File("i", 584)
      Command(ChangeDirectory(".."))
      Command(ChangeDirectory(".."))
      Command(ChangeDirectory("d"))
      Command(ListDirectories)
      ShellEntry.File("j", 4060174)
      ShellEntry.File("d.log", 8033020)
      ShellEntry.File("d.ext", 5626152)
      ShellEntry.File("k", 7214296) ]

  let result =
    Shell.parseHistory "Day7/sample.txt" |> Seq.toList

  Assert.True((expected = result))

// [<Fact>]
// let ``calculateSize on file returns size`` () =
//   let file =
//     FileTree.File
//       { name = "test"
//         size = 1099
//         parent = ref None }
//
//   let size = FileSystem.size file
//   Assert.Equal(1099, size)
//
// [<Fact>]
// let ``calculateSize on directory recursively calculates size`` () =
//   let file1 =
//     File
//       { name = "f1"
//         size = 1
//         parent = ref None }
//
//   let file2 =
//     File
//       { name = "f2"
//         size = 2
//         parent = ref None }
//
//   let file3 =
//     File
//       { name = "f3"
//         size = 3
//         parent = ref None }
//
//   let dir =
//     Directory
//       { name = "d1"
//         files = [ file1; file2; file3 ]
//         parent = ref None }
//
//   let root =
//     Directory
//       { name = "/"
//         files = [ dir; file3 ]
//         parent = ref None }
//
//   dir |> FileTree.updateWithParent (Some root)
//   let p = dir |> FileTree.getParent
//
//   let size = FileSystem.size root
//   Assert.Equal(9, size)

// [<Fact>]
// let ``calculateSize on entire filesystem`` () =
//   let fileSystem =
//     FileTree.Directory
//       { name = "/"
//         files = [ FileTree.Directory(
//           name = "a",
//           files =
//             [ FileTree.Directory(name = "e", files = [ FileTree.File(name = "i", size = 584) ])
//               FileTree.File(name = "f", size = 29116)
//               FileTree.File(name = "g", size = 2557)
//               FileTree.File(name = "h.lst", size = 62596) ]
//         )
//         FileTree.File(name = "b.txt", size = 14848514)
//         FileTree.File(name = "c.dat", size = 8504156)
//         FileTree.Directory(
//           name = "d",
//           files =
//             [ FileTree.File(name = "j", size = 4060174)
//               FileTree.File(name = "d.log", size = 8033020)
//               FileTree.File(name = "d.ext", size = 5626152)
//               FileTree.File(name = "k", size = 7214296) ]
//         ) ]
//     }
//
//   let size = FileSystem.size fileSystem
//   Assert.Equal(48381165, size)
