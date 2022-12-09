module Tests.Day7

open Xunit
open Day7

[<Theory>]
[<InlineData("Day7/sample.txt", 4, 95437)>]
[<InlineData("Day7/input.txt", 200, 1642503)>]
let ``Sum of directories with total size less than 100000`` (fileName: string, totalDirectories: int, expected: int) =
  let fileSystem =
    Shell.parseHistory fileName
    |> FileSystem.fromShellHistory

  Assert.Equal(totalDirectories, fileSystem.directories.Length)

  let result =
    fileSystem
    |> FileSystem.calculateDirectorySizes
    |> Seq.filter (fun size -> size <= 100000)
    |> Seq.sum

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day7/sample.txt", 24933642)>]
[<InlineData("Day7/input.txt", 6999588)>]
let ``Total size of the directory to remove which frees up enough space`` (fileName: string, expected: int) =
  let sizes =
    Shell.parseHistory fileName
    |> FileSystem.fromShellHistory
    |> FileSystem.calculateDirectorySizes

  let unusedSpace = 70000000 - Seq.max sizes

  let result =
    sizes
    |> Seq.filter (fun size -> unusedSpace + size >= 30000000)
    |> Seq.min

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
