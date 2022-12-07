module Day7

type Files =
  | Directory of name: string * files: Files list
  | File of name: string * size: int

type Command =
  | ChangeDirectory of string
  | ListDirectories

type Entry =
  | Command
  // | File of File

type ShellHistory = Entry seq

module Files =
  let fromShellHistory history: Files =
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

  let rec getDirectories (files: Files): Files list =
    let mutable dirs = []
    match files with
    | Directory(name, files) ->
      if name <> "/" then
        let d = Directory(name, files)
        dirs <- d :: dirs

      dirs <- dirs @ (List.collect getDirectories files)
    | _ -> ()
    dirs

  let rec calculateSize (files: Files): int =
    match files with
    | Directory(files = files) -> files |> Seq.map calculateSize |> Seq.sum
    | File(size = size) -> size

module Shell =
  let parseHistory filename: ShellHistory =
    Seq.empty
