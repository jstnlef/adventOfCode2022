module Day7

type Command =
  | ChangeDirectory of name: string
  | ListDirectories

type ShellEntry =
  | Command of Command
  | Directory of name: string
  | File of name: string * size: int

type ShellHistory = ShellEntry seq

module Shell =
  open System.IO
  open System.Text.RegularExpressions

  let private commandRegex =
    Regex("\$ ([a-z]+) *(.+)")

  let private directoryRegex =
    Regex("dir (.+)")

  let private fileRegex = Regex("(\d+) (.+)")

  let private parseLine line : ShellEntry =
    let commandMatch = commandRegex.Match line
    let dirMatch = directoryRegex.Match line
    let fileMatch = fileRegex.Match line

    if commandMatch.Success then
      if commandMatch.Groups[1].Value = "cd" then
        Command(ChangeDirectory(commandMatch.Groups[2].Value))
      else
        Command(ListDirectories)
    else if dirMatch.Success then
      ShellEntry.Directory(dirMatch.Groups[1].Value)
    else
      ShellEntry.File(fileMatch.Groups[2].Value, int fileMatch.Groups[1].Value)

  let parseHistory filename : ShellHistory =
    File.ReadLines filename |> Seq.map parseLine


type File =
  { path: string
    name: string
    size: int }

type Directory = { name: string; files: File list }

type FileSystem =
  { directories: string list
    files: File list }

let pathSeparator = "->"

module FileSystem =
  let calculateDirectorySizes (fileSystem: FileSystem) =
    fileSystem.directories
    |> Seq.map (fun d ->
      fileSystem.files
      |> Seq.filter (fun { path = path } -> path.StartsWith d)
      |> Seq.map (fun f -> f.size))
    |> Seq.map Seq.sum

  type private DiscoverState =
    { path: string list
      files: File list
      directories: string list }

  let private generatePath pathParts =
    pathParts
    |> List.rev
    |> String.concat pathSeparator

  let private processCommand state command : DiscoverState =
    match command with
    | Command.ChangeDirectory name when name = ".." -> { state with DiscoverState.path = List.removeAt 0 state.path }
    | Command.ChangeDirectory name -> { state with DiscoverState.path = name :: state.path }
    | Command.ListDirectories -> state

  let private discoverFileSystem state shellEntry =
    match shellEntry with
    | ShellEntry.File (name, size) ->
      let f =
        { path = state.path |> generatePath
          name = name
          size = size }

      { state with files = f :: state.files }
    | ShellEntry.Directory name ->
      { state with
          directories =
            generatePath (name :: state.path)
            :: state.directories }
    | ShellEntry.Command c -> processCommand state c

  let fromShellHistory (history: ShellHistory) : FileSystem =
    let state =
      { path = []
        files = []
        directories = [ "/" ] }

    history
    |> Seq.fold discoverFileSystem state
    |> fun processed ->
         { directories = processed.directories
           files = processed.files }
