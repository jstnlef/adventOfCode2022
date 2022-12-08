module Day7

type Command =
  | ChangeDirectory of name: string
  | ListDirectories

type FileEntry =
  | Directory of name: string
  | File of name: string * size: int

type ShellEntry =
  | Command of Command
  | FileEntry of FileEntry

type ShellHistory = ShellEntry seq

module Shell =
  open System.IO
  open System.Text.RegularExpressions

  let private commandRegex = Regex("\$ ([a-z]+) *([\w.\.\/]*)")
  let private directoryRegex = Regex("dir ([\w.\.\/]*)")
  let private fileRegex = Regex("(\d+) ([\w.\.\/]*)")

  let private parseLine line: ShellEntry =
    let commandMatch = commandRegex.Match line
    let dirMatch = directoryRegex.Match line
    let fileMatch = fileRegex.Match line
    if commandMatch.Success then
      if commandMatch.Groups[1].Value = "cd" then
        Command(ChangeDirectory(commandMatch.Groups[2].Value))
      else
        Command(ListDirectories)
    else if dirMatch.Success then
      FileEntry(FileEntry.Directory(dirMatch.Groups[1].Value))
    else
      FileEntry(FileEntry.File(fileMatch.Groups[2].Value, int fileMatch.Groups[1].Value))

  let parseHistory filename: ShellHistory =
    File.ReadLines filename
    |> Seq.map parseLine


type FileTree =
  | Directory of name: string * files: FileTree list
  | File of name: string * size: int

module FileSystem =
  open System.Collections.Generic

  let rec iter (file: FileTree): FileTree seq =
    seq {
      match file with
      | Directory(_, children) as d ->
        yield d
        for f in children do
          for child in iter f do
            yield child
      | File _ as f ->
        yield f
    }

  let rec getDirectories (file: FileTree): FileTree seq =
    let isDirectory file =
      match file with Directory _ -> true | _ -> false

    file
    |> iter
    |> Seq.filter isDirectory

  let size (file: FileTree): int =
    let sizeOfFile file =
      match file with
      | Directory _ -> 0
      | File(size = size) -> size

    file
    |> iter
    |> Seq.map sizeOfFile
    |> Seq.sum

  type private DiscoverState = {
    fileTree: FileTree
    currentDir: string
    parentDir: Stack<string>
  }

  let private entryToTree (entry: FileEntry): FileTree =
    match entry with
    | FileEntry.Directory name -> Directory(name, [])
    | FileEntry.File(name, size) -> File(name, size)

  let findDirectoryByName (name: string) (fileTree: FileTree): bool =
    match fileTree with
    | FileTree.Directory(dName, _) when dName = name -> true
    | _ -> false

  let private processCommand (state: DiscoverState) (command: Command): DiscoverState =
    match command with
    | Command.ChangeDirectory name when name = ".." ->
      { state with currentDir = state.parentDir.Pop() }
    | Command.ChangeDirectory name ->
      state.parentDir.Push(state.currentDir)
      { state with currentDir = name }
    | Command.ListDirectories -> state

  let private addFileEntry (entry: FileEntry) (state: DiscoverState): FileTree =
    let rec addFileEntryToDirectory (currentDir: string) (newTree: FileTree) (currentTree: FileTree): FileTree =
      match currentTree with
      | FileTree.Directory(name, files) when currentDir = name ->
        FileTree.Directory(name, newTree::files)
      | FileTree.Directory(name, files) ->
        FileTree.Directory(name, files |> List.map (addFileEntryToDirectory currentDir newTree))
      | _ -> currentTree

    addFileEntryToDirectory state.currentDir (entryToTree entry) state.fileTree

  let private discover state (entry: ShellEntry): DiscoverState =
    match entry with
    | FileEntry entry ->
      {state with fileTree = addFileEntry entry state }
    | Command c ->
      processCommand state c

  let fromShellHistory (history: ShellHistory): FileTree =
    let undiscoveredTree = FileTree.Directory(name = "/", files = [])
    let state = {
      fileTree = undiscoveredTree
      currentDir = "/"
      parentDir = Stack()
    }
    let state = Seq.fold discover state (history |> Seq.tail)
    state.fileTree
