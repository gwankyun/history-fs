module Common
open System.IO

module FileEntry =
    type T =
        | Dir of string
        | File of string
        member this.Value =
            match this with
            | Dir d -> d
            | File f -> f

    let create path =
        match Directory.Exists(path) with
        | true -> Dir path
        | false -> File path

    let isDir (entry: T) : bool =
        match entry with
        | Dir _ -> true
        | _ -> false

type Info = {
    Dir: Map<string, int64>
    File: Map<string, int64>
}

type Diff = {
    Update: Info
    Delete: Info
}

let join (a: string) (b: string) = Path.Join(a, b)
let join3 (a: string) (b: string) (c: string) = Path.Join(a, b, c)
let join4 (a: string) (b: string) (c: string) (d: string) = Path.Join(a, b, c, d)

let exists (fileEntry: FileEntry.T) =
    match fileEntry with
    | FileEntry.Dir dir -> Directory.Exists(dir)
    | FileEntry.File file -> File.Exists(file)

let delete (fileEntry: FileEntry.T) =
    if fileEntry |> exists then
        match fileEntry with
        | FileEntry.Dir dir ->
            Directory.Delete(dir, true)
        | FileEntry.File file ->
            File.Delete(file)

let createDirectory (fileEntry: FileEntry.T) =
    match fileEntry with
    | FileEntry.Dir dir ->
        if fileEntry |> exists |> not then
            Directory.CreateDirectory(dir) |> ignore
    | FileEntry.File file -> failwith "not dir"

let createDirectoryFor (path: string) =
    let dir = (new FileInfo(path)).Directory
    if dir <> null && (not dir.Exists) then
        dir.Create()

let getRelativePath (path: string) (sub: string) =
    Path.GetRelativePath(path, sub)

let contains (sub: string) (str: string) =
    str.Contains(sub)

let getFileSystemEntries (path: string) =
    Directory.GetFileSystemEntries(path, "*", SearchOption.AllDirectories)
    |> Array.filter (contains ".history" >> not)

let copyAll (source: string) (dest: string) =
    let items =
        getFileSystemEntries source
        |> Array.map (fun x -> x |> FileEntry.create, getRelativePath source x)
    items
    |> Array.iter (fun (a, b) ->
        let path = join dest b
        match a with
        | FileEntry.Dir _ ->
            FileEntry.Dir path |> createDirectory
        | FileEntry.File file ->
            createDirectoryFor path
            File.Copy(file, path, true))

let difference (f: 'k -> 'v -> 'v -> bool) (a: Map<'k, 'v>) (b: Map<'k, 'v>) =
    a |> Map.filter (fun k v ->
        match b |> Map.tryFind k with
        | Some x -> f k v x
        | None -> true)
