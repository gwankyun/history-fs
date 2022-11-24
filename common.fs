module Common
open System.IO

module Entry =
    type T =
        | Dir of string
        | File of string
        | Inexistence // 不存在
        member this.Value =
            match this with
            | Dir d -> d
            | File f -> f
            | Inexistence -> ""

    let create path =
        match (Directory.Exists(path), File.Exists(path)) with
        | true, _ -> Dir path
        | _, true -> File path
        | _ -> Inexistence

    let iter f e =
        match e with
        | Dir dir -> f dir
        | File file -> f file
        | Inexistence -> ()

    let isDir (entry: T) : bool =
        match entry with
        | Dir _ -> true
        | _ -> false

    let deleteWith f e =
        match e with
        | Dir d -> Directory.Delete(d, true)
        | File f -> File.Delete(f)
        | Inexistence -> f()

    let delete e =
        e
        |> deleteWith (fun () -> failwith "Inexistence")

    let createDirectory path =
        try
            Directory.CreateDirectory(path) |> ignore
            Dir path
        with _ -> Inexistence

type Info = {
    Dir: Map<string, int64>
    File: Map<string, int64>
}

type Diff = {
    Update: Info
    Delete: Info
}

module DirType =
    type T = 
        | DirType of string
        member this.Value =
            match this with
            | DirType path -> path

    let createWith s f path =
        match Directory.Exists(path) with
        | true -> s (DirType path)
        | false -> f path

    let create path =
        let s e = Some e
        let f _ = None
        createWith s f path

    let apply f (DirType path) = f path

    let value e = apply id e

let join (a: string) (b: string) = Path.Join(a, b)
let join3 (a: string) (b: string) (c: string) = Path.Join(a, b, c)
let join4 (a: string) (b: string) (c: string) (d: string) = Path.Join(a, b, c, d)

let exists (fileEntry: Entry.T) =
    match fileEntry with
    | Entry.Dir dir -> Directory.Exists(dir)
    | Entry.File file -> File.Exists(file)
    | Entry.Inexistence -> false

let delete (fileEntry: Entry.T) =
    if fileEntry |> exists then
        match fileEntry with
        | Entry.Dir dir ->
            Directory.Delete(dir, true)
        | Entry.File file ->
            File.Delete(file)
        | Entry.Inexistence -> failwith "Inexistence"

let createDirectory (fileEntry: Entry.T) =
    match fileEntry with
    | Entry.Dir dir ->
        if fileEntry |> exists |> not then
            Directory.CreateDirectory(dir) |> ignore
    | Entry.File file -> failwith "not dir"
    | Entry.Inexistence -> failwith "Inexistence"

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
        |> Array.map (fun x -> x |> Entry.create, getRelativePath source x)
    items
    |> Array.iter (fun (a, b) ->
        let path = join dest b
        match a with
        | Entry.Dir _ ->
            Entry.Dir path |> createDirectory
        | Entry.File file ->
            createDirectoryFor path
            File.Copy(file, path, true)
        | Entry.Inexistence -> failwith "Inexistence")

let difference (f: 'k -> 'v -> 'v -> bool) (a: Map<'k, 'v>) (b: Map<'k, 'v>) =
    a |> Map.filter (fun k v ->
        match b |> Map.tryFind k with
        | Some x -> f k v x
        | None -> true)
