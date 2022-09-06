namespace Extend
open System.IO

module FileUtility = 
    type MaybeBuilder() =
        member this.Bind(x, f) =
            match x with
            | None -> None
            | Some a -> f a
    
        member this.Return(x) =
            Some x

    let maybe = new MaybeBuilder()

    let createDirectoryFor (path: string) =
        Result.ofTry (fun () ->
            let dir = FileInfo(path).Directory
            if dir <> null && (not dir.Exists) then
                dir.Create()
            dir)

    let copyAll (src: string) (dest: string) (pred: string -> string -> bool) =
        let s = Path.TrimEndingDirectorySeparator(src)
        let d = Path.TrimEndingDirectorySeparator(dest)
        let createDirectory (path: string) =
            if Directory.Exists(path) |> not then
                Directory.CreateDirectory(path) |> ignore
        let copy s d =
            createDirectoryFor d |> ignore
            File.Copy(s, d, true)
        let getFileSystemEntries src =
            Directory.GetFileSystemEntries(src, "*", SearchOption.AllDirectories)//)
        (fun () ->
            getFileSystemEntries src
            |> Array.map (fun x ->
                let relaPath = Path.GetRelativePath(s, x)
                let newPath = Path.Join(d, relaPath)
                x, newPath)
            |> Array.iter (fun (s, d) ->
                if pred s d then
                    if Directory.Exists(s) then // 文件夾
                        createDirectory d |> ignore
                    else // 文件
                        copy s d)
        ) |> Result.ofTry

module FileUtils =
    let getDirectories (info: DirectoryInfo) =
        try
            info.GetDirectories()
        with
            | _ -> Array.empty

    let getFiles (info: DirectoryInfo) =
        try
            info.GetFiles()
        with
            | _ -> Array.empty

    let getAllFiles (path: string) =
        let rec f (s: DirectoryInfo list) (r: FileInfo list) =
            match s with
            | h::t ->
                let dirs = h |> getDirectories |> List.ofArray
                let files = h |> getFiles |> List.ofArray
                let s = t |> List.append dirs
                let r = r |> List.append files
                f s r
            | [] -> r
        f [new DirectoryInfo(path)] []

    let getAllDirectories (path: string) =
        let rec f (s: DirectoryInfo list) (r: DirectoryInfo list) =
            match s with
            | h::t ->
                let dirs = h |> getDirectories |> List.ofArray
                let s = t |> List.append dirs
                let r = r |> List.append dirs
                f s r
            | [] -> r
        f [new DirectoryInfo(path)] []
