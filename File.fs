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
        Option.ofTry (fun () ->
            let dir = FileInfo(path).Directory
            if dir <> null && (not dir.Exists) then
                dir.Create()
            dir)

    let copyAll (src: string) (dest: string) (pred: string -> string -> bool) =
        let entries =
            let s = Path.TrimEndingDirectorySeparator(src)
            let d = Path.TrimEndingDirectorySeparator(dest)
            maybe {
                let r =
                    (fun () -> Directory.GetFileSystemEntries(src, "*", SearchOption.AllDirectories))
                    |> Option.ofTry
                let! result = Option.map (Array.map (fun x ->
                    let relaPath = Path.GetRelativePath(s, x)
                    let newPath = Path.Join(d, relaPath)
                    x, newPath)) r
                result |> Array.iter (fun (s, d) ->
                    if pred s d then
                        if Directory.Exists(s) then
                            if Directory.Exists(d) |> not then
                                Directory.CreateDirectory(d) |> ignore
                        else
                            createDirectoryFor d
                            |> Option.map (fun _ -> File.Copy(s, d, true))
                            |> ignore
                            )
                return result
            }
        // entries
            
        entries
        // |> Option.map (Array.iter (fun (s, d) ->
        //     if pred s d then
        //         if Directory.Exists(s) then
        //             if Directory.Exists(d) |> not then
        //                 Directory.CreateDirectory(d) |> ignore
        //         else
        //             createDirectoryFor d
        //             |> Option.map (fun _ -> File.Copy(s, d, true))
        //             |> ignore
        //             )) |> ignore

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
