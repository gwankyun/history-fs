namespace Extend
open System.Text.Json
open System.IO

module Extend =
    module Result =
        let get (result: Result<'a, 'b>) =
            match result with
            | Ok value -> value
            | Error e -> raise e

        let tryReadAllText (path: string) =
            try
                Ok (File.ReadAllText(path))
            with
                | e -> Error e

        let toOption (result: Result<'a, 'b>) =
            match result with
            | Ok value -> Some value
            | Error _ -> None

        module Path =
            let tryCombine (path1: string) (path2: string) =
                try
                    Ok (Path.Combine(path1, path2))
                with
                    | e -> Error e

    module Directory =
        let exists (path: string) =
            Directory.Exists(path)

module Option =
    //let tryReadAllText (path: string) =
    //    (Extend.Result.tryReadAllText path) |> Extend.Result.toOption

    let ofTry (f: unit -> 'a) : 'a option =
        try
            Some (f())
        with
            | _ -> None

    let ofTryApply (f: 'a -> 'b) (x: 'a) : 'b option =
        try
            Some (f(x))
        with
            | _ -> None
    
    let tryWriteAllText (path: string) (contents: string) =
        try
            Some (File.WriteAllText(path, contents))
        with
            | _ -> None

    // let tryDeserialize (json: string) : 'a option =
    //     ofTry (fun () -> JsonSerializer.Deserialize<'a>(json))
