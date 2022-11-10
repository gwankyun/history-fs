namespace History
open Extend
open System
open System.IO
open System.Text.Json

//module Result =
//    let ofTry (f: unit -> 'a) : Result<'a, exn> =
//        try
//            Ok (f())
//        with
//            | e -> Error e

type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a
  
    member this.Return(x) =
        Some x

//Result.

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module History =
    let maybe = new MaybeBuilder()

    type Info = {
        FullName: string
        LastWriteTime: int64
        IsDir: bool
    }

    type Config = {
        Debug: bool
    }

    // [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PathType =
        type T =
            | PathType of string
            member this.Value =
                match this with
                | PathType s -> s
        let create (s: string) = PathType s
        let value (PathType str) = str 

    let init (path: PathType.T) : PathType.T =
        printfn "path@init: %s" path.Value
        let history = Path.Join(path.Value, ".history")
        if Directory.Exists(history) |> not then
            Directory.CreateDirectory(history) |> ignore
        history |> PathType.create

    let rename (history: PathType.T) (o: string) (n: string) =
        let o = Path.Join(history.Value, o)
        let n = Path.Join(history.Value, n)
        if Directory.Exists(o) then
            (fun () ->
                Directory.Move(o, n)
                true)
            |> Result.ofTry
        else
            Ok false

    let add (config: Config) (name_: string) (path_: PathType.T) (history_: PathType.T) =
        // let name = name_.Value
        let path = path_.Value
        let history = history_.Value
        let namePath = Path.Join(history, name_)

        if config.Debug then
            printfn "name@add: %s" name_
            printfn "path@add: %s" path
            printfn "history@add: %s" history
            printfn "namePath@add: %s" namePath
        
        Directory.CreateDirectory(namePath) |> ignore

        let toFileTime (x: DateTime) = x.ToLocalTime().ToFileTime()
        let toInfo (isDir: bool) (file: FileSystemInfo) =
            {
                FullName = file.FullName;
                LastWriteTime = file.LastWriteTime |> toFileTime;
                IsDir = isDir
            }
        let files =
            FileUtils.getAllFiles(path)
            |> List.map (toInfo false)
        let dirs =
            FileUtils.getAllDirectories(path)
            |> List.map (toInfo true)
        let data =
            dirs
            |> List.append files
            |> List.map (fun x -> { x with FullName = x.FullName.Substring(path_.Value.Length) })
        let json = Path.Join(namePath, "data.json")
        printfn "namePath@json: %s" json
        data
        // |> List.filter (fun x -> x.FullName.StartsWith(".history") |> not)
        |> List.filter (fun x -> x.FullName.Contains(".history") |> not)
        |> (fun x ->
            let data = JsonSerializer.Serialize(x)
            File.WriteAllText(json, data)
        )

    let list (history: PathType.T) =
        let lst = Directory.GetDirectories(history.Value)
        for i in lst do
            let p = i.Substring(history.Value.Length + 1)
            printfn "%A" p

    let toJsonPath (history: string) (ver: string) =
        Path.Join(history, ver, "data.json")

    type Diff = Map<string, Info>
    type DiffResult = Diff * Diff * Diff

    let copy (config: Config) (result: DiffResult) (source_: PathType.T) (dist_: PathType.T) =
        let c, m, d = result
        if config.Debug then
            printfn "c: %A" c
        let partition (d: Diff) = d |> Map.partition (fun _ v -> v.IsDir)
        let copyFile (f: Diff) =
            f |> Map.iter (fun k _ ->
            let oldPath = Path.Join(source_.Value, k)
            let newPath = Path.Join(dist_.Value, k)
            FileUtility.createDirectoryFor newPath |> ignore
            if File.Exists(oldPath) then
                File.Copy(oldPath, newPath, true))
        let copyDir (d: Diff) =
            d 
            |> Map.iter (fun k _ ->
                let path = Path.Join(dist_.Value, k)
                if Directory.Exists(path) |> not then
                    Directory.CreateDirectory(path) |> ignore)

        //printfn "c: %A" c
        let cd, cf = c |> partition
        //printfn "cd: %A" cd
        //printfn "================11================"
        //printfn "cf: %A" cf
        cf |> copyFile // 新增的文件複製
        //printfn "================12================"
        cd |> copyDir // 新增的目錄複製


        let _, mf = m |> partition
        mf |> copyFile // 修改的文件複製

        //printfn "================13================"

        // 將其他信息寫入JSON
        // cd 創建的文件夾
        // d 刪除的文件的文件與文件夾
        let result =
            (fun () -> JsonSerializer.Serialize<Diff>(d))
            |> Option.ofTry
            |> Option.bind (
                Option.ofTryApply (fun (x) ->
                    let path = Path.Join(dist_.Value, ".history", "data.json")
                    FileUtility.createDirectoryFor path |> ignore
                    printfn "path: %A" path
                    File.WriteAllText(path, x)))
        result.IsSome

    let merge (config: Config) (src_: PathType.T) (dest_: PathType.T) =
        // 複製src所有非.history目錄下的文件到dest
        if config.Debug then
            printfn "@merge"
            printfn "$src_: %s" src_.Value
            printfn "$dest_: %s" dest_.Value
        let sc = Path.TrimEndingDirectorySeparator(src_.Value)
        let ds = Path.TrimEndingDirectorySeparator(dest_.Value)
        let history = Path.Join(sc, ".history")

        (fun () ->
            FileUtility.copyAll src_.Value dest_.Value (fun o _ -> o.StartsWith(history) |> not) |> ignore

            // 刪掉過時部分
            let json = Path.Join(history, "data.json")
            if File.Exists(json) then
                let text = File.ReadAllText(json)
                let data = JsonSerializer.Deserialize<Diff>(text)
                data
                |> Map.iter (fun k v ->
                    let path = Path.Join(ds, k)
                    if v.IsDir then
                        if Directory.Exists(path) then
                            Directory.Delete(path, true)
                    else
                        printfn "file: %A" path
                        if File.Exists(path) then
                            File.Delete(path)
                )
        ) |> Result.ofTry

    let result = new Result.Builder()

    let compare (config: Config) (path1_: PathType.T) (path2_: PathType.T) =
        let p1 = Path.TrimEndingDirectorySeparator(path1_.Value)
        let p2 = Path.TrimEndingDirectorySeparator(path2_.Value)
        match Directory.Exists(p1), Directory.Exists(p2) with
        | true, true ->
            let getRelativePath (path: string) =
                (fun () ->
                    Directory.GetFileSystemEntries(path, "*", SearchOption.AllDirectories)
                    |> Array.map(fun x ->
                        let file = new FileInfo(x)
                        x, file.LastWriteTime)
                    |> Array.map (fun (x, w) -> Path.GetRelativePath(path, x), w)
                    |> Array.filter (fun (x, _) -> x.StartsWith(".history") |> not)
                    |> Array.sortBy fst
                    ) |> Result.ofTry
            result {
                let! e1 = getRelativePath p1
                let! e2 = getRelativePath p2
                let diff a b = a |> Array.filter (fun x -> b |> Array.tryFind (fun y -> x = y) |> Option.isNone)
                let _ =
                    match config.Debug with
                    | true ->
                        printfn "diff: %A\n%A" (diff e1 e2) (diff e2 e1)
                        true
                    | false -> false
                return Array.compareWith (fun (a, aw) (b, bw) ->
                    let pa = Path.Join(p1, a)
                    let pb = Path.Join(p1, b)
                    match Directory.Exists(pa), Directory.Exists(pb) with
                    | true, true -> 0
                    | false, false ->
                        if a = b && aw = bw then
                            0
                        else
                            1
                    | _ -> 1) e1 e2
                |> ((=) 0)
            }
        | _ -> Ok false

    let diff (config: Config) (history_: PathType.T) (newPath_: PathType.T) (oldPath_: PathType.T) =
        let readFile (path: PathType.T) =
            path.Value |> toJsonPath history_.Value
            |> Option.ofTryApply (fun x -> File.ReadAllText(x))
        let newFile = newPath_ |> readFile
        let oldFile = oldPath_ |> readFile
        let textToMap (text: string option) =
            maybe {
                let! data =
                    text 
                    |> Option.map (fun x ->
                        JsonSerializer.Deserialize<Info list>(x))
                let data =
                    data
                    |> List.map (fun x -> x.FullName, x)
                    |> Map.ofList
                return data
            }
        maybe {
            let! newData = newFile |> textToMap
            let! oldData = oldFile |> textToMap
            let createFile = // 新有舊無
                Map.difference newData oldData
            let modifyFile = // 新舊皆有，但已修改
                let diffLastWriteTime _ (v1: Info) (v2: Info) =
                    v1.LastWriteTime <> v2.LastWriteTime
                Map.intersectWith diffLastWriteTime newData oldData
            let deleteFile = // 舊有新無
                Map.difference oldData newData
            return createFile, modifyFile, deleteFile
        }

    /// <summary>測試函數</summary>
    /// <param name="currentPath">項目的路徑</param>
    /// <returns>包括一個異常，可再次抛出</returns>
    let testAll (config: Config) (currentPath: string) =
        let compare = compare config
        let copy = copy config
        let diff = diff config
        let merge = merge config
        let add = add config
        let result = new Result.Builder()
        (fun () ->
            let deleteDirectory d =
                Result.ofTry (fun () ->
                    Directory.Delete(d, true)
                    true)

            let deleteDirectory (path: PathType.T) =
                let p = path |> PathType.value
                match Directory.Exists(p) with
                | true -> deleteDirectory p
                | false -> Ok false

            printfn "%A" currentPath
            let test1 = Path.Join(currentPath, "test/test1") |> PathType.create
            let test2 = Path.Join(currentPath, "test/test2") |> PathType.create

            let r =
                result {
                    let! r = deleteDirectory test1
                    let! r = deleteDirectory test2
                    let testPath = Path.Join(currentPath, "test/test")
                    let! r = FileUtility.copyAll testPath test1.Value (fun _ _ -> true)// |> ignore
                    let! r = FileUtility.copyAll testPath test2.Value (fun _ _ -> true)// |> ignore
                    let! r = compare test1 test2
                    return r
                }

            // 兩個目錄要相等
            //match compare test1 test2 with
            match r with
            | Ok true -> ()
            | Ok false ->
                    failwith "compare"
            | Error e -> raise e

        // printfn "================1================"

            let history = init test1// |> PathType.create

            let the1 = "1"// |> PathType.create

            // 添加當前記錄
            add the1 test1 history

            let createDirectory p =
                Result.ofTry (fun () -> Directory.CreateDirectory(p))

            let writeAllText p c =
                Result.ofTry (fun () -> File.WriteAllText(p, c))

            let deleteFile f =
                Result.ofTry (fun () -> File.Delete(f))

            let r = result {
                // 修改test1
                // 添加目录
                let! r = createDirectory (Path.Join(test1.Value, "4"))

                // 添加文件
                let! r = writeAllText (Path.Join(test1.Value, "2.txt")) "2"
                let! r = writeAllText (Path.Join(test1.Value, "4/4.txt")) "4"
                // printfn "================2================"

                // 修改文件
                let! r = writeAllText (Path.Join(test1.Value, "1.txt")) "1"

                // 刪除文件
                let! r = deleteFile (Path.Join(test1.Value, "3/4.txt"))
                return r
            }

            let the2 = "22"// |> PathType.create

            printfn "===test add begin"

            // 保存修改後的狀態
            add the2 test1 history
            list history

            printfn "===test add end"

            match rename history "22" "2" with
            | Error e -> raise e
            | Ok s -> printfn "%A" s

            let the2 = "2" |> PathType.create

            let diffResult = diff history the2 (the1 |> PathType.create)

            let diffPath = Path.Join(currentPath, "test/diff") |> PathType.create
            diffPath |> deleteDirectory |> ignore 
            Directory.CreateDirectory(diffPath.Value) |> ignore

            printfn "================1================"

            if diffResult.IsNone then
                failwith "diff"

            if copy diffResult.Value test1 diffPath |> not then
                failwith "copy"

            printfn "================2================"

            printfn "===test list begin"

            list history

            printfn "===test list end"

            merge diffPath test2 |> ignore

            printfn "================3================"

            match compare test1 test2 with
            | Ok true ->
                printfn "通過測試！" |> ignore
            | Ok false ->
                    failwith "compare"
            | Error e -> raise e
            )
        |> Result.ofTry

    let test (config: Config) (currentPath: string) (target: string) : bool =
        match target with
        | "all" ->
            testAll config currentPath |> ignore
            true
        | "init" ->
            let testPath = Path.Join(currentPath, "test")
            let history = Path.Join(testPath, ".history")
            printfn "%A" (testPath |> PathType.create |> init)
            Directory.Exists(history)
        | "add" ->
            let testPath = Path.Join(currentPath, "test")
            let history = Path.Join(testPath, ".history")
            if Directory.Exists(testPath) then
                Directory.Delete(testPath, true)
            testPath |> PathType.create |> init |> ignore
            File.WriteAllText(Path.Join(testPath, "1.txt"), "1")
            add config "1" (testPath |> PathType.create) (history |> PathType.create)
            let content = File.ReadAllText(Path.Join(history, "1", "data.json"))
            printfn "content: %s" content
            let lst = JsonSerializer.Deserialize<Info list>(content)
            lst.Length = 1 && lst[0].FullName = "\\1.txt"
        | _ -> failwith "Unkowned target!"
