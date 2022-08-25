namespace History
open Extend
open System
open System.IO
open System.Text.Json

module Result =
    let ofTry (f: unit -> 'a) : Result<'a, exn> =
        try
            Ok (f())
        with
            | e -> Error e

type MaybeBuilder() =
    member this.Bind(x, f) =
        match x with
        | None -> None
        | Some a -> f a
  
    member this.Return(x) =
        Some x

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module History =
    let maybe = new MaybeBuilder()

    type Info = {
        FullName: string
        LastWriteTime: int64
        IsDir: bool
    }

    // [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module PathType =
        type t =
            | PathType of string
            member this.Value =
                match this with
                | PathType s -> s
        let create (s: string) = PathType s
        let value (PathType str) = str 

    let init (path_: PathType.t) =
        let history = Extend.Result.Path.tryCombine path_.Value ".history"
        let h = history |> Extend.Result.get
        if h |> Extend.Directory.exists |> not then
            Directory.CreateDirectory(h) |> ignore
        h

    let add (name_: PathType.t) (path_: PathType.t) (history_: PathType.t) =
        // let name = name_ |> PathType.value
        // let path = path_ |> PathType.value
        // let history = history_ |> PathType.value
        Directory.CreateDirectory(Path.Join(history_.Value, name_.Value)) |> ignore

        let toFileTime (x: DateTime) = x.ToLocalTime().ToFileTime()
        let toInfo (isDir: bool) (file: FileSystemInfo) =
            {
                FullName = file.FullName;
                LastWriteTime = file.LastWriteTime |> toFileTime;
                IsDir = isDir
            }
        let files =
            FileUtils.getAllFiles(path_.Value)
            |> List.map (toInfo false)
        let dirs =
            FileUtils.getAllDirectories(path_.Value)
            |> List.map (toInfo true)
        let data =
            dirs
            |> List.append files
            |> List.map (fun x -> { x with FullName = x.FullName.Substring(path_.Value.Length) })
        let json = Path.Combine(history_.Value, name_.Value, "data.json")
        printfn "%s" json
        data
        |> List.filter (fun x -> x.FullName.StartsWith(".history") |> not)
        |> (fun x ->
            let data = JsonSerializer.Serialize(x)
            File.WriteAllText(json, data)
        )
        // |> Option.trySerialize
        // |> Option.bind (Option.tryWriteAllText json)

    let list (history: string) =
        let lst = Directory.GetDirectories(history)
        for i in lst do
            printfn "%A" i

    let toJsonPath (history: string) (ver: string) =
        Path.Combine(history, ver, "data.json")

    type Diff = Map<string, Info>
    type DiffResult = Diff option * Diff option * Diff option

    let copy (result: DiffResult) (source_: PathType.t) (dist_: PathType.t) =
        // let source = source_.Value
        // let dist = dist_.Value
        match result with
        | Some c, Some m, Some d ->
            printfn "c: %A" c
            let partition (d: Diff) = d |> Map.partition (fun _ v -> v.IsDir)
            let copyFile (f: Diff) =
                f |> Map.iter (fun k _ ->
                let oldPath = Path.Join(source_.Value, k)
                let newPath = Path.Join(dist_.Value, k)
                FileUtility.createDirectoryFor newPath |> ignore
                File.Copy(oldPath, newPath, true))
            let copyDir (d: Diff) =
                d 
                |> Map.iter (fun k _ ->
                    let path = Path.Join(dist_.Value, k)
                    if Directory.Exists(path) |> not then
                        Directory.CreateDirectory(path) |> ignore)

            printfn "c: %A" c
            let cd, cf = c |> partition
            printfn "cd: %A" cd
            printfn "================11================"
            printfn "cf: %A" cf
            cf |> copyFile // 新增的文件複製
            printfn "================12================"
            cd |> copyDir // 新增的目錄複製


            let _, mf = m |> partition
            mf |> copyFile // 修改的文件複製

            printfn "================13================"

            // 將其他信息寫入JSON
            // cd 創建的文件夾
            // d 刪除的文件的文件與文件夾
            let result =
                (fun () -> JsonSerializer.Serialize<Diff>(d))
                |> Option.ofTry
                |> Option.bind (fun x ->
                    let r = Option.ofTry (fun () ->
                        let path = Path.Join(dist_.Value, ".history", "data.json")
                        FileUtility.createDirectoryFor path |> ignore
                        printfn "path: %A" path
                        File.WriteAllText(path, x))
                    r)
            result.IsSome
            // |> Option.isSome
        | _ -> false

    let merge (src_: PathType.t) (dest_: PathType.t) =
        // 複製src所有非.history目錄下的文件到dest
        let sc = Path.TrimEndingDirectorySeparator(src_.Value)
        let ds = Path.TrimEndingDirectorySeparator(dest_.Value)
        let history = Path.Join(sc, ".history")
        FileUtility.copyAll src_.Value dest_.Value (fun o _ -> o.StartsWith(history) |> not) |> ignore

        let json = Path.Join(history, "data.json")
        if File.Exists(json) then
            let text = File.ReadAllText(json)
            let data = JsonSerializer.Deserialize<Diff>(text)
            for i in data do
                let path = Path.Join(ds, i.Key)
                if i.Value.IsDir then
                    if Directory.Exists(path) then
                        Directory.Delete(path, true)
                else
                    printfn "file: %A" path
                    if File.Exists(path) then
                        File.Delete(path)

    let compare (path1_: PathType.t) (path2_: PathType.t) =
        // let path1 = path1_ |> PathType.value
        // let path2 = path2_ |> PathType.value
        let p1 = Path.TrimEndingDirectorySeparator(path1_.Value)
        let p2 = Path.TrimEndingDirectorySeparator(path2_.Value)
        match Directory.Exists(p1), Directory.Exists(p2) with
        | true, true ->
            let getRelativePath (path: string) =
                Directory.GetFileSystemEntries(path, "*", SearchOption.AllDirectories)
                |> Array.map(fun x ->
                    let file = FileInfo(x)
                    x, file.LastWriteTime)
                |> Array.map (fun (x, w) -> Path.GetRelativePath(path, x), w)
                |> Array.filter (fun (x, _) -> x.StartsWith(".history") |> not)
                |> Array.sortBy (fun (x, w) -> x)
            let e1 = getRelativePath p1
            let e2 = getRelativePath p2
            let diff a b = a |> Array.filter (fun x -> b |> Array.tryFind (fun y -> x = y) |> Option.isNone)
            printfn "diff: %A\n%A" (diff e1 e2) (diff e2 e1)
            Array.compareWith (fun (a, aw) (b, bw) ->
                let pa = Path.Join(p1, a)
                let pb = Path.Join(p1, b)
                match Directory.Exists(pa), Directory.Exists(pb) with
                | true, true -> 0
                | false, false ->
                    if a = b && aw = bw then
                        0
                    else
                        1
                | _ -> 1
            ) e1 e2
            |> ((=) 0)
            // e1 = e2
        | _ -> false

    let diff (history_: PathType.t) (newPath_: PathType.t) (oldPath_: PathType.t) : DiffResult =
        // printfn "path: %A" (newPath, oldPath)
        let newFile = newPath_.Value |> toJsonPath history_.Value |> Option.tryReadAllText 
        let oldFile = oldPath_.Value |> toJsonPath history_.Value |> Option.tryReadAllText
        // printfn "file: %A" (newFile, newFile)
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
        let newData =
            newFile
            |> textToMap
        let oldData =
            oldFile
            |> textToMap
        // printfn "data: %A" (newData, oldData)
        let createFile =
            Option.map2 Map.difference newData oldData
        let modifyFile =
            let diffLastWriteTime _ (v1: Info) (v2: Info) =
                v1.LastWriteTime <> v2.LastWriteTime
            Option.map2 (Map.intersectWith diffLastWriteTime) newData oldData
        let deleteFile =
            Option.map2 Map.difference oldData newData
            // oldData
            // |> Option.map2 Map.difference newData
        createFile, modifyFile, deleteFile

    /// <summary>測試函數</summary>
    /// <param name="currentPath">項目的路徑</param>
    /// <returns>包括一個異常，可再次抛出</returns>
    let test (currentPath: string) =
        (fun () ->
            let deleteDirectory (path: PathType.t) =
                let p = path |> PathType.value
                if Directory.Exists(p) then
                    Directory.Delete(p, true)

            printfn "%A" currentPath
            let test1 = Path.Join(currentPath, "test/test1") |> PathType.create
            let test2 = Path.Join(currentPath, "test/test2") |> PathType.create
            deleteDirectory test1
            deleteDirectory test2

            let testPath = Path.Join(currentPath, "test/test")

            FileUtility.copyAll testPath test1.Value (fun _ _ -> true) |> ignore
            FileUtility.copyAll testPath test2.Value (fun _ _ -> true) |> ignore

            // 兩個目錄要相等
            if compare test1 test2 |> not then
                failwith "compare"

        // printfn "================1================"

            let history = init test1 |> PathType.create

            let the1 = "1" |> PathType.create

            // 添加當前記錄
            add the1 test1 history

            // 修改test1
            // 添加目录
            Directory.CreateDirectory(Path.Join(test1.Value, "4")) |> ignore

            // 添加文件
            File.WriteAllText(Path.Join(test1.Value, "2.txt"), "2")
            File.WriteAllText(Path.Join(test1.Value, "4/4.txt"), "4")
            // printfn "================2================"

            // 修改文件
            File.WriteAllText(Path.Join(test1.Value, "1.txt"), "1")

            // 刪除文件
            File.Delete(Path.Join(test1.Value, "3/4.txt"))

            let the2 = "2" |> PathType.create

            // 保存修改後的狀態
            add the2 test1 history

            let diffResult = diff history the2 the1

            let diffPath = Path.Join(currentPath, "test/diff") |> PathType.create
            diffPath |> deleteDirectory 
            Directory.CreateDirectory(diffPath.Value) |> ignore

            printfn "================1================"

            if copy diffResult test1 diffPath |> not then
                failwith "copy"

            printfn "================2================"

            merge diffPath test2

            printfn "================3================"

            if compare test1 test2 |> not then
                failwith "compare"
            else
                printfn "通過測試！"

            )
        |> Result.ofTry
