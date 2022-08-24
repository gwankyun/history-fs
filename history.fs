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

module History =
    let maybe = new MaybeBuilder()

    type Info = {
        FullName: string
        LastWriteTime: int64
        IsDir: bool
    }

    let init (path: string) =
        let history = Extend.Result.Path.tryCombine path ".history"
        let h = history |> Extend.Result.get
        if h |> Extend.Directory.exists |> not then
            Directory.CreateDirectory(h) |> ignore
        h

    let add (name: string) (path: string) (history: string) =
        Directory.CreateDirectory(Path.Join(history, name)) |> ignore

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
            |> List.map (fun x -> { x with FullName = x.FullName.Substring(path.Length) })
        let json = Path.Combine(history, name, "data.json")
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

    let copy (result: DiffResult) (source: string) (dist: string) =
        match result with
        | Some c, Some m, Some d ->
            printfn "c: %A" c
            let partition (d: Diff) = d |> Map.partition (fun _ v -> v.IsDir)
            let copyFile (f: Diff) =
                f |> Map.iter (fun k _ ->
                let oldPath = Path.Join(source, k)
                let newPath = Path.Join(dist, k)
                FileUtility.createDirectoryFor newPath |> ignore
                File.Copy(oldPath, newPath, true))
            let copyDir (d: Diff) =
                d 
                |> Map.iter (fun k _ ->
                    let path = Path.Combine(dist, k)
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
                        let path = Path.Combine(dist, ".history", "data.json")
                        FileUtility.createDirectoryFor path |> ignore
                        printfn "path: %A" path
                        File.WriteAllText(path, x))
                    r)
            result.IsSome
            // |> Option.isSome
        | _ -> false

    let merge (src: string) (dest: string) =
        // 複製src所有非.history目錄下的文件到dest
        let sc = Path.TrimEndingDirectorySeparator(src)
        let ds = Path.TrimEndingDirectorySeparator(dest)
        let history = Path.Join(sc, ".history")
        FileUtility.copyAll src dest (fun o _ -> o.StartsWith(history) |> not)

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

    let compare (path1: string) (path2: string) =
        let p1 = Path.TrimEndingDirectorySeparator(path1)
        let p2 = Path.TrimEndingDirectorySeparator(path2)
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

    let diff (history: string) (newPath: string) (oldPath: string) : DiffResult =
        // printfn "path: %A" (newPath, oldPath)
        let newFile = newPath |> toJsonPath history |> Option.tryReadAllText 
        let oldFile = oldPath |> toJsonPath history |> Option.tryReadAllText
        // printfn "file: %A" (newFile, newFile)
        let textToMap (text: string option) =
            text
            |> Option.map
                (fun (x) ->
                    JsonSerializer.Deserialize<Info list>(x)
                    |> List.map (fun x -> x.FullName, x)
                    |> Map.ofList)
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

    let test (currentPath: string) =
        (fun () ->
        // let currentPath = Directory.GetCurrentDirectory()
            printfn "%A" currentPath
            let test1 = Path.Join(currentPath, "test/test1")
            let test2 = Path.Join(currentPath, "test/test2")
            if Directory.Exists(test1) then
                Directory.Delete(test1, true)
            if Directory.Exists(test2) then
                Directory.Delete(test2, true)
            FileUtility.copyAll (Path.Join(currentPath, "test/test")) test1 (fun _ _ -> true)
            FileUtility.copyAll (Path.Join(currentPath, "test/test")) test2 (fun _ _ -> true)

            // 兩個目錄要相等
            if compare test1 test2 |> not then
                failwith "compare"

            // printfn "================1================"

            let history = init test1

            // 添加當前記錄
            add "1" test1 history

            // 修改test1
            // 添加目录
            Directory.CreateDirectory(Path.Join(test1, "4")) |> ignore

            // 添加文件
            File.WriteAllText(Path.Join(test1, "2.txt"), "2")
            File.WriteAllText(Path.Join(test1, "4/4.txt"), "4")
            // printfn "================2================"

            // 修改文件
            File.WriteAllText(Path.Join(test1, "1.txt"), "1")

            // 刪除文件
            File.Delete(Path.Join(test1, "3/4.txt"))

            // 保存修改後的狀態
            add "2" test1 history

            let diffResult = diff history "2" "1"

            let diffPath = Path.Join(currentPath, "test/diff")
            if Directory.Exists(diffPath) then
                Directory.Delete(diffPath, true)
            Directory.CreateDirectory(diffPath) |> ignore

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
