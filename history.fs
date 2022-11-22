namespace Common
open System.IO
open System.Text.Json

module History =
    /// <summary>生成目錄信息</summary>
    /// <param name="path">路徑</param>
    /// <returns>信息</returns>
    let add (path: string) : Info =
        // 獲取當前
        let lastWriteTime (info: FileEntry.T) =
            let fileTime (p: FileSystemInfo) =
                p.LastWriteTime.ToLocalTime().ToFileTime()
            match info with
            | FileEntry.Dir dir -> new DirectoryInfo(dir) |> fileTime
            | FileEntry.File file -> new FileInfo(file) |> fileTime

        let getRelativePath (path: string) (info: FileEntry.T) : FileEntry.T =
            let relative = Path.GetRelativePath(path, info.Value)
            match info with
            | FileEntry.Dir _ -> relative |> FileEntry.Dir
            | FileEntry.File _ -> relative |> FileEntry.File

        let getInfo (path: string) (p: FileEntry.T) =
            let relative = getRelativePath path p
            (relative, lastWriteTime p)

        let (dir, file) =
            Common.getFileSystemEntries path
            |> Array.map FileEntry.create
            |> Array.map (getInfo path)
            |> Array.partition (fun (k, _) -> FileEntry.isDir k)
            ||> (fun a b ->
                let f m =
                    m
                    |> Array.map (fun (k: FileEntry.T, v) -> k.Value, v)
                    |> Map.ofArray
                (a |> f, b |> f))
        { Dir = dir; File = file }

    let diff (a: Info) (b: Info) : Diff =
        let differenceValue (f: 'v -> 'v -> bool) =
            Common.difference (fun _ x y -> f x y)
        let diffKey = differenceValue (fun _ _ -> false)
        let newDir = diffKey a.Dir b.Dir
        let deleteDir = diffKey b.Dir a.Dir
        let updateFile = differenceValue (<>) a.File b.File
        let deleteFile = diffKey b.File a.File
        { Update = { Dir = newDir; File = updateFile }; Delete = { Dir = deleteDir; File = deleteFile } }

    /// <summary>複製差異部分</summary>
    /// <param name="df">差異</param>
    /// <param name="path">輸入</param>
    /// <param name="target">輸出</param>
    /// <returns>無</returns>
    let copy (df: Diff) (path: string) (target: string) =
        let update = df.Update
        let delete = df.Delete
        // 更新的要複製
        update.Dir |> Map.iter (fun k _ ->
            // let k = k.Value
            let dest = Common.join3 target "data" k
            FileEntry.Dir dest |> Common.createDirectory)
        update.File |> Map.iter (fun k _ ->
            let source = Common.join path k
            let dest = Common.join3 target "data" k
            Common.createDirectoryFor dest
            printfn "[%s] source: %s" __LINE__ source
            printfn "[%s] dest: %s" __LINE__ dest
            if source |> FileEntry.File |> Common.exists then
                File.Copy(source, dest, true)
            else
                printfn "[%s] source not exists: %s" __LINE__ source)
        // 刪除的衹要Json就好
        let jsonContent = JsonSerializer.Serialize(df)
        File.WriteAllText(Common.join target "data.json", jsonContent)

    /// <summary>合併</summary>
    /// <param name="path">要處理目錄</param>
    /// <param name="d">補丁包路徑</param>
    /// <returns>無</returns>
    let merge (path: string) (d: string) =
        // 讀配置
        let content = File.ReadAllText(Common.join d "data.json")
        let dif = JsonSerializer.Deserialize<Diff>(content)
        let joinPath = Common.join path
        let delete k = joinPath k |> FileEntry.create |> Common.delete
        dif.Update.Dir |> Map.iter (fun k _ ->
            joinPath k |> FileEntry.Dir |> Common.createDirectory)
        dif.Update.File |> Map.iter (fun k _ ->
            joinPath k |> Common.createDirectoryFor
            let source = Common.join3 d "data" k
            let dest = joinPath k
            printfn "[%s] source: %s" __LINE__ source
            printfn "[%s] dest: %s" __LINE__ dest
            if source |> FileEntry.File |> Common.exists |> not then
                failwith "not exists"
            File.Copy(source, dest, true))
        dif.Delete.Dir |> Map.iter (fun k _ -> delete k)
        dif.Delete.File |> Map.iter (fun k _ -> delete k)

    let compare (a: Info) (b: Info) : bool =
        // 目錄同時存在就當相等
        let dir = Common.difference (fun _ _ _ -> false) a.Dir b.Dir
        let file = Common.difference (fun _ i j -> i <> j) a.File a.File
        dir.IsEmpty && file.IsEmpty
