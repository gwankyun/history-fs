namespace Common
open System.IO
open System.Text.Json

module History =
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

        // let r =
        let (dir, file) =
            Common.getFileSystemEntries path
            |> Array.map FileEntry.create
            |> Array.map (getInfo path)
            // |> Map.ofArray
            |> Array.partition (fun (k, _) -> FileEntry.isDir k)
            ||> (fun a b ->
                let f m =
                    m
                    |> Array.map (fun (k: FileEntry.T, v) -> k.Value, v)
                    |> Map.ofArray
                (a |> f, b |> f))
            // |> Array.map (fun (k, v) -> (k.))
        { Dir = dir; File = file }

    let diff (a: Info) (b: Info) : Diff =
        let difference (f: 'k -> 'v -> 'v -> bool) (a: Map<'k, 'v>) (b: Map<'k, 'v>) =
            a |> Map.filter (fun k v ->
                match b |> Map.tryFind k with
                | Some x -> f k v x
                | None -> true)
        let differenceValue (f: 'v -> 'v -> bool) =
            difference (fun _ x y -> f x y)
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
        let joinAll = List.reduce Common.join
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
            printfn "source: %s" source
            printfn "dest: %s" dest
            if source |> FileEntry.File |> Common.exists then
                File.Copy(source, dest, true)
            else
                printfn "source not exists: %s" source)
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
        dif.Update.Dir |> Map.iter (fun k _ ->
            FileEntry.Dir (Common.join path k) |> Common.createDirectory)
        dif.Update.File |> Map.iter (fun k _ ->
            Common.createDirectoryFor (Common.join path k)
            printfn "source: %s" (Common.join path k)
            printfn "dest: %s" (Common.join3 d "data" k)
            File.Copy(Common.join3 d "data" k, Common.join path k, true))
        dif.Delete.Dir |> Map.iter (fun k _ ->
            FileEntry.Dir (Common.join path k) |> Common.delete)
        dif.Delete.File |> Map.iter (fun k _ ->
            FileEntry.File (Common.join path k) |> Common.delete)
