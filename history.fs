module History
open System.IO
open System.Text.Json

open Common
/// <summary>生成目錄信息</summary>
/// <param name="path">路徑</param>
/// <returns>信息</returns>
let add (path: string) : Info =
    // 獲取當前
    let lastWriteTime (info: Entry.T) =
        let fileTime (p: FileSystemInfo) =
            p.LastWriteTime.ToLocalTime().ToFileTime()
        match info with
        | Entry.Dir dir -> new DirectoryInfo(dir) |> fileTime
        | Entry.File file -> new FileInfo(file) |> fileTime
        | Entry.Inexistence -> failwith "Inexistence"

    let getRelativePath (path: string) (info: Entry.T) : Entry.T =
        let relative = Path.GetRelativePath(path, info.Value)
        match info with
        | Entry.Dir _ -> relative |> Entry.Dir
        | Entry.File _ -> relative |> Entry.File
        | Entry.Inexistence -> failwith "Inexistence"

    let getInfo (path: string) (p: Entry.T) =
        let relative = getRelativePath path p
        (relative, lastWriteTime p)

    let (dir, file) =
        getFileSystemEntries path
        |> Array.map Entry.create
        |> Array.map (getInfo path)
        |> Array.partition (fun (k, _) -> Entry.isDir k)
        ||> (fun a b ->
            let f m =
                m
                |> Array.map (fun (k: Entry.T, v) -> k.Value, v)
                |> Map.ofArray
            (a |> f, b |> f))
    { Dir = dir; File = file }

let diff (a: Info) (b: Info) : Diff =
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
    let update = df.Update
    let delete = df.Delete
    // 更新的要複製
    update.Dir |> Map.iter (fun k _ ->
        let dest = join3 target "data" k
        dest |> Entry.createDirectory |> ignore)
    update.File |> Map.iter (fun k _ ->
        let source = join path k
        let dest = join3 target "data" k
        createDirectoryFor dest
        printfn "[%s] source: %s" __LINE__ source
        printfn "[%s] dest: %s" __LINE__ dest
        if source |> Entry.File |> exists then
            File.Copy(source, dest, true)
        else
            printfn "[%s] source not exists: %s" __LINE__ source)
    // 刪除的衹要Json就好
    let jsonContent = JsonSerializer.Serialize(df)
    File.WriteAllText(join target "data.json", jsonContent)

let deleteReadOnly path =
    let mutable info = new FileInfo(path)
    if info.Exists && info.IsReadOnly then
        info.IsReadOnly <- false

/// <summary>合併</summary>
/// <param name="path">要處理目錄</param>
/// <param name="d">補丁包路徑</param>
/// <returns>無</returns>
let merge (path: string) (d: string) =
    // 讀配置
    let content = File.ReadAllText(join d "data.json")
    let dif = JsonSerializer.Deserialize<Diff>(content)
    let joinPath = join path
    let delete k = joinPath k |> Entry.create |> Entry.deleteWith ignore
    dif.Update.Dir |> Map.iter (fun k _ ->
        joinPath k |> Entry.createDirectory |> ignore)
    dif.Update.File |> Map.iter (fun k _ ->
        joinPath k |> createDirectoryFor
        let source = join3 d "data" k
        let dest = joinPath k
        printfn "[%s] source: %s" __LINE__ source
        printfn "[%s] dest: %s" __LINE__ dest
        if source |> Entry.File |> exists |> not then
            failwith "not exists"
        if File.Exists(source) && File.Exists(dest) then
            deleteReadOnly dest
            File.Copy(source, dest, true))
    let deleteKey k _ = delete k
    dif.Delete.Dir |> Map.iter deleteKey
    dif.Delete.File |> Map.iter deleteKey

let compare (a: Info) (b: Info) : bool =
    // 目錄同時存在就當相等
    let dir = difference (fun _ _ _ -> false) a.Dir b.Dir
    let file = difference (fun _ i j -> i <> j) a.File a.File
    dir.IsEmpty && file.IsEmpty
