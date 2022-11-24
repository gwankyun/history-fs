module Test
open System.IO
open System.Text.Json

open Common
let init (path: string) =
    let dir = join path "test"
    if Directory.Exists(dir) then
        Directory.Delete(dir, true)

    let dir = dir |> Entry.createDirectory

    // 創建文件
    dir |> Entry.iter (fun x ->
        File.WriteAllText(join x "1.txt", "1"))

    join dir.Value "2"
    |> Entry.createDirectory
    |> Entry.iter (fun x ->
        File.WriteAllText(join x "3.txt", "3"))

    // 文件夹
    join dir.Value "3" |> Entry.createDirectory |> ignore
    dir

let update (dir: Entry.T) =
    // 增
    File.WriteAllText(join dir.Value "4.txt", "4")
    // 刪
    join3 dir.Value "2" "3.txt" |> Entry.create |> Entry.deleteWith ignore
    // 改
    File.WriteAllText(join dir.Value "1.txt", "11")
    // 增
    join3 dir.Value "2" "3" |> Entry.createDirectory |> ignore
    // 刪
    join dir.Value "3" |> Entry.create |> Entry.deleteWith ignore

let test (m: Map<string, int64>) = m |> Map.keys |> List.ofSeq |> List.sort

let testInit (info: Info) : bool =
    let { Dir = dir; File = file} = info//add dir.Value
    let testDir = dir |> test
    let testFile = file |> test
    testDir = ["2"; "3"] && testFile = ["1.txt"; "2\\3.txt"]

let testUpdate (info2: Info) : bool =
    let { Dir = dir; File = file } = info2
    let testDir = dir |> test
    let testFile = file |> test
    testDir = ["2"; "2\\3"] && testFile = ["1.txt"; "4.txt"]

let printInto (info: Info) =
    let { Dir = dir; File = file} = info
    printfn "Dir: %A" dir
    printfn "File: %A" file

let getFileList (path: string) =
    getFileSystemEntries path
    |> Array.map (getRelativePath path)
    |> Array.sort

let add (path: string) =
    // 清空目錄
    let dir = init path

    let info = History.add dir.Value
    match testInit info with
    | true ->
        printfn "測試通過"
        true
    | false ->
        printInto info
        printfn "測試失敗"
        false

let diff (path: string) =
    let dir = init path

    let info1 = History.add dir.Value

    // 更新
    update dir

    let info2 = History.add dir.Value
    match testUpdate info2 with
    | true -> printfn "測試通過"
    | false ->
        printInto info2
        printfn "測試失敗"
        exit 1

    let df = History.diff info2 info1
    printfn "df: %A" df

    let updateFile = df.Update.File |> test
    if updateFile <> ["1.txt"; "4.txt"] then
        printfn "Update.File測試失敗"
        exit 1

    let updateDir = df.Update.Dir |> test
    if updateDir <> ["2\\3"] then
        printfn "Update.Dir測試失敗"
        exit 1

    let deleteFile = df.Delete.File |> test
    if deleteFile <> ["2\\3.txt"] then
        printfn "Delete.File測試失敗"
        exit 1

    let deleteDir = df.Delete.Dir |> test
    if deleteDir <> ["3"] then
        printfn "Delete.Dir測試失敗"
        exit 1

    printfn "測試通過"

let merge (path: string) =
    let dir = init path

    join path "test2" |> Entry.Dir |> delete
    copyAll dir.Value (join path "test2")

    let result = getFileList (join path "test2")

    if result <> ([|"2"; "3"; "1.txt"; "2\\3.txt"|] |> Array.sort) then
        printfn "測試失敗"
        exit 1

    let info1 = History.add dir.Value

    // 更新
    update dir

    let info2 = History.add dir.Value
    match testUpdate info2 with
    | true -> printfn "測試通過"
    | false ->
        printInto info2
        printfn "測試失敗"
        exit 1

    let df = History.diff info2 info1
    printfn "df: %A" df

    let updateFile = df.Update.File |> test
    if updateFile <> ["1.txt"; "4.txt"] then
        printfn "Update.File測試失敗"
        exit 1

    let updateDir = df.Update.Dir |> test
    if updateDir <> ["2\\3"] then
        printfn "Update.Dir測試失敗"
        exit 1

    let deleteFile = df.Delete.File |> test
    if deleteFile <> ["2\\3.txt"] then
        printfn "Delete.File測試失敗"
        exit 1

    let deleteDir = df.Delete.Dir |> test
    if deleteDir <> ["3"] then
        printfn "Delete.Dir測試失敗"
        exit 1

    // 保存差異到本地
    let diffPath = join path "diff"
    if diffPath |> Entry.Dir |> exists then
        diffPath |> Entry.Dir |> delete
    History.copy df (join path "test") diffPath

    // 合併差異到備份
    History.merge (join path "test2") (join path "diff")

    // 對比
    let a = History.add (join path "test")
    let b = History.add (join path "test2")
    if History.compare a b |> not then
        printfn "test: %A" a
        printfn "test2: %A" b
        printfn "[%s] 測試失敗" __LINE__
        exit 1   

    printfn "測試通過"

let compare (current: string) (json: string) : bool =
    let pathToInfo (p: string) =
        let txt = File.ReadAllText(p)
        JsonSerializer.Deserialize<Info>(txt)
    History.compare (History.add current) (pathToInfo json)
