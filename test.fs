namespace Common
open System.IO

module Test =
    let init (path: string) =
        let dir = FileEntry.Dir (Common.join path "test")
        if dir |> Common.exists then
            Common.delete dir
        Common.createDirectory dir

        // 創建文件
        File.WriteAllText(Common.join dir.Value "1.txt", "1")

        FileEntry.Dir (Common.join dir.Value "2") |> Common.createDirectory
        File.WriteAllText(Common.join3 dir.Value "2" "3.txt", "3")

        // 文件夹
        (Common.join dir.Value "3") |> FileEntry.Dir |> Common.createDirectory
        dir

    let update (dir: FileEntry.T) =
        // 增
        File.WriteAllText(Common.join dir.Value "4.txt", "4")
        // 刪
        FileEntry.File (Common.join3 dir.Value "2" "3.txt") |> Common.delete
        // 改
        File.WriteAllText(Common.join dir.Value "1.txt", "11")
        // 增
        (Common.join3 dir.Value "2" "3") |> FileEntry.Dir |> Common.createDirectory
        // 刪
        (Common.join dir.Value "3") |> FileEntry.Dir |> Common.delete

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
        Common.getFileSystemEntries path
        |> Array.map (fun x -> Common.getRelativePath path x//Path.GetRelativePath(path, x)
        )
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

        Common.join path "test2" |> FileEntry.Dir |> Common.delete
        Common.copyAll dir.Value (Common.join path "test2")

        let result = getFileList (Common.join path "test2")

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
        let diffPath = Common.join path "diff"
        if diffPath |> FileEntry.Dir |> Common.exists then
            diffPath |> FileEntry.Dir |> Common.delete
        History.copy df path diffPath

        // 合併差異到備份
        History.merge (Common.join path "test2") (Common.join path "diff")

        // 對比
        if getFileList (Common.join path "test") <> getFileList (Common.join path "test2") then
            printfn "測試失敗"
            exit 1

        printfn "測試通過"
