// For more information see https://aka.ms/fsharp-console-apps
open System
open System.IO
open System.Text.Json
open Common

let args = Environment.GetCommandLineArgs()
printfn "%A" args
printfn "%A" args.Length

match args[1] with
| "help" ->
    printfn "help"
    exit 0
| "add" ->
    if args.Length < 4 then
        printfn "need 4+ args"
        exit 1

    let path = args[2]
    if Directory.Exists(path) |> not then
        printfn "path not exists"
        exit 1

    let ver = args[3]
    
    printfn "path: %s" path

    let info = History.add path

    printfn "info: %A" info
    
    // 保存文件
    let jsonPath = Common.join3 path ".history" (ver + ".json")
    Common.createDirectoryFor jsonPath
    File.WriteAllText(jsonPath, JsonSerializer.Serialize(info))

    exit 0
| "diff" ->
    if args.Length < 6 then
        printfn "need 6+ args"
        exit 1
    
    let path = args[2]
    let target = args[3]
    if Directory.Exists(path) |> not then
        printfn "path not exists"
        exit 1

    let verNew = args[4]
    let verOld = args[5]

    if Directory.Exists(Path.Join(path, ".history")) |> not then
        exit 1

    let getInfo (path: string) (ver: string) : Info option =
        let verPath = Path.Join(path, ".history", ver + ".json")
        match File.Exists(verPath) with
        | true ->
            let content = File.ReadAllText(verPath)
            Some (JsonSerializer.Deserialize<Info>(content))
        | false ->
            None

    let newInfo: Info option = getInfo path verNew
    let oldInfo = getInfo path verOld

    match newInfo, oldInfo with
    | Some nI, Some oI ->
        let df = History.diff nI oI
        History.copy df path target
        exit 0
    | _ -> exit 1

    exit 0
| "merge" ->
    let path = args[2]
    let target = args[3]

    History.merge path target

    exit 0
| "test" ->
    if args.Length < 4 then
        printfn "參數太少"
        exit 1

    let cmd = args[2]
    let path = args[3]

    match cmd with
    | "add" ->
        if Test.add path then
            exit 0

    | "diff" ->
        Test.diff path
        exit 0

    | "merge" ->
        Test.merge path
        exit 0

    | _ ->
        printfn "未知命令"
        exit 1

    exit 0
| _ -> ()
