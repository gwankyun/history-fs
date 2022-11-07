open System
open System.IO
open System.Text
open Extend
open History

module Int32 =
    let tryParse (str: string) =
        match System.Int32.TryParse str with
        | true, v -> Some v
        | false, _ -> None

let parseArgs (arg: string []) =
    let rec f (a: string list) m k =
        match a with
        | h::t ->
            match h.StartsWith("--"), k with
            | false, None -> f t m None
            | false, Some x ->
                let nm = m |> Map.change x (Option.map (fun n -> h::n))
                f t nm (Some x)
            | true, _ ->
                let nm = m |> Map.add h []
                f t nm (Some h)
        | [] -> m |> Map.map (fun _ v -> v |> List.rev)
    f (arg |> List.ofArray) Map.empty None

let tryGetIndex (opt: string) (args: string[]) =
    args
    |> Array.tryFindIndex ((=) opt)
    |> Option.map ((+) 1)
    |> Option.filter ((>) args.Length)

let args = Environment.GetCommandLineArgs();
printfn "%A" args

let argTable = args |> parseArgs

printfn "%A" argTable

let currentPath = Directory.GetCurrentDirectory()
printfn "%A" (Directory.GetCurrentDirectory())

// 查找參數
// key 參數
// num 數量
let tryGet (key: string) (num: int) (m: Map<string, string list>) =
    m
    |> Map.tryFind key
    |> Option.filter (List.length >> ((<=) num))
    |> Option.map (List.take num)

let help' =
    argTable
    |> tryGet "help" 0

printfn "args: %A" args

match args[1] with
| "help" ->
    printfn "test"
    printfn "add path version"
    printfn "diff path new old target"
    printfn "merge patch target"
    printfn "list path"
    exit 0
| "test" ->
    let result =
        History.test (Directory.GetCurrentDirectory())

    match result with
    | Error e -> raise e |> ignore
    | _ -> ()
    exit 0
| "list" ->
    if args.Length < 3 then
        // printfn "lsi patch target"
        exit 1
    let path = args[2] |> History.PathType.create
    let history = History.init path |> History.PathType.create
    History.list history
| "merge" ->
    printfn "%i" args.Length
    if args.Length < 4 then
        printfn "merge patch target"
        exit 1
    let patch = args[2] |> History.PathType.create
    let target = args[3] |> History.PathType.create
    match History.merge patch target with
    | Ok _ -> exit 0
    | Error e ->
        printfn "%s" e.Message
        exit 1
| "add" ->
    if args.Length < 4 then
        printfn "add path version"
        exit 1
    let path = args[2] |> History.PathType.create
    let version = args[3] |> History.PathType.create
    let history = History.init path |> History.PathType.create
    History.add version path history
    exit 0
| "diff" ->
    if args.Length < 6 then
        printfn "diff path new old target"
        exit 1
    let path = args[2] |> History.PathType.create
    let n = args[3] |> History.PathType.create // 新
    let o = args[4] |> History.PathType.create // 舊
    let output = args[5] |> History.PathType.create
    printfn "output: %s" output.Value
    let history = History.init path |> History.PathType.create
    let result = History.diff history n o
    if Directory.Exists(output.Value) then
        printfn "copy: %A" (History.copy result.Value path output)
        exit 0
    else
        printfn "no output"
        exit 1
    exit 0
| _ -> ()

let output' =
    argTable
    |> tryGet "--output" 1
    |> Option.map (List.item 0)

if output'.IsSome then
    let output = output'.Value
    printfn "output: %A" output

let merge' =
    argTable
    |> tryGet "--merge" 1
    |> Option.map (List.item 0)

if merge'.IsSome then
    let merge = merge'.Value |> History.PathType.create
    if output'.IsSome then
        let output = output'.Value |> History.PathType.create
        History.merge merge output |> ignore
        exit 0

let compare' =
    argTable
    |> tryGet "--compare" 2

if compare'.IsSome then
    let compare = compare'.Value
    let a = compare[0] |> History.PathType.create
    let b = compare[1] |> History.PathType.create
    let result = (fun () -> History.compare a b) |> Option.ofTry
    match result with
    | Some e -> printfn "對比結果：%A" e
    | None -> printfn "對比失敗"
    exit 0

let path' =
    argTable
    |> tryGet "--path" 1
    |> Option.map (List.item 0)

printfn "path: %A" path'

// 必須要目錄
if path'.IsNone then
    exit 1

let path = path'.Value |> History.PathType.create

let history = History.init path |> History.PathType.create

History.list history

let rename' =
    argTable
    |> tryGet "--rename" 2

if rename'.IsSome then
    let rename = rename'.Value
    let n = rename[0]// |> History.PathType.create // 新
    let o = rename[1]// |> History.PathType.create // 舊
    History.rename history o n |> ignore
    exit 0

let add' =
    argTable
    |> tryGet "--add" 1
    |> Option.map (List.item 0)

printfn "add" 

if add'.IsSome then
    History.add
        (add'.Value |> History.PathType.create)
        path
        history
    exit 0

let diff' =
    argTable
    |> tryGet "--diff" 2
    // |> Option.map (List.item 0)

if diff'.IsSome then
    let diff = diff'.Value
    let n = diff[0] |> History.PathType.create // 新
    let o = diff[1] |> History.PathType.create // 舊
    let result = History.diff history n o
    if output'.IsSome then
        let output = output'.Value |> History.PathType.create
        if Directory.Exists(output.Value) |> not then
            exit 1
        printfn "copy: %A" (History.copy result.Value path output)
    else
        printfn "no output"
    exit 0

let toHex (str: string) =
    Convert.ToHexString(Encoding.UTF8.GetBytes(str))

let getCmd (cmd: string) : string =
    let head = "3C55AAC3"
    let len = cmd.Length.ToString("X8")
    let checksum =
        cmd.ToCharArray()
        |> Array.map (fun x -> Convert.ToInt32(x))
        |> Array.reduce (+)
        |> (fun x -> x.ToString("X8"))
    let xml = toHex cmd
    head + len + checksum + "00000000" + xml
