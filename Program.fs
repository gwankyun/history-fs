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

// let test' =
//     argsTab

let test' =
    argTable
    |> tryGet "--test" 0

if test'.IsSome then
    let result =
        History.test (Directory.GetCurrentDirectory())

    match result with
    | Error e -> raise e |> ignore
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
        History.merge merge output
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

History.list history.Value

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
    // printfn "%A" result
    if output'.IsSome then
        let output = output'.Value |> History.PathType.create
        if Directory.Exists(output.Value) |> not then
            exit 1
        printfn "copy: %A" (History.copy result path output)
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
