namespace Extend

module Result =
    type Builder() =
        member this.Bind(x, f) = Result.bind f x
        member this.Return(x) = Ok x

    let workflow = new Builder()

    let ofTry (f: unit -> 'a) : Result<'a, exn> =
        try
            Ok (f())
        with
            | e -> Error e

