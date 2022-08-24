namespace Extend

module Map =
    let difference (a: Map<'k, 'v>) (b: Map<'k, 'v>) =
        a
        |> Map.filter (fun k _ -> b |> Map.containsKey k |> not)

    let intersectWith (f: 'k -> 'v -> 'v -> bool) (a: Map<'k, 'v>) (b: Map<'k, 'v>) =
        a
        |> Map.filter (fun k v1 ->
            match b |> Map.tryFind k with
            | Some v2 -> f k v1 v2
            | None -> false)
