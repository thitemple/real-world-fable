module Result

let toOption (result: Result<_, _>): Option<_> =
    match result with
    | Ok a -> Some a
    | Error _ -> None
