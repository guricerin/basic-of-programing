#load "Util.fsx"

/// n以下の素数を列挙する
let prime (n: int) =
    let rec sieve (lst: int list) : int list =
        do
            printfn "%d" (List.length lst)
        match lst with
        | [] -> []
        | first :: rest ->
            let f x = x % first <> 0
            first :: List.filter f (sieve rest)
    [2..n] |> sieve

solve "21-2" (prime 100)