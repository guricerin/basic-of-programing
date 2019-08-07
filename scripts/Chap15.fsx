#load "Util.fsx"


/// takeLessかtakeGreaterのどちらかに=をつけないと、重複する要素が削除される
let rec quickSort lst =
    let take n lst p =
        List.filter (fun x -> p x n) lst
    let takeLess n lst = take n lst (<=)
    let takeGreater n lst = take n lst (>)
    match lst with
    | [] -> []
    | first :: rest ->
        quickSort (takeLess first rest)
        @ [first]
        @ quickSort (takeGreater first rest)

assertEq "15-1" (quickSort []) []
assertEq "15-1" (quickSort [1]) [1]
assertEq "15-1" (quickSort [1;2]) [1;2]
assertEq "15-1" (quickSort [2;1]) [1;2]
assertEq "15-1" (quickSort [5;4;9;8;2;3]) [2;3;4;5;8;9]
assertEq "15-1" (quickSort [4;3;3;3]) [3;3;3;4]

/// 二つの自然数を受け取り、最大公約数を返す
let rec gcd (n: int) (m: int) =
    let n, m = if n <= m then n, m else m, n
    match n with
    | 0 -> m
    | _ -> gcd n (m % n)

assertEq "15-2" (gcd 0 1) 1
assertEq "15-2" (gcd 2 3) 1
assertEq "15-2" (gcd 121 11) 11

/// n以下の素数を列挙する
let prime (n: int) =
    let rec sieve (lst: int list) : int list =
        match lst with
        | [] -> []
        | first :: rest ->
            let f x = x % first <> 0
            first :: List.filter f (sieve rest)
    [2..n] |> sieve

assertEq "15-3" (prime 2) [2]
assertEq "15-3" (prime 10) [2;3;5;7]
assertEq "15-3" (prime 20) [2;3;5;7;11;13;17;19]