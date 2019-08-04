#load "Util.fsx"

/// 昇順にソート済みの整数リストと整数nを受け取り、昇順となる位置にnを挿入したリストを返す
let rec insert (lst: int list) (n: int) : int list =
    match lst with
    | [] -> [n]
    | first :: rest ->
        if n <= first then n :: first :: rest
        else first :: insert rest n
assertEq "10-1" (insert [] 10) [10]
assertEq "10-1" (insert [1;2;4;5] 3) [1;2;3;4;5]
assertEq "10-1" (insert [2;3;4;5] 1) [1;2;3;4;5]
assertEq "10-1" (insert [2;3;4;5] 6) [2;3;4;5;6]