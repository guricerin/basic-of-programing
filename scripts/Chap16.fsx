#load "Util.fsx"

/// 整数のリストを受け取り、それまでの数の合計からなるリストを返す
let sumList (lst: int list) : int list =
    let rec loop acc = function
    | [] -> []
    | first :: rest ->
        let acc = acc + first
        acc :: loop acc rest
    loop 0 lst

assertEq "16-1" (sumList []) []
assertEq "16-1" (sumList [1]) [1]
assertEq "16-1" (sumList [3;2;1;4]) [3;5;6;10]

let rec foldLeft f init lst =
    match lst with
    | [] -> init
    | first :: rest -> foldLeft f (f init first) rest

assertEq "16-2" (foldLeft (+) 0 [1;2;3;4;5]) 15
assertEq "16-2" (foldLeft (fun lst x -> x :: lst) [] [1;2;3;4;5]) [5;4;3;2;1]