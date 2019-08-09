#load "Util.fsx"

let counter = ref 0
let gensym str =
    let str = sprintf "%s%d" str !counter
    counter := !counter + 1
    str

assertEq "22-1" (gensym "a") "a0"
assertEq "22-1" (gensym "x") "x1"
assertEq "22-1" (gensym "hoge") "hoge2"

let fibArray (arr: int array) =
    let fib = function
    | n when n <= 1 -> n
    | n -> arr.[n-1] + arr.[n-2]

    do
        Array.iteri (fun i x -> arr.[i] <- (fib i)) arr
    arr

assertEq "22-2" (fibArray [|0|]) [|0|]
assertEq "22-2" (fibArray [|0;0;0|]) [|0;1;1|]
let arr = [|0;0;0;0;0;0;0;0;0;0|]
assertEq "22-2" (fibArray arr) [|0;1;1;2;3;5;8;13;21;34|]
solve "22-2" arr // 書き換わっている