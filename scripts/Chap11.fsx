#load "Util.fsx"

/// 0から受け取った自然数までの2乗の和を求める
let sumOfSquare (n: int) : int =
    let rec loop n acc =
        if n <= 0 then acc
        else loop (n - 1) (acc + n * n)
    loop n 0

assertEq "11-1" (sumOfSquare 0) 0
assertEq "11-1" (sumOfSquare 1) 1
assertEq "11-1" (sumOfSquare 4) 30

/// a_0 = 3, a_n = 2 * a_n-1 - 1 の漸化式からなる数列の一般項を求める
let rec question112 (n: int) : int =
    if n <= 0 then 3
    else 2 * question112 (n - 1) - 1

assertEq "11-2" (question112 0) 3
assertEq "11-2" (question112 1) 5
assertEq "11-2" (question112 10) 2049