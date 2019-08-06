#load "Util.fsx"

/// 9-5で作成したevenをfilterを用いて定義
let even (lst: int list) : int list =
    let f x = x % 2 = 0
    List.filter f lst

assertEq "14-1" (even []) []
assertEq "14-1" (even [1]) []
assertEq "14-1" (even [2;1;6;4;7]) [2;6;4]

#load "Gakusei.fsx"

/// 9-6で示したcount_Aをfilterとlengthを用いて定義
let countA (lst: Gakusei list) : int =
    let f x = x.seiseki = "A"
    List.filter f lst |> List.length

assertEq "14-2" (countA []) 0
assertEq "14-2" (countA gakuseiList1) 2