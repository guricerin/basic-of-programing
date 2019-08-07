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

/// 9-6で作成したconcatをfold_rightを使って定義
let concat (lst: string list) : string =
    let f x acc = x + acc
    List.foldBack f lst ""

assertEq "14-3" (concat []) ""
assertEq "14-3" (concat ["春"]) "春"
assertEq "14-3" (concat ["春";"夏";"秋";"冬"]) "春夏秋冬"

/// Gakusei型のリストを受け取り、全員の点数の合計を返す
let gakuseiSum (lst: Gakusei list) : int =
    let f x acc = x.tensuu + acc
    List.foldBack f lst 0

assertEq "14-4" (gakuseiSum []) 0
assertEq "14-4" (gakuseiSum gakuseiList1) 403

/// 13-1で示したcountをfilterとlengthを使って定義
let count (lst: Gakusei list) (seiseki: string) : int =
    let f x = x.seiseki = seiseki
    List.filter f lst |> List.length

assertEq "14-6" (count [] "F") 0
assertEq "14-6" (count gakuseiList1 "F") 1
assertEq "14-6" (count gakuseiList1 "A") 2

/// 整数を受け取り、その2乗から1引いた数を返す匿名関数
let queation148 = fun x -> x * x - 1

assertEq "14-8" (queation148 0) -1
assertEq "14-8" (queation148 1) 0
assertEq "14-8" (queation148 100) 9999

#load "Person.fsx"

/// 8-3で定義したPerson型のリストを受け取り、名前フィールドを取り出す匿名関数
let queation149 = List.map (fun x -> x.name)

assertEq "14-9" (queation149 []) []
assertEq "14-9" (queation149 persons1) ["ikeda";"ueki";"akasaki"]

/// 14-3で作成したconcatをワンライナーで定義
let concat' (lst: string list) : string = List.foldBack (+) lst ""

assertEq "14-14" (concat' []) ""
assertEq "14-14" (concat' ["春"]) "春"
assertEq "14-14" (concat' ["春";"夏";"秋";"冬"]) "春夏秋冬"

let enumerate n = [for i in n..(-1)..1 -> i]

let divisor n =
    enumerate n
    |> List.filter (fun x -> n % x = 0)

/// 1から受け取った自然数までの合計を求める関数（enumerateを使う）
let oneToN n =
    enumerate n
    |> List.sum

assertEq "14-15" (oneToN 0) 0
assertEq "14-15" (oneToN 1) 1
assertEq "14-15" (oneToN 10) 55

/// 階乗を求める関数（enumerateを使う）
let fac n =
    enumerate n
    |> List.fold (*) 1

assertEq "14-16" (fac 0) 1
assertEq "14-16" (fac 1) 1
assertEq "14-16" (fac 10) 3628800