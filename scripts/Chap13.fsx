#load "Util.fsx"

#load "Person.fsx"

/// Person型のリストを受け取り、指定した血液型の人数を返す
let countKetsueki (lst: Person list) (blood: BloodType) : int =
    let rec loop lst acc =
        match lst with
        | [] -> acc
        | first :: rest ->
            let acc =
                if first.bloodType = blood then acc + 1
                else acc
            loop rest acc
    loop lst 0

assertEq "13-1" (countKetsueki [] A) 0
assertEq "13-1" (countKetsueki persons1 A) 1
assertEq "13-1" (countKetsueki persons1 B) 0

/// Person型のリストを受け取り、人の名前を抽出しリストにして返す
let personNamae (lst: Person list) : string list =
    List.map (fun x -> x.name) lst

assertEq "13-2" (personNamae []) []
assertEq "13-2" (personNamae persons1) ["ikeda";"ueki";"akasaki"]

/// 'a -> 'a
let question1331 x = x
solve "13-3" (question1331 0)
solve "13-3" (question1331 "あほ")
solve "13-3" (question1331 ['a';'b';'c';])

/// 'a -> 'b -> 'a
let question1332 x y = x
solve "13-3" (question1332 0 'a')
solve "13-3" (question1332 "あほ" 1)
solve "13-3" (question1332 ['a';'b';'c';] "kasu")

/// 'a -> 'b -> 'b
let question1333 x y = y
solve "13-3" (question1333 0 'a')
solve "13-3" (question1333 "あほ" 1)
solve "13-3" (question1333 ['a';'b';'c';] "kasu")

/// 'a -> ('a -> 'b) -> 'b
let question1334 x f = f x
solve "13-3" (question1334 1 float)
solve "13-3" (question1334 [1;2;3;4;5] (List.map (fun x -> x * x)))
solve "13-3" (question1334 [|'A';'B';'C';'D';'E'|] (Array.mapi (fun i x -> i * i)))

/// ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
let question1335 f g x = x |> f |> g
solve "13-3" (question1335 float ((*) 3.) 10)
solve "13-3" (question1335 (List.map (fun x -> x.ToString())) (List.fold (fun acc x -> acc + x) "hoge") [1;2;3;4;5])
solve "13-3" (question1335 ((*) 10L) ((+) 8L) 1_000_000_009L)

/// 関数を2つ受け取り、それらを合成する
let compose f g = g >> f
let time2 = (*) 2
let add3 = (+) 3
assertEq "13-4" (4 |> (compose time2 add3)) 14

let twice f = f >> f
solve "13-5" (9 |> (twice add3))

// let twice2 = twice twice // 型推論失敗
let hoge = twice twice ((+) 10)
solve "13-5" (hoge 1)