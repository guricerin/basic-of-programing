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

/// 整数のリストを受け取り、昇順にソートする（挿入ソート）
let insSort (lst: int list) : int list =
    let rec loop (sortedLst: int list) (n: int) (rest: int list) =
        match rest with
        | [] -> insert sortedLst n
        | first :: rest ->
            loop (insert sortedLst n) first rest
    match lst with
    | [] -> []
    | first :: rest -> loop [] first rest

assertEq "10-2" (insSort []) []
assertEq "10-2" (insSort [5;4;3;2;1]) [1;2;3;4;5]
assertEq "10-2" (insSort [3;2;5;1;4]) [1;2;3;4;5]
assertEq "10-2" (insSort [0;0;1;0;0]) [0;0;0;0;1]


#load "Gakusei.fsx"

let rec quickSort lst f =
    match lst with
    | [] -> []
    | pivot :: rest ->
        let comp x = f x pivot
        let left, right = List.partition comp rest
        quickSort left f @ [pivot] @ quickSort right f

// Gakusei型のリストを受け取り、tensuuフィールドを基準にソートしたリストを返す
let gakuseiSort (lst: Gakusei list) : Gakusei list =
    let f x y = x.tensuu < y.tensuu
    quickSort lst f

assertEq "10-3" (gakuseiSort []) []
assertEq "10-3" (gakuseiSort gakuseiList1) [
    {namae = "casu"; tensuu = 44; seiseki = "F"};
    {namae = "dameppi"; tensuu = 73; seiseki = "C"};
    {namae = "boke"; tensuu = 87; seiseki = "B"};
    {namae = "erodanshaku"; tensuu = 99; seiseki = "A"}
    {namae = "aho"; tensuu = 100; seiseki = "A"};
]

#load "Person.fsx"

/// Person型のリストを受け取り、名前の順にソートしたリストを返す
let personSort (lst: Person list) : Person list =
    let f x y = x.name < y.name
    quickSort lst f

assertEq "10-4" (personSort []) []
assertEq "10-4" (personSort persons1) [
    {name = "akasaki";height = 160.;weight = 60.;birth = (1,21);bloodType = A};
    {name = "ikeda";height = 177.;weight = 88.;birth = (10,11);bloodType = O};
    {name = "ueki";height = 198.;weight = 110.;birth = (9,5);bloodType = AB};
]

/// Gakusei型のリストを受け取り、最高得点の人のレコードを返す
let gakuseiMax (lst: Gakusei list) : int =
    let rec loop lst n =
        match lst with
        | [] -> n
        | first :: rest ->
            if first.tensuu >= n then loop rest first.tensuu
            else loop rest n
    loop lst System.Int32.MinValue

assertEq "10-5" (gakuseiMax []) System.Int32.MinValue
assertEq "10-5" (gakuseiMax gakuseiList1) 100

/// Person型のリストを受け取り、各血液型が何人いるかを組みにして返す
let rec ketsuekiShukei (lst: Person list) : int * int * int * int =
    match lst with
    | [] -> (0, 0, 0, 0)
    | first :: rest ->
        let (a, b, o, ab) = ketsuekiShukei rest
        match first.bloodType with
        | A -> (a+1, b, o, ab)
        | B -> (a, b+1, o, ab)
        | O -> (a, b, o+1, ab)
        | AB -> (a, b, o, ab+1)