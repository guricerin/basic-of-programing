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
let ketsuekiShukei (lst: Person list) : int * int * int * int =
    let rec loop lst acc =
        match lst with
        | [] -> acc
        | first :: rest ->
            let (a, b, o, ab) = acc
            match first.bloodType with
            | A -> loop rest (a+1, b, o, ab)
            | B -> loop rest (a, b+1, o, ab)
            | O -> loop rest (a, b, o+1, ab)
            | AB -> loop rest (a, b, o, ab+1)
    loop lst (0,0,0,0)

assertEq "10-7" (ketsuekiShukei []) (0,0,0,0)
assertEq "10-7" (ketsuekiShukei persons1) (1,0,1,1)

/// Person型のリストを受け取り、最も人数の多い血液型を返す
/// この実装だと、最多の血液型が複数ある場合に一番先頭の血液型を返してしまう
let saitaKetsueki (lst: Person list) : BloodType =
    let (a,b,o,ab) = ketsuekiShukei lst
    let nax = [a;b;o;ab] |> List.max
    if nax = a then A
    else if nax = b then B
    else if nax = o then O
    else AB

assertEq "10-8" (saitaKetsueki []) A
assertEq "10-8" (saitaKetsueki persons1) A

/// 2つのリストを受け取り、同じ長さか判定する（lengthを使わないこと）
let rec equalLength lst1 lst2 : bool =
    match (lst1, lst2) with
    | ([], []) -> true
    | (_ :: _, []) -> false
    | ([], _ :: _) -> false
    | (_ :: rest1, _ :: rest2) -> equalLength rest1 rest2

assertEq "10-9" (equalLength [] []) true
assertEq "10-9" (equalLength [1;2;3] [4;5]) false
assertEq "10-9" (equalLength [1;2] [3;4;5;6]) false
assertEq "10-9" (equalLength [1;2;3] [4;5;6]) true
