#load "Util.fsx"

#load "Person.fsx"

/// Person型のリストを受け取り、最初のA型の人のレコードをオプション型で返す
let rec firstA = function
| [] -> None
| first :: rest ->
    if first.bloodType = A then Some (first)
    else firstA rest

assertEq "18-1" (firstA []) None
assertEq "18-1" (firstA persons1) (Some ({name="akasaki"; height=160.; weight=60.; birth=(1,21); bloodType=A}))

let rec price item = function
| [] -> None
| (yasai, nedan) :: rest ->
    if item = yasai then Some (nedan)
    else price item rest

/// 野菜のリストと八百屋のリストを受け取り、八百屋に置いていない野菜の数を返す
let rec countUrikireYasai (yasaiList: string list) (yaoyaList: (string * int) list) : int =
    match yasaiList with
    | [] -> 0
    | first :: rest ->
        match price first yaoyaList with
        | None -> 1 + countUrikireYasai rest yaoyaList
        | Some (_) -> countUrikireYasai rest yaoyaList

let yaoyaList = [("トマト", 300); ("たまねぎ", 200); ("にんじん", 150); ("ほうれん草", 200)]

assertEq "18-2" (countUrikireYasai ["たまねぎ"; "にんじん"] yaoyaList) 0
assertEq "18-2" (countUrikireYasai ["たまねぎ"; "じゃがいも"; "にんじん"] yaoyaList) 1
assertEq "18-2" (countUrikireYasai ["しいたけ"; "なす"; "にんじん"] yaoyaList) 2