#load "Util.fsx"

let siki = "春" :: "夏" :: "秋" :: "冬" :: []
solve "9-1" siki

let siki2 = ["春"; "夏"; "秋"; "冬"]
solve "9-3" siki2

/// 整数のリストを受け取り、長さを返す
let rec length (lst: int list) : int =
    match lst with
    | [] -> 0
    | first :: rest -> 1 + length rest

assertEq "9-4" (length []) 0
assertEq "9-4" (length [1]) 1
assertEq "9-4" (length [2;1;6;4;7]) 5

/// 整数のリストを受け取り、偶数の要素のみを含むリストを返す
let rec even (lst: int list) : int list =
    match lst with
    | [] -> []
    | first :: rest ->
        if first % 2 = 0 then first :: even rest
        else even rest

assertEq "9-5" (even []) []
assertEq "9-5" (even [1]) []
assertEq "9-5" (even [2;1;6;4;7]) [2;6;4]

/// 文字列のリストを受け取り、要素を前から順にくっつけた文字列を返す
let rec concat (lst: string list) : string =
    match lst with
    | [] -> ""
    | first :: rest -> first + concat rest

assertEq "9-6" (concat []) ""
assertEq "9-6" (concat ["春"]) "春"
assertEq "9-6" (concat ["春";"夏";"秋";"冬"]) "春夏秋冬"


#load "Person.fsx"

/// Person型のリストを受け取り、血液型がAの人の名前からなるリストを返す
let rec countKetsuekiA (lst: Person list) : string list =
    match lst with
    | [] -> []
    | first :: rest ->
        match first.bloodType with
        | A -> first.name :: countKetsuekiA rest
        | _ -> countKetsuekiA rest

let persons = [
    {
        name = "akasaki"
        height = 160.
        weight = 60.
        birth = (1,21)
        bloodType = A
    };
    {
        name = "ikeda"
        height = 177.
        weight = 88.
        birth = (10,11)
        bloodType = O
    };
    {
        name = "ueki"
        height = 198.
        weight = 110.
        birth = (9,5)
        bloodType = AB
    }]
assertEq "9-7" (countKetsuekiA []) []
assertEq "9-7" (countKetsuekiA persons) ["akasaki"]

/// 誕生日を受け取り、星座を返す
let seiza (month : int, day : int) : string =
    let birth = month * 100 + day
    if 321 <= birth && birth <= 419 then "牡羊座"
    else if 420 <= birth && birth <= 520 then "牡牛座"
    else if 521 <= birth && birth <= 621 then "ふたご座"
    else if 622 <= birth && birth <= 722 then "蟹座"
    else if 723 <= birth && birth <= 822 then "獅子座"
    else if 823 <= birth && birth <= 922 then "乙女座"
    else if 923 <= birth && birth <= 1023 then "天秤座"
    else if 1024 <= birth && birth <= 1122 then "蠍座"
    else if 1123 <= birth && birth <= 1221 then "射手座"
    else if birth <= 119 || 1222 <= birth then "山羊座"
    else if 120 <= birth && birth <= 218 then "水瓶座"
    else "魚座"

/// Person型のリストを受け取り、乙女座の人の名前からなるリストを返す
let rec otomeza (lst: Person list) : string list =
    match lst with
    | [] -> []
    | first :: rest ->
        match seiza first.birth with
        | "乙女座" -> first.name :: otomeza rest
        | _ -> otomeza rest
assertEq "9-8" (otomeza []) []
assertEq "9-8" (otomeza persons) ["ueki"]