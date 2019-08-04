#load "Util.fsx"

let a = if 2 < 1 then 3 else 4
solve "5-1-1" (getType a)

// 条件式が真理値ではないのでエラー
// let b = if "true" then 3 else 4

let b = if "a" = "b" then false else true
solve "5-1-3" (getType b)

// if節とelse節の型が一致しないのでエラー
// let c = if true < false then 1 else "2"

let c = if not (3 = 4) then 1 < 2 else 1 > 2
solve "5-1-5" (getType c)

/// 時間を受け取り、午前か午後かを返す
let jikan (t : int) : string =
    let t = t % 24
    if 0 <= t && t < 12 then "午前"
    else "午後"

assertEq "5-2" (jikan 0) "午前"
assertEq "5-2" (jikan 12) "午後"
assertEq "5-2" (jikan 24) "午前"

/// 誕生日を受け取り、星座を返す
let seiza (month : int) (day : int) : string =
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

assertEq "5-3" (seiza 1 1) "山羊座"
assertEq "5-3" (seiza 3 20) "魚座"
assertEq "5-3" (seiza 12 31) "山羊座"

/// 二次方程式 ax^2 + bx + c = 0 の係数を受け取り、判別式の値を返す
/// a > 0
/// 判別式の符号が正 -> その二次方程式は異なる実数解を2つもつ
/// 0 -> 実数解を1つ（重解）をもつ
/// 負 -> 互いに共役な2つの複素数解をもつ
let hanbetsusiki (a : float) (b : float) (c : float) : float =
    b ** 2. - 4. * a * c

assertEq "5-4" (hanbetsusiki 2. 3. -1.) 17.
assertEq "5-4" (hanbetsusiki 1. -6. 9.) 0.
assertEq "5-4" (hanbetsusiki 15. 24. 10.) -24.

/// 二次方程式 ax^2 + bx + c = 0 の係数を受け取り、解の個数を返す
/// a > 0
let kaiNoKosuu (a : float) (b : float) (c : float) : int =
    let d = hanbetsusiki a b c
    if d = 0. then 1
    else 2

assertEq "5-5" (kaiNoKosuu 2. 3. -1.) 2
assertEq "5-5" (kaiNoKosuu 1. -6. 9.) 1
assertEq "5-5" (kaiNoKosuu 15. 24. 10.) 2

/// 二次方程式 ax^2 + bx + c = 0 の係数を受け取り、虚数解をもつか判定する
/// a > 0
let kyosuukai (a : float) (b : float) (c : float) : bool =
    let d = hanbetsusiki a b c
    if d < 0. then true
    else false

assertEq "5-6" (kyosuukai 2. 3. -1.) false
assertEq "5-6" (kyosuukai 1. -6. 9.) false
assertEq "5-6" (kyosuukai 15. 24. 10.) true

[<Measure>] type m
[<Measure>] type kg
let bmi (height : float<m>) (weight : float<kg>) : float<kg/m^2> = weight / (height * height)

/// 身長と体重を受け取り、bmi指数を計算して体型を返す
let taikei (height : float<m>) (weight : float<kg>) : string =
    let b = bmi height weight
    if b < 18.5<kg/m^2> then "痩せ"
    else if 18.5<kg/m^2> <= b && b < 25.<kg/m^2> then "標準"
    else if 25.<kg/m^2> <= b && b < 30.<kg/m^2> then "肥満"
    else "高度肥満"

assertEq "5-7" (taikei 1.6<m> 56.<kg>) "標準"
assertEq "5-7" (taikei 1.46<m> 38.<kg>) "痩せ"
assertEq "5-7" (taikei 1.86<m> 94.<kg>) "肥満"
