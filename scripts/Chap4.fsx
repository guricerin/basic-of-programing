#load "Util.fsx"

let baitoKyuyo year time =
    (850 + (year * 100)) * time

solve "4-1" (baitoKyuyo 3 40)


let jikoshokai name = "I'm " + name
solve "4-1" (jikoshokai "guricerin")


[<Measure>] type m

/// 引数や戻り値には型を明示できる
let hyojunTaiju (height : float<m>) = height * height * 22.

solve "4-3" (hyojunTaiju 1.6<m>)


[<Measure>] type kg

let bmi (height : float<m>) (weight : float<kg>) = weight / (height * height)

solve "4-4" (bmi 1.6<m> 56.32<kg>)


/// 鶴の数を与えたら、足の本数を返す
/// (3つのスラッシュでDocコメントになる)
let tsuruNoAshi (tsuru : int) : int = tsuru * 2
assertEq "4-6-1" (tsuruNoAshi 0) 0
assertEq "4-6-2" (tsuruNoAshi 101) 202
assertEq "4-6-3" (tsuruNoAshi 9999) 19998

/// 亀の数を与えたら、足の本数を返す
let kameNoAshi (kame : int) : int = kame * 4
assertEq "4-6-4" (kameNoAshi 0) 0
assertEq "4-6-5" (kameNoAshi 101) 404
assertEq "4-6-6" (kameNoAshi 9999) 39996

/// 鶴と亀の数を与えたら、足の本数の合計を返す
let tsuruKameNoAsi (tsuru : int) (kame : int) : int = tsuruNoAshi tsuru + kameNoAshi kame
assertEq "4-7-1" (tsuruKameNoAsi 0 0) 0
assertEq "4-7-1" (tsuruKameNoAsi 1 1) 6
assertEq "4-7-1" (tsuruKameNoAsi 10 20) 100

/// 鶴と亀の数の合計と足の数の合計を与えたら、鶴の数を返す
let tsuruKame (tsuruToKame : int) (legs : int) : int = 2 * tsuruToKame - legs / 2
assertEq "4-8-1" (tsuruKame 0 0) 0
assertEq "4-8-1" (tsuruKame 1 4) 0
assertEq "4-8-1" (tsuruKame 1 2) 1
assertEq "4-8-1" (tsuruKame 5 16) 2
