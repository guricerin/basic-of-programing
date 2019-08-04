#load "Util.fsx"

/// 5教科の点数を受け取り、合計点と平均点を組みにして返す
let goukeiToHeikin (kokugo : int) (suugaku : int) (eigo : int) (rika : int) (shakai : int) : (int * float) =
    let total = kokugo + suugaku + eigo + rika + shakai
    let average = float total / 5.
    (total, average)

assertEq "7-1" (goukeiToHeikin 0 0 0 0 0) (0, 0.)
assertEq "7-1" (goukeiToHeikin 100 100 100 100 100) (500, 100.)
assertEq "7-1" (goukeiToHeikin 50 50 50 50 50) (250, 50.)

/// 名前と成績の組みを受け取り、「xさんの評価はyです」という文字列を返す
let seiseki (name : string , grade : int) : string =
    let result =
        if grade < 60 then "F"
        else if 60 <= grade && grade < 70 then "D"
        else if 70 <= grade && grade < 80 then "C"
        else if 80 <= grade && grade < 90 then "B"
        else "A"
    name + "さんの評価は" + result + "です"

assertEq "7-2" (seiseki ("ringo", 90)) "ringoさんの評価はAです"
assertEq "7-2" (seiseki ("すぬけ", 89)) "すぬけさんの評価はBです"
assertEq "7-2" (seiseki ("高橋", 59)) "高橋さんの評価はFです"

/// x座標とy座標を受け取り、x軸について対称な点の座標を返す
let taishoX (point : float * float) : (float * float) =
    let x, y = point
    (x, -y)

assertEq "7-3" (taishoX (0., 0.)) (0., 0.)
assertEq "7-3" (taishoX (17., 99.)) (17., -99.)
assertEq "7-3" (taishoX (-88., -42.)) (-88., 42.)

/// 平面座標を2つ受け取り、その中点の座標を返す
let chuten (point1 : float * float) (point2 : float * float): (float * float) =
    let x1, y1 = point1
    let x2, y2 = point2
    let x = (x1 + x2) / 2.
    let y = (y1 + y2) / 2.
    (x, y)

assertEq "7-4" (chuten (2., 3.) (4., 1.)) (3., 2.)
assertEq "7-4" (chuten (3., 1.) (2., -2.)) (2.5, -0.5)
assertEq "7-4" (chuten (-8., 11.) (-4., 5.)) (-6., 8.)