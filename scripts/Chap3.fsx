#load "Util.fsx"

let e = 2.7182
solve "3-1-1" (getType e)
let positive = e > 0.0
solve "3-1-2" (getType positive)
let secondsOfDay = 60 * 60 * 24
solve "3-1-3" (getType secondsOfDay)
let name = "茗荷谷"
solve "3-1-4" (getType name)

// fsxファイルでは、シャドウイング不可能？
// トップレベルではない場合、同一モジュール内でのシャドウイングは不可能らしい
// let e = 2.7182
let otherE = 2.7182
solve "3-2" (1.0 + otherE * 2.0)