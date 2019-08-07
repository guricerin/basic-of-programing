#load "Util.fsx"

type Nengou =
    | Meiji of int
    | Taisho of int
    | Showa of int
    | Heisei of int
    | Reiwa of int

let toSeireki = function
    | Meiji (n) -> n + 1867
    | Taisho (n) -> n + 1911
    | Showa (n) -> n + 1925
    | Heisei (n) -> n + 1988
    | Reiwa (n) -> n + 2019

/// 誕生年と現在の年をNengou型の値として受け取り、年齢を返す
let nenrei (birth: Nengou) (current: Nengou) : int =
    toSeireki current - toSeireki birth

assertEq "17-1" (nenrei (Meiji (0)) (Reiwa (0))) 152
assertEq "17-1" (nenrei (Heisei (5)) (Reiwa (0))) 26

/// 17-2
type Year =
    | January of int
    | Februaly of int
    | March of int
    | April of int
    | May of int
    | June of int
    | July of int
    | August of int
    | September of int
    | October of int
    | November of int
    | Decenber of int

type Seiza =
    | Ohitsuji
    | Oushi
    | Hutago
    | Kani
    | Shishi
    | Otome
    | Tenbin
    | Sasori
    | Ite
    | Yagi
    | Mizugame
    | Uo

/// Year型の値を受け取り、Seiza型の値を返す
let seiza (year: Year) : Seiza =
    match year with
    | January (day) -> if day <= 19 then Yagi else Mizugame
    | Februaly (day) -> if day <= 18 then Mizugame else Uo
    | March (day) -> if day <= 20 then Uo else Ohitsuji
    | April (day) -> if day <= 19 then Ohitsuji else Oushi
    | May (day) -> if day <= 20 then Oushi else Hutago
    | June (day) -> if day <= 21 then Hutago else Kani
    | July (day) -> if day <= 22 then Kani else Shishi
    | August (day) -> if day <= 22 then Shishi else Otome
    | September (day) -> if day <= 22 then Otome else Tenbin
    | October (day) -> if day <= 23 then Tenbin else Sasori
    | November (day) -> if day <= 22 then Sasori else Ite
    | Decenber (day) -> if day <= 22 then Ite else Yagi

assertEq "17-4" (seiza (June (11))) Hutago
assertEq "17-4" (seiza (June (30))) Kani
assertEq "17-4" (seiza (September (17))) Otome
assertEq "17-4" (seiza (October (7))) Tenbin