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

/// 17-3
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

type Tree =
    | Empty
    | Leaf of int
    | Node of Tree * int * Tree

let t1 = Empty
let t2 = Leaf (3)
let t3 = Node (t1, 4, t2)
let t4 = Node (t2, 5, t3)

/// Tree型の木を受け取り、節や葉に入っている値を全て2倍にした木を返す
let rec treeDouble = function
| Empty -> Empty
| Leaf (n) -> Leaf (n * 2)
| Node (left, n, right) -> Node (treeDouble left, n * 2, treeDouble right)

assertEq "17-5" (treeDouble t1) Empty
assertEq "17-5" (treeDouble t2) (Leaf (6))
assertEq "17-5" (treeDouble t3) (Node (Empty, 8, Leaf (6)))
assertEq "17-5" (treeDouble t4) (Node (Leaf (6), 10, Node (Empty, 8, Leaf (6))))

/// int -> int型の関数fとTree型の木を受け取り、節や葉に入っている値全てにfを適用した木を返す
let rec treeMap f = function
| Empty -> Empty
| Leaf (n) -> Leaf (f n)
| Node (left, n, right) -> Node (treeMap f left, f n, treeMap f right)

assertEq "17-6" (treeMap id t1) Empty
assertEq "17-6" (treeMap (fun x -> x + 1) t2) (Leaf (4))
assertEq "17-6" (treeMap (fun x -> x * 3) t3) (Node (Empty, 12, Leaf (9)))
assertEq "17-6" (treeMap (fun x -> x * 2) t4) (Node (Leaf (6), 10, Node (Empty, 8, Leaf (6))))

/// Tree型の木を受け取り、節と葉が合計いくつあるかを返す
let treeLength (tree: Tree) : int =
    let rec loop acc = function
    | Empty -> acc
    | Leaf (_) -> acc + 1
    | Node (left, _, right) -> loop acc left + 1 + loop acc right
    loop 0 tree

assertEq "17-7" (treeLength t1) 0
assertEq "17-7" (treeLength t2) 1
assertEq "17-7" (treeLength t3) 2
assertEq "17-7" (treeLength t4) 4

/// Tree型の木を受け取り、木の深さを返す
let treeDepth tree =
    let rec loop acc = function
    | Empty -> acc
    | Leaf (_) -> acc
    | Node (left, _, right) -> max (loop (acc + 1) left) (loop (acc + 1) right)
    loop 0 tree

assertEq "17-8" (treeDepth t1) 0
assertEq "17-8" (treeDepth t2) 0
assertEq "17-8" (treeDepth t3) 1
assertEq "17-8" (treeDepth t4) 2