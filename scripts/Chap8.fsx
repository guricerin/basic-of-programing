#load "Util.fsx"

type Book = { title : string; author : string; publisher : string; price : int; isbn : string; }

let book1 = { title = "敵は海賊・不敵な休暇"; author = "神林長平"; publisher = "早川書房"; price = 580; isbn = "hoge"}

solve "8-1" book1
let book2 = {
    title = "キノの旅"
    author = "時雨沢恵一"
    publisher = "電撃文庫"
    price = 980
    isbn = "fuga"
}
solve "8-1" book2
let book3 = {
    title = "プログラミングの基礎"
    author = "浅井健一"
    publisher = "サイエンス社"
    price = 2300
    isbn = "piyo"
}
solve "8-1" book3


type Okozukai = {
    item : string
    price : int
    where : string
    day : string
}

let okozukai1 = {
    item = "プログラミングの基礎"
    price = 2300
    where = "Amazon"
    day = "2018/08/01"
}
solve "8-2" okozukai1
let okozukai2 = {
    item = "B'z The Best Treasure"
    price = 100
    where = "Book Off"
    day = "2006/07/22"
}
solve "8-2" okozukai2
let okozukai3 = {
    item = "Mac Book Air"
    price = 198000
    where = "ヨドバシ"
    day = "2017/09/10"
}
solve "8-2" okozukai3


#load "Person.fsx"

let matsumoto = {
    name = "松本孝弘"
    height = 165.0
    weight = 0. // 知らん
    birth = (3, 27)
    bloodType = B // 知らん
}
solve "8-3" matsumoto

let inaba = {
    name = "稲葉浩志"
    height = 173.0
    weight = 0. // 知らん
    birth = (9, 23)
    bloodType = BloodType.A // 知らん
}
solve "8-3" inaba


/// Person型の値を受け取り、「xさんの血液型はy型です」という文字列を返す
let ketsuekiHyoji (person: Person) : string =
    let blood =
        match person.bloodType with
        | A -> "A"
        | B -> "B"
        | O -> "O"
        | AB -> "AB"
    person.name + "さんの血液型は" + blood + "型です"

solve "8-4" (ketsuekiHyoji matsumoto)
solve "8-4" (ketsuekiHyoji inaba)