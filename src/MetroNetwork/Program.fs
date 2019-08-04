open System
open MetroNetwork.Ekimei
open MetroNetwork.Ekikan

[<EntryPoint>]
let main argv =
    let eki = {
        kanji = "茗荷谷"
        kana = "みょうがだに"
        romaji = "myogadani"
        shozoku = "丸ノ内線"
    }
    printfn "%s" (hyoji eki)
    0 // return an integer exit code
