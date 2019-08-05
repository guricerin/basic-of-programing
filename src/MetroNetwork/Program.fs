open System
open MetroNetwork.Ekimei
open MetroNetwork.Ekikan
open MetroNetwork.Metro

[<EntryPoint>]
let main argv =
    let eki = {
        kanji = "茗荷谷"
        kana = "みょうがだに"
        romaji = "myogadani"
        shozoku = "丸ノ内線"
    }
    printfn "%s" (hyoji eki)
    let s = romajiToKanji globalEkimeiList "boke"
    printfn "%s" s
    let s = kyoriWoHyoji "akasaka" "kokkaigijidomae"
    printfn "%s" s
    let s = kyoriWoHyoji "akihabara" "hoge"
    printfn "%s" s
    let s = kyoriWoHyoji "akihabara" "kasumigaseki"
    printfn "%s" s
    0 // return an integer exit code
