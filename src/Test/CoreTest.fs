namespace Test

open MetroNetwork.Eki
open MetroNetwork.Ekikan
open MetroNetwork.Global
open MetroNetwork.Core

module CoreTest =

    let private romajiToKanjiTest (acc: bool) : bool =
        let acc = acc && assertEq "romajiToKanji 1" (romajiToKanji globalEkimeiList "myogadani") "茗荷谷"
        let acc = acc && assertEq "romajiToKanji 1" (romajiToKanji globalEkimeiList "shibuya") "渋谷"
        let acc = acc && assertEq "romajiToKanji 1" (romajiToKanji globalEkimeiList "otemachi") "大手町"
        acc

    let private dijkstraTest (acc: bool) : bool =
        let sol = {
            namae = "護国寺"; saitanKyori = 9.8<km>;
            temaeList =
                ["護国寺"; "江戸川橋"; "飯田橋"; "市ヶ谷"; "麹町"; "永田町";
                "青山一丁目"; "表参道"; "渋谷"]
        }
        let acc = acc && assertEq "dijkstra 1" (dijkstra "shibuya" "gokokuji") sol
        let sol = {
            namae = "目黒";
            saitanKyori = 12.7000000000000028<km>;
            temaeList =
                ["目黒"; "白金台"; "白金高輪"; "麻布十番"; "六本木一丁目"; "溜池山王";
                "永田町"; "麹町"; "市ヶ谷"; "飯田橋"; "後楽園"; "茗荷谷"]
        }
        let acc = acc && assertEq "dijkstra 2" (dijkstra "myogadani" "meguro") sol
        acc

    let doTest (acc: bool) : bool =
        acc
        |> romajiToKanjiTest
        |> dijkstraTest