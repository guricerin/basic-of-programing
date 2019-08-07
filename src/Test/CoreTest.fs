namespace Test

open MetroNetwork.Eki
open MetroNetwork.Global
open MetroNetwork.Core

module CoreTest =

    (* 駅の例 *)
    let eki1 = {namae="池袋"; saitanKyori = inf; temaeList = []}
    let eki2 = {namae="新大塚"; saitanKyori = 1.2<km>; temaeList = ["新大塚"; "茗荷谷"]}
    let eki3 = {namae="茗荷谷"; saitanKyori = 0.<km>; temaeList = ["茗荷谷"]}
    let eki4 = {namae="後楽園"; saitanKyori = inf; temaeList = []}

    (* 駅リストの例 *)
    let lst = [eki1; eki2; eki3; eki4]


    let private dijkstraMainTest (acc: bool) : bool =
        let acc = acc && assertEq "dijkstraMain 1" (dijkstraMain [] globalEkikanList) []
        let sol = [
            {namae = "茗荷谷"; saitanKyori = 0.<km>; temaeList = ["茗荷谷"]};
            {namae = "新大塚"; saitanKyori = 1.2<km>; temaeList = ["新大塚"; "茗荷谷"]};
            {namae = "後楽園"; saitanKyori = 1.8<km>; temaeList = ["後楽園"; "茗荷谷"]};
            {namae = "池袋"; saitanKyori = 3.<km>; temaeList = ["池袋"; "新大塚"; "茗荷谷"]}
        ]
        let acc = acc && assertEq "dijkstraMain 2" (dijkstraMain lst globalEkikanList) sol
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
        |> dijkstraMainTest
        |> dijkstraTest