namespace Test

open MetroNetwork.Eki
open MetroNetwork.Ekikan
open MetroNetwork.Global
open MetroNetwork.Core

module CoreTest =

    (* 駅の例 *)
    let eki1 = {namae="池袋"; saitanKyori = inf; temaeList = []}
    let eki2 = {namae="新大塚"; saitanKyori = 1.2<km>; temaeList = ["新大塚"; "茗荷谷"]}
    let eki3 = {namae="茗荷谷"; saitanKyori = 0.<km>; temaeList = ["茗荷谷"]}
    let eki4 = {namae="後楽園"; saitanKyori = inf; temaeList = []}

    let ekikan1 = {kiten="池袋"; shuten="新大塚"; keiyu="丸ノ内線"; kyori=1.8<km>; jikan=3<minute>}
    let ekikan2 = {kiten="新大塚"; shuten="茗荷谷"; keiyu="丸ノ内線"; kyori=1.2<km>; jikan=2<minute>}
    let ekikan3 = {kiten="茗荷谷"; shuten="後楽園"; keiyu="丸ノ内線"; kyori=1.8<km>; jikan=2<minute>}

    let tree1 =
        Node (Empty, "新大塚", [("池袋", 1.8<km>)],
            Node (Empty, "池袋", [("新大塚", 1.8<km>)], Empty))
    let tree2 =
        Node (Empty, "新大塚", [("茗荷谷", 1.2<km>); ("池袋", 1.8<km>)],
            Node (Empty, "池袋", [("新大塚", 1.8<km>)],
                Node (Empty, "茗荷谷", [("新大塚", 1.2<km>)], Empty)))
    let tree3 =
        Node (Node (Empty, "後楽園", [("茗荷谷", 1.8<km>)], Empty),
            "新大塚", [("茗荷谷", 1.2<km>); ("池袋", 1.8<km>)],
            Node (Empty,
                "池袋", [("新大塚", 1.8<km>)],
                    Node (Empty, "茗荷谷", [("後楽園", 1.8<km>); ("新大塚", 1.2<km>)], Empty)))

    (* 駅リストの例 *)
    let lst = [eki1; eki2; eki3; eki4]

    let private romajiToKanjiTest (acc: bool) : bool =
        let acc = acc && assertEq "romajiToKanji 1" (romajiToKanji globalEkimeiList "myogadani") "茗荷谷"
        let acc = acc && assertEq "romajiToKanji 1" (romajiToKanji globalEkimeiList "shibuya") "渋谷"
        let acc = acc && assertEq "romajiToKanji 1" (romajiToKanji globalEkimeiList "otemachi") "大手町"
        acc

    let private assocTest (acc: bool) : bool =
        // let acc = acc && assertEq "assoc 1" (assoc "後楽園" []) inf
        let acc = acc && assertEq "assoc 2" (assoc "後楽園" [("新大塚", 1.2<km>); ("後楽園", 1.8<km>)]) 1.8<km>
        // let acc = acc && assertEq "assoc 3" (assoc "池袋" [("新大塚", 1.2<km>); ("後楽園", 1.8<km>)]) inf
        acc

    let private insertEkikanTest (acc: bool) : bool =
        let acc = acc && assertEq "insertEkikan 1" (insertEkikan Empty ekikan1) tree1
        let acc = acc && assertEq "insertEkikan 2" (insertEkikan tree1 ekikan2) tree2
        let acc = acc && assertEq "insertEkikan 3" (insertEkikan tree2 ekikan3) tree3
        acc

    let private insertsEkikanTest (acc: bool) : bool =
        let acc = acc && assertEq "insertsEkikan 1" (insertsEkikan Empty [ekikan1; ekikan2; ekikan3]) tree3
        acc

    let private getEkikanKyoriTest (acc: bool) : bool =
        let ekikanTree = insertsEkikan Empty globalEkikanList
        let acc = acc && assertEq "getEkikanKyori 1" (getEkikanKyori ekikanTree "茗荷谷" "新大塚") 1.2<km>
        // let acc = acc && assertEq "getEkikanKyori 2" (getEkikanKyori ekikanTree "茗荷谷" "池袋") inf
        let acc = acc && assertEq "getEkikanKyori 3" (getEkikanKyori ekikanTree "東京" "大手町") 0.6<km>
        acc

    let private saitanWoBunriTest (acc: bool) : bool =
        let acc = acc && assertEq "saitanWoBunri 1" (saitanWoBunri eki1 [eki2; eki3; eki4]) (eki3, [eki4; eki2; eki1])
        acc

    let private dijkstraMainTest (acc: bool) : bool =
        let acc = acc && assertEq "dijkstraMain 1" (dijkstraMain [] Empty) []
        let sol = [
            {namae = "茗荷谷"; saitanKyori = 0.<km>; temaeList = ["茗荷谷"]};
            {namae = "新大塚"; saitanKyori = 1.2<km>; temaeList = ["新大塚"; "茗荷谷"]};
            {namae = "後楽園"; saitanKyori = 1.8<km>; temaeList = ["後楽園"; "茗荷谷"]};
            {namae = "池袋"; saitanKyori = 3.<km>; temaeList = ["池袋"; "新大塚"; "茗荷谷"]}
        ]
        let ekikanTree = insertsEkikan Empty globalEkikanList
        let acc = acc && assertEq "dijkstraMain 2" (dijkstraMain lst ekikanTree) sol
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
        |> assocTest
        |> insertEkikanTest
        |> insertsEkikanTest
        |> getEkikanKyoriTest
        |> saitanWoBunriTest
        |> dijkstraMainTest
        |> dijkstraTest