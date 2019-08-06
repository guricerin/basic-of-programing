namespace MetroNetwork

open MetroNetwork.Ekimei
open MetroNetwork.Metro

module Eki =

    /// メトロネットワーク最短路問題での頂点
    type Eki = {
        namae : string // 漢字の駅名
        saitanKyori : float<km> // 最短距離
        temaeList : string list // 最短距離をどう辿ってきたかを逆順に保持するリスト
    }

    // infinityに測定単位をそのまま付与できないっぽいので、代わりにこいつを使う
    let inf: float<km> = LanguagePrimitives.FloatWithMeasure infinity

    /// Ekimei型のリストを受け取り、その駅名を使ってEki型のリストを作る
    let rec makeEkiList (lst: Ekimei list) : Eki list =
        match lst with
        | [] -> []
        | first :: rest ->
            let eki = {namae = first.kanji; saitanKyori = inf; temaeList = []}
            eki :: makeEkiList rest

    /// 頂点の集合
    let globalEkiList = makeEkiList MetroNetwork.Metro.globalEkimeiList

    /// Eki型のリストと起点（漢字）を受け取り、
    /// 起点のみsaitanKyoriが0.、temaeListが始点の駅名のみからなるリストになっているEki型のリストを返す
    let rec shokika (lst: Eki list) (start: string) : Eki list =
        match lst with
        | [] -> []
        | first :: rest ->
            let s =
                if first.namae = start then {first with saitanKyori = 0.<km>; temaeList = [start]}
                else first
            s :: shokika rest start

    /// 直前に最短距離が確定した駅p（Eki型）と未確定の駅q（Eki型）を受け取り、
    /// pとqが直接繋がっていたらqの最短距離と手前リストを「最短距離がp経由の方が小さくなっていたら」更新したもの、
    /// 繋がっていなかったらqをそのまま返す
    let koushin1 (p: Eki) (q: Eki) : Eki =
        match getEkikanKyori globalEkikanList p.namae q.namae with
        | None -> q
        | Some (dist) ->
            let updatedQ =
                let newDist = dist + p.saitanKyori
                if newDist < q.saitanKyori
                then {q with saitanKyori = newDist; temaeList = q.namae :: p.temaeList}
                else q
            updatedQ

    /// 直前に最短距離が確定した駅p（Eki型）と未確定の駅のリストv（Eki list型）を受け取り、
    /// 必要な更新処理を行なった後の未確定の駅のリストを返す
    let koushin (p: Eki) (v: Eki list) : Eki list =
        let f = koushin1 p
        List.map f v