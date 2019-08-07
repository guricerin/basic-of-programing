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

    /// Ekimei型のリストと起点（漢字）を受け取り、
    /// 起点のみsaitanKyoriが0.、temaeListが始点の駅名のみからなるリストになっているEki型のリストを返す
    let makeInitialEkiList (ekimeiLst: Ekimei list) (start: string) =
        let f ekimei =
            if ekimei.kanji = start
            then {namae = start; saitanKyori = 0.<km>; temaeList = [start]}
            else {namae = ekimei.kanji; saitanKyori = inf; temaeList = []}
        List.map f ekimeiLst

    /// Eki型のリストを受け取り、「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組みを返す
    let saitanWoBunri (ekiList: Eki list) : Eki * Eki list =
        let saitan lst =
            let f x y = if x.saitanKyori < y.saitanKyori then x else y
            List.fold f {namae=""; saitanKyori = inf; temaeList = []} lst
        let saitanEki = saitan ekiList
        let bunri =
            let f x = saitanEki <> x
            List.filter f ekiList
        (saitanEki, bunri)

    /// 直前に最短距離が確定した駅p（Eki型）と未確定の駅のリストv（Eki list型）を受け取り、
    /// 必要な更新処理を行なった後の未確定の駅のリストを返す
    let koushin (p: Eki) (v: Eki list) : Eki list =
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
        let f = koushin1 p
        List.map f v