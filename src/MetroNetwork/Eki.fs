namespace MetroNetwork

open MetroNetwork.Ekimei

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