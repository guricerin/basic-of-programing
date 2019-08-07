namespace MetroNetwork

module Eki =

    /// メトロネットワーク最短路問題での頂点
    type Eki = {
        namae : string // 漢字の駅名
        saitanKyori : float<km> // 最短距離
        temaeList : string list // 最短距離をどう辿ってきたかを逆順に保持するリスト
    }
