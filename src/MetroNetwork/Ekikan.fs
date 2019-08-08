namespace MetroNetwork

module Ekikan =

    /// 駅と駅の接続情報を格納する
    type Ekikan = {
        kiten : string // 起点の駅名
        shuten : string // 終点の駅名
        keiyu : string // 経由する路線名
        kyori : float<km> // 2点間の距離
        jikan : int<minute> // 所要時間
    }

    type EkikanTree =
        | Empty
        | Node of EkikanTree * string * (string * float<km>) list * EkikanTree