namespace MetroNetwork

module Ekimei =

    /// 駅名の情報を格納する
    type Ekimei = {
        kanji : string // 漢字の駅名
        kana : string // ひらがなの駅名
        romaji : string // ローマ字の駅名
        shozoku : string // 路線名
    }

    /// 「路線名, 駅名（かな）」
    let hyoji (ekimei: Ekimei) =
        ekimei.shozoku + ", " + ekimei.kanji + "（" + ekimei.kana + "）"