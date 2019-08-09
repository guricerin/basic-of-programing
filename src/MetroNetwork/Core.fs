namespace MetroNetwork

open MetroNetwork.Ekimei
open MetroNetwork.Ekikan
open MetroNetwork.RedBlackTree
open MetroNetwork.Eki
open MetroNetwork.Global

module Core =

    exception NoSuchStationException of string // ローマ字の駅名

    /// ローマ字の駅名と駅名リストを受け取り、その駅の漢字表記を文字列で返す
    /// 見つからない場合は空文字列を返す
    let rec romajiToKanji (lst: Ekimei list) (romaji: string) : string =
        match lst with
        | [] -> raise (NoSuchStationException romaji)
        | first :: rest ->
            if first.romaji = romaji then first.kanji
            else romajiToKanji rest romaji

    /// ローマ字の駅名を2つ受け取り、直接繋がっている場合は「x駅からy駅まではzkmです」
    /// 繋がっていない場合は「x駅とy駅は繋がっていません」
    /// 入力されたローマ字の駅名が存在しない場合は「xという駅は存在しません」という文字列を返す
    let kyoriWoHyoji (ekimei1: string) (ekimei2: string) : string =
        let f = romajiToKanji globalEkimeiList
        let kanjiEkimei1 = f ekimei1
        let kanjiEkimei2 = f ekimei2
        if kanjiEkimei1 = "" then ekimei1 + "という駅は存在しません"
        else if kanjiEkimei2 = "" then ekimei2 + "という駅は存在しません"
        else
            let ekikanTree = insertsEkikan empty globalEkikanList
            match getEkikanKyori ekikanTree kanjiEkimei1 kanjiEkimei2 with
            | kyori when kyori = inf -> kanjiEkimei1 + "駅と" + kanjiEkimei2 + "駅は繋がっていません"
            | kyori -> kanjiEkimei1 + "駅から" + kanjiEkimei2 + "駅までは" + kyori.ToString() + "kmです"

    /// Ekimei型のリストを受け取り、ひらがなの順に整列して同じ駅の重複を取り除いたEkimei型のリストを返す
    /// 乗り換えを考慮するのであれば、globalEkimeiListをそのまま使う
    /// その場合、路線が別で駅名が同じ駅については別の頂点として扱い、その間を「乗り換え」という辺で繋がっていると解釈する
    let seiretsu (lst: Ekimei list) : Ekimei list =
        let f x y = x.kana < y.kana
        let rec qsort lst =
            match lst with
            | [] -> []
            | pivot :: rest ->
                let cmp x = f x pivot
                let left, right = List.partition cmp rest
                qsort left @ [pivot] @ qsort right
        qsort lst

    /// Ekimei型のリストと起点（漢字）を受け取り、
    /// 起点のみsaitanKyoriが0.、temaeListが始点の駅名のみからなるリストになっているEki型のリストを返す
    let makeInitialEkiList (ekimeiLst: Ekimei list) (start: string) =
        let f ekimei =
            if ekimei.kanji = start
            then {namae = start; saitanKyori = 0.<km>; temaeList = [start]}
            else {namae = ekimei.kanji; saitanKyori = inf; temaeList = []}
        List.map f ekimeiLst

    /// Eki型のリストを受け取り、「最短距離最小の駅」と「最短距離最小の駅以外からなるリスト」の組みを返す
    let saitanWoBunri (eki: Eki) (ekiList: Eki list) : Eki * Eki list =
        let f (p, v) first =
            if p.saitanKyori <= first.saitanKyori
            then (p, first :: v)
            else (first, p :: v)
        List.fold f (eki, []) ekiList

    /// 直前に最短距離が確定した駅p（Eki型）と未確定の駅のリストv（Eki list型）を受け取り、
    /// 必要な更新処理を行なった後の未確定の駅のリストを返す
    let koushin (p: Eki) (v: Eki list) (ekikanTree: EkikanTree): Eki list =
        /// 直前に最短距離が確定した駅p（Eki型）と未確定の駅q（Eki型）を受け取り、
        /// pとqが直接繋がっていたらqの最短距離と手前リストを「最短距離がp経由の方が小さくなっていたら」更新したもの、
        /// 繋がっていなかったらqをそのまま返す
        let koushin1 (p: Eki) (q: Eki) : Eki =
            try
                let dist = getEkikanKyori ekikanTree p.namae q.namae
                let updatedQ =
                    let newDist = dist + p.saitanKyori
                    if newDist < q.saitanKyori
                    then {q with saitanKyori = newDist; temaeList = q.namae :: p.temaeList}
                    else q
                updatedQ
            with
            | NotFoundException -> q
        let f = koushin1 p
        List.map f v

    /// Eki型の（未確定の）リストとEkikan型のリストを受け取り、
    /// 各駅について最短距離と最短経路が正しく格納されたEki型のリストを返す
    let rec dijkstraMain (ekiList: Eki list) (ekikanTree: EkikanTree) : Eki list =
        match ekiList with
        | [] -> []
        | first :: rest ->
            let saitan, nokori = saitanWoBunri first rest
            let ekiList2 = koushin saitan nokori ekikanTree
            saitan :: dijkstraMain ekiList2 ekikanTree

    /// 始点の駅名（ローマ字）と終点の駅名（ローマ字）を受け取り、
    /// seiretsuを使ってglobalEkimeiListの重複を取り除き、
    /// romajiToKanjiを使って始点と終点の漢字表記を求め、
    /// makeInitailEkiListを使って駅のリストを作成し、
    /// dijkstraMainを使って各駅までの最短路を確定し、
    /// その中から終点の駅のレコードを返す
    let dijkstra (start: string) (goal: string) : Eki =
        let ekimeiList = seiretsu globalEkimeiList
        let start = romajiToKanji ekimeiList start
        let goal = romajiToKanji ekimeiList goal
        let ekiList = makeInitialEkiList ekimeiList start
        let ekikanTree = insertsEkikan empty globalEkikanList
        let ekiList = dijkstraMain ekiList ekikanTree

        let init = {namae = goal; saitanKyori = inf; temaeList = []}
        let f x y = if x.namae = y.namae then y else x
        List.fold f init ekiList