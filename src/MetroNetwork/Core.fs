namespace MetroNetwork

open MetroNetwork.Ekimei
open MetroNetwork.Ekikan
open MetroNetwork.RedBlackTree
// open MetroNetwork.Heap
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

    /// 未確定駅の情報を表すヒープ
    /// 最短距離、駅名、手前リスト
    type EkiHeap = Heap.Heap<float<km>, Eki>

    /// Ekimei listと起点の駅名を受け取り、EkiHeapを作成する
    /// 起点のみ、saitanKyoriを0.0, temaeListを["起点の駅名"]にする
    let makeInitialEkiHeap (ekimeiList: Ekimei list) (kiten: string): EkiHeap =
        let size = List.length ekimeiList
        let init = {namae = ""; saitanKyori = inf; temaeList = []}
        let heap = Heap.create size 0.<km> init
        let f (ekimei: Ekimei) =
            if ekimei.kanji = kiten
            then do
                Heap.insert heap 0.<km> {namae = ekimei.kanji; saitanKyori = 0.<km>; temaeList = [ekimei.kanji]}
                |> ignore
            else do
                Heap.insert heap inf {namae = ekimei.kanji; saitanKyori = inf; temaeList = []}
                |> ignore

        List.iter f ekimeiList
        heap

    /// Eki listからEkiHeapを作成
    let makeEkiHeap (ekiList: Eki list) : EkiHeap =
        let rec insert heap = function
        | [] -> heap
        | first :: rest ->
            let (_, heap) = Heap.insert heap first.saitanKyori first
            insert heap rest

        let size = List.length ekiList
        let heap = Heap.create size 0.<km> {namae = ""; saitanKyori = inf; temaeList = []}
        insert heap ekiList

    /// EkiHeapからEki listを作成
    let rec makeEkiList (ekiHeap: EkiHeap) : Eki list =
        if Heap.length ekiHeap = 0
        then []
        else
            let ((kyori, eki), restHeap) = Heap.splitTop ekiHeap
            eki :: makeEkiList restHeap

    /// 直前に最短距離が確定した駅p（Eki型）と未確定の駅のヒープv（EkiHeap型）を受け取り、
    /// 必要な更新処理を行なった後の未確定の駅のヒープを返す
    let koushin (p: Eki) (v: EkiHeap) (ekikanTree: EkikanTree): EkiHeap =
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
        /// todo:EkiListを介するのではなく、EkiHeapを直接扱うよう修正
        makeEkiList v |> List.map f |> makeEkiHeap

    /// Eki型の（未確定の）ヒープとEkikan型のリストを受け取り、
    /// 各駅について最短距離と最短経路が正しく格納されたEki型のリストを返す
    let rec dijkstraMain (ekiHeap: EkiHeap) (ekikanTree: EkikanTree) : Eki list =
        if Heap.length ekiHeap = 0
        then []
        else
            let ((kyori, eki), restHeap) = Heap.splitTop ekiHeap
            let ekiHeap2 = koushin eki restHeap ekikanTree
            eki :: dijkstraMain ekiHeap2 ekikanTree

    /// 駅名とEki型のリストを受け取り、該当するEki型のレコードを返す
    let private find ekimei (ekiList: Eki list) =
        let res = List.filter (fun eki -> eki.namae = ekimei) ekiList
        match res with
        | [] -> {namae = ""; saitanKyori = inf; temaeList = []}
        | first :: _ -> first

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
        let ekikanTree = makeEkikanTree globalEkikanList
        let ekiHeap = makeInitialEkiHeap ekimeiList start
        let ekiList = dijkstraMain ekiHeap ekikanTree

        find goal ekiList

    /// Eki型の値を受け取り、最短路問題の結果を綺麗に表示
    let printEki (eki: Eki) : unit =
        let mutable msg = ""
        let rec loop = function
        | [] -> failwith "起こり得ない"
        | [last] -> do
            msg <- sprintf "%s%s" msg last
        | first :: rest -> do
            msg <- sprintf "%s%s -> " msg first
            loop rest

        do
            msg <- sprintf "%s駅への最短経路:\n" eki.namae
            loop (List.rev eki.temaeList)
            printfn "%s (%.2f km)" msg eki.saitanKyori