namespace MetroNetwork

open RedBlackTree

module Ekikan =

    /// 駅と駅の接続情報を格納する
    type Ekikan = {
        kiten : string // 起点の駅名
        shuten : string // 終点の駅名
        keiyu : string // 経由する路線名
        kyori : float<km> // 2点間の距離
        jikan : int<minute> // 所要時間
    }

    // type EkikanTree =
    //     | Empty
    //     | Node of EkikanTree * string * (string * float<km>) list * EkikanTree
    // type EkikanTree = Tree.Tree<string, (string * float<km>) list>
    type EkikanTree = RbTree<string, (string * float<km>) list>

    let empty = Empty

    /// 受け取ったEkikan情報をEkikanTreeに挿入した木を返す
    let insertEkikan ekikanTree (ekikan: Ekikan) =
        let rec insert1 (ekikanTree: EkikanTree) kiten shuten kyori =
            let lst =
                try
                    search ekikanTree kiten
                with
                | NotFoundException -> []
            insert ekikanTree kiten ((shuten, kyori) :: lst)

        insert1 (insert1 ekikanTree ekikan.kiten ekikan.shuten ekikan.kyori) ekikan.shuten ekikan.kiten ekikan.kyori

    /// EkikanTree型の木とEkikan型のリストを受け取り、リストの中に含まれる駅間を全て挿入した木を返す
    let insertsEkikan (tree: EkikanTree) (ekikanList: Ekikan list) : EkikanTree =
        List.fold insertEkikan tree ekikanList

    /// 「駅名」と「駅名と距離の組みのリスト」を受け取り、その駅までの距離を返す
    let rec assoc (ekimei: string) (lst: (string * float<km>) list) : float<km> =
        match lst with
        | [] -> raise NotFoundException
        | first :: rest ->
            let s, k = first
            if ekimei = s then k
            else assoc ekimei rest

    /// 漢字の駅名2つとEkikanTree型の木を受け取り、2駅が直接繋がっている場合にその距離を返す
    /// 辺の重みに対応している
    let getEkikanKyori tree (ekimei1: string) (ekimei2: string) =
        assoc ekimei2 (search tree ekimei1)