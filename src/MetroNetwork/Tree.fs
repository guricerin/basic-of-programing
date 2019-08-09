// namespace MetroNetwork

[<RequireQualifiedAccessAttribute>]
module Tree

    /// 2分探索木
    type Tree<'a, 'b> =
        | Empty
        | Node of Tree<'a, 'b> * 'a * 'b * Tree<'a, 'b> // キーが'a型、値が'b型

    type T<'a, 'b> = Tree<'a, 'b>

    let empty = Empty

    /// treeにキーがkで値がvを挿入した木を返す
    let rec insert tree k v =
        match tree with
        | Empty -> Node (Empty, k, v, Empty)
        | Node (left, key, value, right) ->
            if k = key
            then Node (left, k, v, right)
            else if k < key
            then Node (insert left k v, key, value, right)
            else Node (left, key, value, insert right k v)

    exception NotFoundException

    /// treeの中のキーkに対応する値を探して返す
    /// 見つからなければ例外を起こす
    let rec search tree k =
        match tree with
        | Empty -> raise NotFoundException
        | Node (left, key, value, right) ->
            if k = key
            then value
            else if k < key
            then search left k
            else search right k