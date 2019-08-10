namespace MetroNetwork

module RedBlackTree =

    type Color = Red | Black

    /// 赤黒木
    /// 親の頂点の値は左の子よりも大きく、右の子よりも小さい。
    /// 木の根から空の葉に至る全てのパスにおいて、黒い頂点の数は同じ。
    /// 赤い頂点の子はどちらも必ず黒。親は必ず黒。
    type RbTree<'a, 'b> =
        | Empty
        | Node of RbTree<'a, 'b> * 'a * 'b * Color * RbTree<'a, 'b>

    /// 赤黒木の赤の連続を解消する
    let private balance (tree: RbTree<'a, 'b>) : RbTree<'a, 'b> =
        match tree with
        | Node (Node (Node (a, xa, xb, Red, b), ya, yb, Red, c), za, zb, Black, d)
        | Node (Node (a, xa, xb, Red, Node (b, ya, yb, Red, c)), za, zb, Black, d)
        | Node (a, xa, xb, Black, Node (Node (b, ya, yb, Red, c), za, zb, Red, d))
        | Node (a, xa, xb, Black, Node (b, ya, yb, Red, Node (c, za, zb, Red, d)))
            -> Node (Node (a, xa, xb, Black, b), ya, yb, Red, Node (c, za, zb, Black, d))
        | _
            -> tree

    /// 赤黒木とキーと値を受け取り、それを挿入した赤黒木を返す
    let insert tree k v =
        let rec insertCore = function
        | Empty -> Node (Empty, k, v, Red, Empty)
        | Node (left, key, value, color, right) ->
            if k = key
            then Node (left, k, v, color, right)
            else if k < key
            then balance (Node (insertCore left, key, value, color, right))
            else balance (Node (left, key, value, color, insertCore right))

        match insertCore tree with
        | Empty -> failwith "起こりえない例外"
        | Node (left, key, value, color, right) ->
            Node (left, key, value, Black, right) // このノードは根なので黒にする

    exception NotFoundException

    /// treeの中のキーkに対応する値を探して返す
    /// 見つからなければ例外を起こす
    let rec search tree k =
        match tree with
        | Empty -> raise NotFoundException
        | Node (left, key, value, color, right) ->
            if k = key
            then value
            else if k < key
            then search left k
            else search right k

    let RbTreeTest () =
        let rbtree1 =
            Node (Node (Node (Empty, 10, "x", Red, Empty), 13, "y", Red, Empty), 15, "z", Black, Empty)
        let rbtree2 =
            Node (Node (Empty, 10, "x", Red, Node (Empty, 13, "y", Red, Empty)), 15, "z", Black, Empty)
        let rbtree3 =
            Node (Empty, 10, "x", Black, Node (Node (Empty, 13, "y", Red, Empty), 15, "z", Red, Empty))
        let rbtree4 =
            Node (Empty, 10, "x", Black, Node (Empty, 13, "y", Red, Node (Empty, 15, "z", Red, Empty)))
        let rbtree5 =
            Node (Node (Empty, 10, "x", Black, Empty), 13, "y", Red, Node (Empty, 15, "z", Black, Empty))
        let rbtree6 = Empty
        let test1 = balance rbtree1 = rbtree5
        assert test1
        let test2 = balance rbtree2 = rbtree5
        assert test2
        let test3 = balance rbtree3 = rbtree5
        assert test3
        let test4 = balance rbtree4 = rbtree5
        assert test4
        let test5 = balance rbtree6 = rbtree6
        assert test5
