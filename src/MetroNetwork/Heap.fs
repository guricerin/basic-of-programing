namespace MetroNetwork

module Heap =

    exception HeapFullException

    exception HeapEmptyException

    exception HeapNotFoundException

    /// ヒープの添字
    type Idx = int ref

    /// ヒープ
    /// 各頂点の値は、どちらの子の頂点の値よりも小さい（子同士の大小関係は問わない） -> 最小値は常に根に格納される。
    /// 配列に木を埋め込む実装。現在見ている頂点の添字をiとすると、子の添字は 2i+1, 2i+2、親の添字は (i-1)/2 となる。
    /// 最小値をmとめる値が'a型、その他の付加情報が'b型
    /// 最初のint refはヒープに入っている要素数
    type Heap<'a, 'b> = int ref * (Idx * 'a * 'b) array

    /// 最大capacity個の要素を格納できる新たなヒープを返す
    let create capacity iniValue iniInfo : Heap<'a, 'b> =
        let exampleIdx = ref (-1)
        (ref 0, Array.create capacity (exampleIdx, iniValue, iniInfo))

    /// currentIdxとparentIdxの要素を入れ替える
    let private swap (arr: (Idx * 'a * 'b) array) currentIdx parentIdx =
        let (childIdxRef, childValue, childInfo) as childEntry = arr.[currentIdx]
        let (parentIdxRef, parentValue, parentInfo) as parentEntry = arr.[parentIdx]
        do
            arr.[currentIdx] <- parentEntry
            arr.[parentIdx] <- childEntry
            childIdxRef := parentIdx // idxも付け替える
            parentIdxRef := currentIdx

    /// 下方向に向かってヒープの条件を満たすように要素の入れ替えを行う
    let rec private adjustChild size (arr: (Idx * 'a * 'b) array) currentIdx =
        if currentIdx >= size
        then ()
        else
            let (_, currentValue, _) = arr.[currentIdx]
            let child1Idx = 2 * currentIdx + 1
            let child2Idx = child1Idx + 1
            if child1Idx >= size
            then ()
            else
                let (_, child1Value, _) = arr.[child1Idx]
                if child2Idx >= size
                then
                    if currentValue <= child1Value
                    then () // 親の値がが子の値よりも小さいことが確定したら下降を停止
                    else do // 親の値が子の値よりも小さくなるようにする
                        swap arr currentIdx child1Idx
                        adjustChild size arr child1Idx
                else
                    let (_, child2Value, _) = arr.[child2Idx]
                    if currentValue <= child1Value && currentValue <= child2Value
                    then () // 親の値が子の値よりも小さいことが確定したら下降を停止
                    else if child1Value < child2Value // 値がより小さい方に下降する
                    then do
                        swap arr currentIdx child1Idx
                        adjustChild size arr child1Idx
                    else do
                        swap arr currentIdx child2Idx
                        adjustChild size arr child2Idx

    /// 上方向に向かってヒープの条件を満たすように要素の入れ替えを行う
    let rec private adjustParent (arr: (Idx * 'a * 'b) array) currentIdx =
        if currentIdx = 0
        then ()
        else
            let (_, childValue, _) = arr.[currentIdx]
            let parentIdx = (currentIdx - 1) / 2
            let (_, parentValue, _) = arr.[parentIdx]
            if childValue < parentValue
            then do
                swap arr currentIdx parentIdx
                adjustParent arr parentIdx
            else ()

    /// ヒープにデータを挿入する
    /// ヒープは破壊的に書き換わる
    let insert heap value info : Idx * Heap<'a, 'b> =
        let (sizeRef, array) = heap
        if !sizeRef >= Array.length array
        then raise HeapFullException
        else
            let idx = ref !sizeRef
            array.[!sizeRef] <- (idx, value, info)
            adjustParent array !sizeRef
            sizeRef := !sizeRef + 1
            (idx, (sizeRef, array))

    /// ヒープの!idxRef番目の要素を返す
    /// idxRefが無効ならば例外を投げる
    let get (heap: Heap<'a, 'b>) idxRef =
        let (sizeRef, arr) = heap
        if 0 <= !idxRef && !idxRef < !sizeRef
        then
            let (_, a, b) = arr.[!idxRef]
            (a, b)
        else raise HeapNotFoundException

    /// ヒープのidx番目の値を更新したヒープを返す
    /// ヒープは破壊的に書き換わる
    let set (heap: Heap<'a, 'b>) idxRef value info =
        let (sizeRef, arr) = heap
        let (_, v, _) = arr.[!idxRef]
        do
            arr.[!idxRef] <- (idxRef, value, info)
            if value < v
            then adjustParent arr !idxRef
            else adjustChild !sizeRef arr !idxRef
        (sizeRef, arr)

    /// 最小値を持つものとそれを取り除いたヒープの組みを返す
    /// 最小の値を持つもののidxは無効な値になる
    /// ヒープが空の時は例外を投げる
    /// ヒープは破壊的に書き換わる
    let splitTop (heap: Heap<'a, 'b>) =
        let (sizeRef, arr) = heap
        if !sizeRef = 0 then raise HeapEmptyException else
        let (idxRef, value, info) = arr.[0] // 最小値は常に根に格納されている
        do
            sizeRef := !sizeRef - 1 // 要素数を1つ減らす
            arr.[0] <- arr.[!sizeRef]
            adjustChild !sizeRef arr 0
            idxRef := -1 // 取り出した要素のidxRefは無効にする
        ((value, info), (sizeRef, arr))

    /// ヒープの要素数を返す
    let length (heap: Heap<'a, 'b>) = !(fst heap)

    let rec private extractAllElements heap lst =
        try
            let ((a, _), heap) = splitTop heap
            extractAllElements heap (a :: lst)
        with HeapEmptyException ->
            lst

    /// 降順に整列したリストを返す
    let heapSort lst =
        match lst with
        | [] -> []
        | first :: rest ->
            let size = List.length lst
            let heap =
                let f heap x =
                    let (_, heap) = insert heap x ()
                    heap
                List.fold f (create size first ()) lst
            extractAllElements heap []

    let heapTest () =
        let test1 = heapSort []
        assert (List.isEmpty test1)
        let test2 = heapSort [1]
        assert (test2 = [1])
        let test3 = heapSort [1;2]
        assert (test3 = [2;1])
        let test4 = heapSort [2;1]
        assert (test4 = [2;1])
        let test5 = heapSort [5;3;8;1;7;4]
        assert (test5 = [8;7;5;4;3;1])