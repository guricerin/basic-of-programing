open System
open MetroNetwork.RedBlackTree
open MetroNetwork.Heap
open MetroNetwork.Core

[<EntryPoint>]
let main argv =
    // RbTreeTest()
    // heapTest()
    // printfn "%s" "All test passed!"
    do
        printfn ""
        printfn "--- メトロネットワーク最短路問題 ---"
        printfn ""
        printf "始点の駅名をローマ字で入力してください > "
        let start = Console.ReadLine()
        printf "終点の駅名をローマ字で入力してください > "
        let goal = Console.ReadLine()
        printfn ""
        dijkstra start goal |> printEki
        printfn ""
    0 // return an integer exit code
