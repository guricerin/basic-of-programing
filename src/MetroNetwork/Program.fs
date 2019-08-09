open System
open MetroNetwork.Ekimei
open MetroNetwork.RedBlackTree
open MetroNetwork.Ekikan
open MetroNetwork.Eki
open MetroNetwork.Global
open MetroNetwork.Core

[<EntryPoint>]
let main argv =
    RbTreeTest()
    printfn "%s" "All test passed!"
    0 // return an integer exit code
