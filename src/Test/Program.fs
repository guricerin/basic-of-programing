// Learn more about F# at http://fsharp.org

open System
open MetroNetwork.Global
open Test

let isSuccess x =
    if x
    then printfn "All test succeed!"

[<EntryPoint>]
let main argv =
    true
    |> CoreTest.doTest
    |> isSuccess
    0 // return an integer exit code
