[<AutoOpen>]
module Util

let solve problemName expr =
    printfn "Q.%s: %A" problemName expr

let getType x = x.GetType()

let assertEq problemName x y =
    if x = y
    then printfn "Q.%s: test Passed!" problemName
    else printfn "Q.%s: test Failed..." problemName