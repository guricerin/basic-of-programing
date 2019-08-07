namespace Test

[<AutoOpen>]
module Util =

    let assertEq moduleName x y =
        if x <> y
        then
            printfn "%s: test failed..." moduleName |> ignore
            false
        else true