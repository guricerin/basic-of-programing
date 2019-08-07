open System
open MetroNetwork.Ekimei
open MetroNetwork.Ekikan
open MetroNetwork.Metro
open MetroNetwork.Eki

[<EntryPoint>]
let main argv =
    let name = "秋葉原"
    let s = makeInitialEkiList globalEkimeiList name
    let s = koushin {namae="秋葉原"; saitanKyori = 0.0<km>; temaeList = ["秋葉原"]} s
    s |> List.iter (fun x -> if x.saitanKyori <> inf then printfn "%A" x)
    0 // return an integer exit code
