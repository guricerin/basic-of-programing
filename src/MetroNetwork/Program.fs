open System
open MetroNetwork.Ekimei
open MetroNetwork.Ekikan
open MetroNetwork.Metro
open MetroNetwork.Eki

[<EntryPoint>]
let main argv =
    let s = makeEkiList globalEkimeiList
    let name = "秋葉原"
    s |> List.iter (fun x -> if x.namae = name then printfn "%A" x)
    let s = shokika s name
    s |> List.iter (fun x -> if x.namae = name then printfn "%A" x)
    let s = seiretsu globalEkimeiList
    s |> List.iter (fun x -> printfn "%A" x)
    0 // return an integer exit code
