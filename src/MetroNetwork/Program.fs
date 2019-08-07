open System
open MetroNetwork.Ekimei
open MetroNetwork.Ekikan
open MetroNetwork.Eki
open MetroNetwork.Global
open MetroNetwork.Core

[<EntryPoint>]
let main argv =
    let name = "秋葉原"
    let s = makeInitialEkiList globalEkimeiList name
    let s = koushin {namae="秋葉原"; saitanKyori = 0.0<km>; temaeList = ["秋葉原"]} s globalEkikanList
    s |> List.iter (fun x -> if x.saitanKyori <> inf then printfn "%A" x)

    (* 駅の例 *)
    let eki1 = {namae="池袋"; saitanKyori = inf; temaeList = []}
    let eki2 = {namae="新大塚"; saitanKyori = 1.2<km>; temaeList = ["新大塚"; "茗荷谷"]}
    let eki3 = {namae="茗荷谷"; saitanKyori = 0.<km>; temaeList = ["茗荷谷"]}
    let eki4 = {namae="後楽園"; saitanKyori = inf; temaeList = []}

    (* 駅リストの例 *)
    let lst = [eki1; eki2; eki3; eki4]

    (* テスト *)
    let test = saitanWoBunri lst = (eki3, [eki1; eki2; eki4])
    printfn "%A" test
    0 // return an integer exit code
