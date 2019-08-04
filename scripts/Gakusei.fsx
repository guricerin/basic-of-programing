[<AutoOpen>]
module Gakusei

type Gakusei = {
    namae : string
    tensuu : int
    seiseki : string
}

let gakuseiList1 = [
    {namae = "aho"; tensuu = 100; seiseki = "A"};
    {namae = "boke"; tensuu = 87; seiseki = "B"};
    {namae = "casu"; tensuu = 44; seiseki = "F"};
    {namae = "dameppi"; tensuu = 73; seiseki = "C"};
    {namae = "erodanshaku"; tensuu = 99; seiseki = "A"}
]