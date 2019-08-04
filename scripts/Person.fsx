[<AutoOpen>]
module Person

type BloodType = A | B | O | AB

type Person = {
    name : string
    height : float
    weight : float
    birth : int * int
    bloodType : BloodType
}

let persons1 = [
    {name = "ikeda";height = 177.;weight = 88.;birth = (10,11);bloodType = O};
    {name = "ueki";height = 198.;weight = 110.;birth = (9,5);bloodType = AB};
    {name = "akasaki";height = 160.;weight = 60.;birth = (1,21);bloodType = A};
]