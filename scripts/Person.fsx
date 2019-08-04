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