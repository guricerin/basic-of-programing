// namespace MetroNetwork

[<AutoOpen>]
module Measure

    [<Measure>] type km

    [<Measure>] type minute

    // infinityに測定単位をそのまま付与できないっぽいので、代わりにこいつを使う
    let inf: float<km> = LanguagePrimitives.FloatWithMeasure infinity