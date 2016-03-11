[<AutoOpen>]
module internal PgnParsers.DateTagParser

open FParsec

// Date Tag value (e.g. ????.??.??, 2013.05.18, 2013.??.??)
let pYear = ((str "????" |>> fun x -> None) <|> (pint16 |>> fun x -> Some(x)))
let pMonth = ((str "??" |>> fun x -> None) <|> (pint8 |>> fun x -> Some(x|>byte)))
let pDay = pMonth

let pDateTagValue = 
    attempt(pchar '"' >>. pYear .>> pchar '.' .>>. pMonth .>> pchar '.' .>>. pDay .>> pchar '"')
    <|> ((pchar '"' >>. pYear .>> pchar '"') |>> fun year -> ((year, None), None))
    |>> fun((year, month), day) -> PgnDateTag("Date", year, month, day) :> PgnTag
    <!> "pDateTagValue"

let applypDateTagValue p = run pDateTagValue p