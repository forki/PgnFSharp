[<AutoOpen>]
module internal PgnParsers.Tag

open FParsec
open PgnFSharp

type PgnTag = 
    | Basic of string * string
    | Date of int16 option * byte option * byte option
    | Result of GameResult

let pTagName = identifier (IdentifierOptions()) .>> ws1 <!> "pTagName"
// Date Tag value (e.g. ????.??.??, 2013.05.18, 2013.??.??)
let pYear = ((str "????" |>> fun x -> None) <|> (pint16 |>> fun x -> Some(x)))
let pMonth = ((str "??" |>> fun x -> None) <|> (pint8 |>> fun x -> Some(x |> byte)))
let pDay = pMonth
let pDateTagValue = 
    attempt (pchar '"' >>. pYear .>> pchar '.' .>>. pMonth .>> pchar '.' .>>. pDay .>> pchar '"') 
    <|> ((pchar '"' >>. pYear .>> pchar '"') |>> fun year -> ((year, None), None)) 
    |>> fun ((year, month), day) -> Date(year, month, day) 
    <!> "pDateTagValue"
let pResultTagVaue = 
    pchar '"' 
    >>. (((str "1-0" <|> str "1 - 0") |>> fun _ -> GameResult.WhiteWin) 
         <|> ((str "0-1" <|> str "0 - 1") |>> fun _ -> GameResult.BlackWin) 
         <|> ((str "1/2-1/2" <|> str "1/2 - 1/2" <|> str "½-½" <|> str "½ - ½") |>> fun _ -> GameResult.Draw) 
         <|> ((str "*" <|> str "?") |>> fun _ -> GameResult.Open)) .>> pchar '"' |>> fun result -> Result(result) 
    <!> "pResultTagVaue"
// Basic tag (e.g. [Site "Boston"]
let pBasicTagValue = between (pchar '"') (pchar '"') (pNotChar '"') <!> "pBasicTagValue"
let pBasicTag = pTagName .>> spaces .>>. pBasicTagValue |>> Basic <!> "pBasicTag"
let tagContent = 
    (str "Date" .>> ws >>. pDateTagValue) <|> (str "Result" .>> ws >>. pResultTagVaue) <|> pBasicTag <!> "pTagContent"
let pTag = 
    ws .>> pchar '[' .>> ws >>. tagContent .>> ws .>> pchar ']' .>> ws <!!> ("pTag", 1) 
    <?> "Tag (e.g [Date \"2013.10.02\"])"
let pTagList = ws >>. sepEndBy pTag ws <!!> ("pTagList", 2)
let applyPTag p = run pTag p
