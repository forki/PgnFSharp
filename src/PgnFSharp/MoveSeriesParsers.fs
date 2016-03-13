[<AutoOpen>]
module internal PgnParsers.MoveSeries

open FParsec
open PgnFSharp

let pPeriods = 
    (str ".." .>> manyChars (pchar '.') >>% true) //two or more dots => Continued move pair
    <|> (str "…" >>% true) // => Continued move pair
    <|> (pchar '.' >>% false) // => Non-Continued move pair (move start)

let pMoveNumberIndicator = 
    attempt(pint32 .>> ws .>>. pPeriods |>> fun (num, contd) -> (Some(num), contd))
    <|> preturn (None, false)
    <!> "pMoveNumberIndicator"
    <?> "Move number indicator (e.g. 5. or 13...)"

let pFullMoveTextEntry =
    pMoveNumberIndicator .>> ws .>>. pMove .>> ws1 .>>. pMove 
    |>> fun (((moveNum, contd), moveWhite), moveBlack) ->  
            MovePairEntry(moveNum, moveWhite, moveBlack) |>Some
    <!!> ("pFullMoveTextEntry", 3)

let pSplitMoveTextEntry = 
    pMoveNumberIndicator .>> ws .>>. pMove
    |>> fun ((moveNum, contd), move) -> HalfMoveEntry(moveNum,contd,move)|>Some
    <!!> ("pSplitMoveTextEntry", 3)

let pCommentary = 
    between (str "{") (str "}") (skipMany (noneOf "}")) 
    <|> between (str ";") newline (skipMany (noneOf "\n")) //to end of line comment
    |>> fun _ -> None
    <!!> ("pCommentary", 3)
    <?> "Comment ( {...} or ;... )"

let pOneHalf = str "1/2" <|> str "½"
let pDraw = pOneHalf .>> ws .>> str "-" .>> ws .>> pOneHalf |>> fun _ -> GameResult.Draw
let pWhiteWin = str "1" .>> ws .>> str "-" .>> ws .>> str "0"  |>> fun _ -> GameResult.WhiteWin
let pBlackWin = str "0" .>> ws .>> str "-" .>> ws .>> str "1"  |>> fun _ -> GameResult.BlackWin
let pEndOpen = str "*"  |>> fun _ -> GameResult.Open

let pEndOfGame =
    pDraw <|> pWhiteWin <|> pBlackWin <|> pEndOpen |>> fun _ -> None
    <!!> ("pEndOfGame", 3)
    <?> "Game termination marker (1/2-1/2 or 1-0 or 0-1 or *)"

let pNAG =
    pchar '$' >>. pint32 |>> fun _ -> None
    <?> "NAG ($<num> e.g. $6 or $32)"
    <!!> ("pNAG", 3)

let (pRAV:Parser<MoveTextEntry option,unit>), pRAVImpl = createParserForwardedToRef()

pRAVImpl := 
    between (str "(") (str ")") (many ((noneOf ")("|>>fun _ -> None)<|>pRAV))
    |>> fun _ -> None
    <?> "RAV e.g. \"(6. Bd3)\""
    <!!> ("pRAV", 4)
let pMoveSeriesEntry= 
     pCommentary
    <|> pNAG
    <|> pRAV
    <|> attempt(pFullMoveTextEntry) 
    <|> attempt(pSplitMoveTextEntry)
    <|> pEndOfGame
    <!!> ("pMoveSeriesEntry", 4)

let pMoveSeries = (
    sepEndBy1 pMoveSeriesEntry ws
    <!!> ("pMoveSeries", 5)
    )
