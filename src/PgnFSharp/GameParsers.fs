[<AutoOpen>]
module internal PgnParsers.Game

open FParsec
open PgnFSharp

let setTag (game : Game, tag : PgnTag) = 
    match tag with
    | Basic(n, v) -> 
        match n with
        | "Event" -> { game with Event = v }
        | "Site" -> { game with Site = v }
        | "Round" -> { game with Round = v }
        | "White" -> { game with WhitePlayer = v }
        | "Black" -> { game with BlackPlayer = v }
        | _ -> game
    | Date(y, m, d) -> 
        { game with Year = y
                    Month = m
                    Day = d }
    | Result(r) -> { game with Result = r }

let makeGame (tagList : PgnTag list, moveTextList : MoveTextEntry list) = 
    let rec addTags tl gm = 
        if List.isEmpty tl then gm
        else addTags tl.Tail (setTag (gm, tl.Head))
    
    let game = addTags tagList (BlankGm())
    { game with MoveText = moveTextList }

let pGame = ws >>. pTagList .>> ws .>>. pMoveSeries .>> (ws <|> eof) |>> makeGame <!!> ("pGame", 5)
let pDatabase = sepEndBy pGame ws .>> eof |>> fun games -> (games : Database)
