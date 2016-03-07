[<AutoOpen>]
module internal ilf.pgn.PgnParsers.Game

open System
open FParsec
open pgn.Data

let setTag(game : Game, tag : PgnTag) =
    game.Tags.Add(tag.Name, tag.Value)
    match tag.Name with
    | "Event" -> {game with Event = tag.Value}
    | "Site" -> {game with Site = tag.Value}
    | "Date" -> let dt = tag :?> PgnDateTag
                {game with Year = dt.Year; Month = dt.Month; Day = dt.Day}
    | "Round" -> {game with Round = tag.Value}
    | "White" -> {game with WhitePlayer = tag.Value}
    | "Black" -> {game with BlackPlayer = tag.Value}
    | "Result" -> {game with Result = (tag :?> PgnResultTag).Result}
    | "FEN" -> {game with BoardSetup = Some((tag :?> FenTag).Setup)}
    | _ -> let addi = {Name=tag.Name;Value=tag.Value}::game.AdditionalInfo
           {game with AdditionalInfo=addi}

let makeGame (tagList : PgnTag list, moveTextList : MoveTextEntry list) =
    let rec addTags tl gm =
        if List.isEmpty tl then gm
        else addTags tl.Tail (setTag(gm,tl.Head))
    let game = addTags tagList (BlankGm())
    {game with MoveText=moveTextList;AdditionalInfo=game.AdditionalInfo|>List.rev}

let pGame = 
    ws >>. pTagList .>> ws .>>.  pMoveSeries .>> (ws <|> eof)
    |>>  makeGame
    <!!> ("pGame", 5)


let pDatabase = 
    sepEndBy pGame ws .>> eof
    |>> fun games -> (games:Database)

