[<AutoOpen>]
module internal PgnParsers.PgnTags

open System
open PgnFSharp

type PgnTag(name: string, value: string) = 
    member val Name = name with get, set
    member val Value = value with get, set

let formatDate (year : int16 option, month : byte option, day : byte option) = 
    let yearStr = 
        match year with 
        | None -> "????"
        | _ -> year.Value.ToString("D4")

    let monthStr = 
        match month with
        | None -> "??"
        | _ -> month.Value.ToString("D2");

    let dayStr = 
        match day with
        | None -> "??"
        | _ -> day.Value.ToString("D2");

    String.Format("{0}-{1}-{2}", yearStr, monthStr, dayStr)

type PgnDateTag(name: string, year: int16 option, month: byte option, day: byte option) = 
    inherit PgnTag(name, formatDate(year, month, day))
    
    member val Year = year with get, set
    member val Month = month with get, set
    member val Day = day with get, set


let formatResult(result: GameResult) = 
    match result with
    | GameResult.WhiteWin -> "1 - 0"
    | GameResult.BlackWin -> "0 - 1"
    | GameResult.Draw  -> "1/2 - 1/2"
    | GameResult.Open  -> "*"

type PgnResultTag(name: string, result: GameResult) = 
    inherit PgnTag(name, formatResult(result))
    
    member val Result: GameResult = result with get, set

type FenTag(name: string, setup: BoardSetup) =
    inherit PgnTag(name, setup.ToString())

    member val Setup: BoardSetup = setup with get, set