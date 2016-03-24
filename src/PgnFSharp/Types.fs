namespace PgnFSharp

open System
open System.Text

[<AutoOpen>]
module Types = 
    type GameResult = 
        | Draw
        | WhiteWins
        | BlackWins
        | NotKnown
        override x.ToString() = 
            match x with
            | Draw -> "1/2-1/2"
            | WhiteWins -> "1-0"
            | BlackWins -> "0-1"
            | NotKnown -> "*"
    
    /// Move type where not a simple move
    type MvTyp = 
        | Prom of char
        | CasK
        | CasQ
        | Ep

    /// Index of square on the board
    type Sq = int

    /// Fast type for making moves on board
    type Mov = 
        { Mfrom : Sq
          Mto : Sq
          Mtyp : MvTyp option
          Mpgn : string }
        override x.ToString() = x.Mpgn
    
    /// Game - holds key headers and list of moves
    let nl = Environment.NewLine
    let FormatTag name value = "[" + name + " \"" + value + "\"]" + nl
    type Game = 
        { Event : string
          Site : string
          Year : int16 option
          Month : byte option
          Day : byte option
          Round : string
          White : string
          Black : string
          Result : GameResult
          Moves : Mov list }
        
        member x.DateStr = 
            (if x.Year.IsNone then "????"
             else x.Year.Value.ToString("0000")) + "." + (if x.Month.IsNone then "??"
                                                          else x.Month.Value.ToString("00")) + "." 
            + (if x.Day.IsNone then "??"
               else x.Day.Value.ToString("00"))
        
        override x.ToString() = 
            let sb = new StringBuilder()
            
            let rec mvs2txt ct mvl = 
                if List.isEmpty mvl then sb.ToString()
                elif mvl.Length = 1 then 
                    sb.Append(ct.ToString() + ". " + mvl.Head.ToString() + " ") |> ignore
                    sb.ToString()
                else 
                    let w = mvl.Head.ToString()
                    let b = mvl.Tail.Head.ToString()
                    let rest = mvl.Tail.Tail
                    sb.Append(ct.ToString() + ". " + w + " " + b + " ") |> ignore
                    mvs2txt (ct + 1) rest
            (x.Event |> FormatTag "Event") + (x.Site |> FormatTag "Site") + (x.DateStr |> FormatTag "Date") 
            + (x.Round |> FormatTag "Round") + (x.White |> FormatTag "White") + (x.Black |> FormatTag "Black") 
            + (x.Result.ToString() |> FormatTag "Result") + nl + (x.Moves |> mvs2txt 1) + x.Result.ToString()
        
        member x.AddHdr(t, v) = 
            match t with
            | "Event" -> { x with Event = v }
            | "Site" -> { x with Site = v }
            | "Date" -> 
                let b = v.Split('.')
                if b.Length = 3 then 
                    let tfy, y = Int16.TryParse(b.[0])
                    let tfm, m = Byte.TryParse(b.[1])
                    let tfd, d = Byte.TryParse(b.[2])
                    { x with Year = 
                                 if tfy then y |> Some
                                 else None
                             Month = 
                                 if tfm then m |> Some
                                 else None
                             Day = 
                                 if tfd then d |> Some
                                 else None }
                else x
            | "Round" -> { x with Round = v }
            | "White" -> { x with White = v }
            | "Black" -> { x with Black = v }
            | "Result" -> 
                let res = 
                    match v with
                    | "1/2-1/2" -> Draw
                    | "1-0" -> WhiteWins
                    | "0-1" -> BlackWins
                    | _ -> NotKnown
                { x with Result = res }
            | _ -> x
        
        static member Blank = 
            { Event = "?"
              Site = "?"
              Year = None
              Month = None
              Day = None
              Round = "?"
              White = "?"
              Black = "?"
              Result = NotKnown
              Moves = [] }
