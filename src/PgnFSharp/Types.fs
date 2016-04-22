namespace PgnFSharp

open System
open System.Text

type GameResult = 
    | Draw
    | WhiteWins
    | BlackWins
    override x.ToString() = 
        match x with
        | Draw -> "1/2-1/2"
        | WhiteWins -> "1-0"
        | BlackWins -> "0-1"
    member x.ToByte() = 
        match x with
        | Draw -> byte(1)
        | WhiteWins -> byte(2)
        | BlackWins -> byte(0)
    static member FromByte b = 
        match b with
        | b when b=byte(1) -> Draw
        | b when b=byte(2) -> WhiteWins
        | b when b=byte(0) -> BlackWins
        | _ -> failwith "invalid byte for GameResult" 

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
type Game = 
    { Id : int
      Event : string
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
        let nl = Environment.NewLine
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
        (x.Event |> Game.FormatTag "Event") + (x.Site |> Game.FormatTag "Site") + (x.DateStr |> Game.FormatTag "Date") 
        + (x.Round |> Game.FormatTag "Round") + (x.White |> Game.FormatTag "White") 
        + (x.Black |> Game.FormatTag "Black") + (x.Result.ToString() |> Game.FormatTag "Result") + nl 
        + (x.Moves |> mvs2txt 1) + x.Result.ToString()
    
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
            elif b.Length = 1 then 
                let tfy, y = Int16.TryParse(b.[0])
                { x with Year = 
                             if tfy then y |> Some
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
                | _ -> failwith "invalid game result"
            { x with Result = res }
        | _ -> x
    
    static member FormatTag name value = "[" + name + " \"" + value + "\"]" + Environment.NewLine
    static member Blank = 
        { Id = -1
          Event = "?"
          Site = "?"
          Year = None
          Month = None
          Day = None
          Round = "?"
          White = "?"
          Black = "?"
          Result = Draw
          Moves = [] }

/// Board state - quick version
type Posn = 
    { Sqs : char []
      mutable IsW : bool }
    
    /// loads Posn given a FEN like string
    static member FromString(s : string) = 
        let b = s.Split(' ')
        let isw = b.[1] = "w"
        let sqs = Array.create 64 ' '
        
        let rec getp i ps = 
            if ps = "" then 
                { Sqs = sqs
                  IsW = isw }
            else 
                match ps.[0] with
                | '/' -> getp i ps.[1..]
                | c -> 
                    let ok, p = Int32.TryParse(c.ToString())
                    if ok then getp (i + p) ps.[1..]
                    else 
                        sqs.[i] <- c
                        getp (i + 1) ps.[1..]
        getp 0 b.[0]
    
    /// Gets initial Posn
    static member Start() = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w" |> Posn.FromString
    
    override x.ToString() = 
        let sb = new StringBuilder()
        
        let rec getstr i e chl = 
            if List.isEmpty chl then 
                if e > 0 then sb.Append(e.ToString()) |> ignore
                sb.ToString() + (if x.IsW then " w"
                                 else " b")
            elif i <> 8 && chl.Head = ' ' then getstr (i + 1) (e + 1) chl.Tail
            else 
                if e > 0 then sb.Append(e.ToString()) |> ignore
                if i = 8 then sb.Append("/") |> ignore
                else sb.Append(chl.Head) |> ignore
                if i = 8 then getstr 0 0 chl
                else getstr (i + 1) 0 chl.Tail
        getstr 0 0 (x.Sqs |> List.ofArray)
    
    member x.Copy() = 
        { Sqs = x.Sqs |> Array.copy
          IsW = x.IsW }
    
    
